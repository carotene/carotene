-module(carotene_connection).

-behaviour(gen_server).

-define(CONNTIMEOUT, 100000).

-export([start/3, start_link/3]).
-export([stop/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-record(state, {
          user_id,
          user_data,
          publishers,
          subscribers,
          transport,
          transport_state,
          buffer,
          connId,
          timer
         }).

start_link(From, Type, ConnId) ->
    Opts = [],
    gen_server:start_link(?MODULE, [From, Type, ConnId], Opts).

start(From, Type, ConnId) ->
    Opts = [],
    gen_server:start(?MODULE, [From, Type, ConnId], Opts).

init([From, intermittent, ConnId]) ->
    % Long polling connection, set timeout
    {ok, #state{publishers = dict:new(), subscribers = dict:new(), transport = From, user_id = anonymous, transport_state = temporary, connId = ConnId, buffer = [], timer = erlang:start_timer(?CONNTIMEOUT, self(), trigger)}};

init([From, permanent, undefined]) ->
    % permanent connection, alert when dies
    erlang:monitor(process, From),
    {ok, #state{publishers = dict:new(), subscribers = dict:new(), transport = From, user_id = anonymous, transport_state = permanent, connId = undefined, buffer = [], timer = undefined}}.


handle_cast({process_message, Message}, State = #state{timer = Timer}) ->
    Timer2 = reset_timer(Timer),
    StateNew = case jsonx:decode(Message, [{format, proplist}]) of
                   {error, invalid_json, _} -> 
                       self() ! {just_send, <<"Non JSON message received">>},
                       State;
                   Msg -> 
                       process_message(lists:keysort(1, Msg), State)
               end,
    {noreply, StateNew#state{timer = Timer2}};
handle_cast({keepalive, From}, State = #state{timer = Timer, buffer = Buffer}) ->
    Timer2 = reset_timer(Timer),
    case Buffer of
        [] -> ok;
        Msgs -> flush_buffer(From, Msgs)
    end,

    {noreply, State#state{transport = From, timer = Timer2, transport_state = temporary, buffer = []}};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_info(transport_hiatus, State) ->
    % When long polling is (briefly) interrupted, we stop sending messages
    {noreply, State#state{transport_state = hiatus}};
handle_info({'DOWN', _Ref, process, _Pid, _}, State) ->
    {stop, shutdown, State};

handle_info(timeout, State) ->
    {stop, shutdown, State};
handle_info({timeout, _TRef, _Msg}, State) ->
    {stop, shutdown, State};
handle_info({presence_response, Msg}, State = #state{transport = Transport, buffer = Buffer, transport_state = TState}) ->
    NewBuffer = send_transport(Transport, Msg, Buffer, TState),
    {noreply, State#state{buffer = NewBuffer}};

handle_info({received_message, Msg}, State = #state{transport = Transport, buffer = Buffer, transport_state = TState}) ->
    NewBuffer = send_transport(Transport, Msg, Buffer, TState),
    {noreply,State#state{buffer = NewBuffer}};

handle_info({just_send, Msg}, State = #state{transport = Transport, buffer = Buffer, transport_state = TState}) ->
    NewBuffer = send_transport(Transport, jsonx:encode([{<<"type">>, <<"info">>},
                                                   {<<"payload">>, Msg}]), Buffer, TState),
    {noreply, State#state{buffer = NewBuffer}};

handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

terminate(_Reason, #state{connId = undefined}) ->
    ok;
terminate(_Reason, #state{connId = Token}) ->
    % long polling
    ets:delete(carotene_connections, Token),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

process_message([{<<"subscribe">>, Channel}], State = #state{subscribers=Subs, user_id=UserId}) ->
    NewSubs = case dict:find(Channel, Subs) of
                  error -> 
                      {ok, SubscriberPid} = carotene_subscriber_sup:start_child([Channel, UserId, self()]),
                      case carotene_subscriber:subscribe(SubscriberPid) of
                          ok -> dict:store(Channel, SubscriberPid, Subs);
                          {error, Error} -> self() ! {just_send, Error},
                                            Subs
                      end;
                  {ok, _} -> Subs
              end,
    State#state{subscribers = NewSubs};

process_message([{<<"channel">>, Channel}, {<<"publish">>, Message}], State = #state{publishers=Pubs, user_id=UserId, user_data=UserData}) ->
    CompleteMessage = jsonx:encode([
                                  {<<"type">>, <<"message">>},
                                  {<<"message">>, Message},
                                  {<<"channel">>, Channel},
                                  {<<"user_id">>, UserId},
                                  {<<"user_data">>, UserData}
                                 ]),
    NewPubs = case dict:find(Channel, Pubs) of
                  {ok, PublisherPid} -> 
                      publish(PublisherPid, CompleteMessage),
                      Pubs;
                  error ->
                      {ok, PublisherPid} = carotene_publisher_sup:start_child([Channel, UserId, self()]),
                      publish(PublisherPid, CompleteMessage),
                      dict:store(Channel, PublisherPid, Pubs)

              end,
    State#state{publishers = NewPubs};

process_message([{<<"authenticate">>, AssumedUserId}, {<<"token">>, Token}], State = #state{user_id = anonymous} ) ->
    case application:get_env(carotene, authenticate_url) of
        undefined -> self() ! {just_send, <<"Authentication error: no authentication_url in config">>},
                     State; 
        {ok, AuthenticateUrl} -> try_authenticate(AuthenticateUrl, AssumedUserId, Token, State)
    end;
process_message([{<<"authenticate">>, _AssumedUserId}, {<<"token">>, _Token}], State = #state{user_id = _UserId} ) ->
    self() ! {just_send, <<"Authentication error: already authenticated">>},
    State;

process_message([{<<"presence">>, Channel}], State = #state{subscribers=Subs}) ->
    case application:get_env(carotene, presence) of
        {ok, true} -> 
            case dict:find(Channel, Subs) of
                error -> self() ! {just_send, <<"Cannot ask for presence when not subscribed to the channel">>};
                {ok, _} ->
                    UsersSub = carotene_presence:presence(Channel),
                    self() ! {presence_response, jsonx:encode([
                                                             {<<"type">>, <<"presence">>},
                                                             {<<"subscribers">>, UsersSub},
                                                             {<<"channel">>, Channel}
                                                            ])}
            end;
        _ -> self() ! {just_send, <<"Presence is disabled">>}
    end,
    State;

process_message(_Msg, State) ->
    self() ! {just_send, <<"Unknown message received">>},
    State .

try_authenticate(AuthenticateUrl, AssumedUserId, Token, State = #state{subscribers = Subs, publishers = Pubs}) ->
    case httpc:request(post, {AuthenticateUrl, [], "application/x-www-form-urlencoded", "user_id="++binary_to_list(AssumedUserId)++"&token="++binary_to_list(Token)}, [], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} -> 
            {UserId, UserData} = try lists:keysort(1, jsonx:decode(binary:list_to_bin(Body), [{format, proplist}])) of 
                                     [{<<"authenticated">>, true}, {<<"user_data">>, ResUserData}] ->
                                         self() ! {just_send, <<"Authenticated">>},
                                         update_users(AssumedUserId, Subs, Pubs),
                                         {AssumedUserId, ResUserData};
                                     _ -> self() ! {just_send, <<"Authentication failed">>},
                                          {undefined, undefined}
                                 catch _:_ ->
                                           self() ! {just_send, <<"Authentication error: Bad JSON response from server">>},
                                           {undefined, undefined}
                                 end,
            State#state{user_id = UserId, user_data = UserData};
        _ -> self() ! {just_send, <<"Authentication error: Bad response from server">>},
             State
    end.

update_users(UserId, Subs, Pubs) ->
    dict:map(fun(_Channel, Sub) -> 
                     carotene_subscriber:update_user(Sub, UserId) end, Subs),
    dict:map(fun(_Channel, Pub) -> 
                     carotene_publisher:update_user(Pub, UserId) end, Pubs).

publish(PublisherPid, CompleteMessage) ->
    case carotene_publisher:publish(PublisherPid, CompleteMessage) of
        ok -> ok;
        {error, Error} -> self() ! {just_send, Error}
    end.

reset_timer(Timer) ->
    case Timer of
        undefined -> 
            undefined;
        TimerRef -> case catch erlang:cancel_timer(TimerRef) of
                        false ->
                            erlang:start_timer(?CONNTIMEOUT, self(), trigger);
                        _ ->
                            erlang:start_timer(?CONNTIMEOUT, self(), trigger)
                    end
    end.

send_transport(Transport, Msg, [], permanent) ->
    % always open, just send
    Transport ! {text, Msg},
    [];
send_transport(Transport, Msg, Buffer, temporary) ->
    % open for one response, flush buffer
    flush_buffer(Transport, [Msg|Buffer]),
    [];
send_transport(_Transport, Msg, Buffer, hiatus) ->
    % connection inactve, store
    [Msg|Buffer].

flush_buffer(Transport, Msgs) ->
    Transport ! {list, Msgs}.

