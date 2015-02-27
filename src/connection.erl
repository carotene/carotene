-module(connection).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([start/1, start_link/1]).
-export([stop/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-record(state, {
          user_id,
          user_data,
          publishers,
          subscribers,
          transport
         }).

start_link(From) ->
    Opts = [],
    gen_server:start_link(?MODULE, [From], Opts).

start(From) ->
    Opts = [],
    gen_server:start(?MODULE, [From], Opts).

init([From]) ->
    erlang:monitor(process, From),
    {ok, #state{publishers = dict:new(), subscribers = dict:new(), transport = From, user_id = anonymous}}.

handle_cast({process_message, Message}, State) ->
    StateNew = try jsx:decode(Message) of
                   Msg -> 
                       process_message(lists:keysort(1, Msg), State)
               catch _:_ ->
                         self() ! {just_send, <<"Non JSON message received">>},
                         State
               end,
    {noreply, StateNew};

handle_cast(_Message, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_info({'DOWN', _Ref, process, _Pid, _}, State) ->
    {stop, shutdown, State};

handle_info({presence_response, Msg}, State = #state{transport = Transport}) ->
    Transport ! {text, Msg},
    {noreply, State};

handle_info({received_message, Msg}, State = #state{transport = Transport}) ->
    Transport ! {text, Msg},
    {noreply, State};

handle_info({just_send, Msg}, State = #state{transport = Transport}) ->
    Transport ! {text, jsx:encode([{<<"info">>, Msg}])},
    {noreply, State};

handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

process_message([{<<"subscribe">>, Channel}], State = #state{subscribers=Subs, user_id=UserId}) ->
    NewSubs = case dict:find(Channel, Subs) of
                  error -> 
                      {ok, SubscriberPid} = subscriber_sup:start_child([Channel, UserId, self()]),
                      case subscriber:subscribe(SubscriberPid) of
                          ok -> dict:store(Channel, SubscriberPid, Subs);
                          {error, Error} -> self() ! {just_send, Error},
                                            Subs
                      end;
                  {ok, _} -> Subs
              end,
    State#state{subscribers = NewSubs};

process_message([{<<"channel">>, Channel}, {<<"publish">>, Message}], State = #state{publishers=Pubs, user_id=UserId, user_data=UserData}) ->
    CompleteMessage = {publish, jsx:encode([{<<"message">>, Message},
                                            {<<"channel">>, Channel},
                                            {<<"user_id">>, UserId},
                                            {<<"user_data">>, UserData}
                                           ])},
    NewPubs = case dict:find(Channel, Pubs) of
                  {ok, PublisherPid} -> 
                      gen_server:call(PublisherPid, CompleteMessage),
                      Pubs;
                  error -> 
                      {ok, PublisherPid} = publisher_sup:start_child([Channel, UserId, self()]),
                      publisher:publish(PublisherPid, CompleteMessage),
                      dict:store(Channel, PublisherPid, Pubs)

              end,
    State#state{publishers = NewPubs};

process_message([{<<"authenticate">>, AssumedUserId}, {<<"token">>, Token}], State ) ->
    case application:get_env(carotene, authenticate_url) of
        undefined -> self() ! {just_send, <<"Authentication error: no authentication_url in config">>},
                     State; 
        {ok, AuthenticateUrl} -> try_authenticate(AuthenticateUrl, AssumedUserId, Token, State)
    end;

process_message([{<<"presence">>, Channel}], State = #state{publishers=Pubs}) ->
    case dict:find(Channel, Pubs) of
        error -> ok;
        {ok, _} ->
            {UsersPub, UsersSub} = presence_serv:presence(Channel),
            self() ! {presence_response, jsx:encode([{<<"publishers">>, UsersPub},
                                                     {<<"subscribers">>, UsersSub},
                                                     {<<"publisher">>, Channel}
                                                    ])}
    end,
    State;

process_message(_Msg, State) ->
    self() ! {just_send, <<"Unknown message received">>},
    State .

try_authenticate(AuthenticateUrl, AssumedUserId, Token, State) ->
    case httpc:request(post, {AuthenticateUrl, [], "application/x-www-form-urlencoded", "user_id="++binary_to_list(AssumedUserId)++"&token="++binary_to_list(Token)}, [], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} -> 
            {UserId, UserData} = try lists:keysort(1, jsx:decode(binary:list_to_bin(Body))) of 
                                     [{<<"authenticated">>, true}, {<<"user_data">>, ResUserData}] ->
                                         self() ! {just_send, <<"Authenticated">>},
                                         %update info on all channels
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
