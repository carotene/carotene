-module(connection).

-behaviour(gen_server).

-export([start/1, start_link/1]).
-export([stop/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-record(state, {
          user_id,
          user_data,
          exchanges,
          queues,
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
    {ok, #state{exchanges=dict:new(), queues=dict:new(), transport=From}}.

handle_cast({process_message, Message}, State) ->
    StateNew = try jsx:decode(Message) of
                   Msg -> process_message(Msg, State)
               catch _:_ ->
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

handle_info({received_message, Msg, exchange, _ExchangeName}, State = #state{transport = Transport}) ->
    Transport ! {text, Msg},
    {noreply, State};

handle_info({just_send, Msg}, State = #state{transport = Transport}) ->
    Transport ! {text, jsx:encode([{<<"info">>, Msg}])},
    {noreply, State};

handle_info({timeout, _Ref, Msg}, State = #state{transport = Transport}) ->
    Transport ! {text, Msg},
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

process_message([{<<"joinexchange">>, ExchangeName}], State = #state{exchanges=Exs, queues=Qs, user_id=UserId}) ->
    {ok, ExchangePid} = supervisor:start_child(msg_exchange_sup, [ExchangeName, UserId, self()]),
    {ok, QueuePid} = supervisor:start_child(msg_queue_sup, [ExchangeName, UserId, self()]),
    NewExs = case dict:find(ExchangeName, Exs) of
        error -> dict:store(ExchangeName, ExchangePid, Exs);
        {ok, _} -> Exs
    end,
    NewQs = case dict:find(ExchangeName, Qs) of
        error -> dict:store(ExchangeName, QueuePid, Qs);
        {ok, _} -> Qs
    end,
    State#state{exchanges = NewExs, queues = NewQs};

process_message([{<<"send">>, Message}, {<<"exchange">>, ExchangeName}], State = #state{exchanges=Exs, user_id=UserId, user_data=UserData}) ->
    % TODO: make robust
    {ok, ExchangePid} = dict:find(ExchangeName, Exs),
    gen_server:call(ExchangePid, {publish, jsx:encode([{<<"message">>, Message},
                                                    {<<"exchange">>, ExchangeName}, 
                                                    {<<"user_id">>, UserId},
                                                    {<<"user_data">>, UserData}
                                                   ])}),
    State;

process_message([{<<"presence">>, ExchangeName}], State = #state{exchanges=Exs}) ->
    case dict:find(ExchangeName, Exs) of
        error -> ok;
        {ok, _} ->
            {UsersPub, UsersSub} = presence_serv:presence(ExchangeName),
            self() ! {presence_response, jsx:encode([{<<"publishers">>, UsersPub},
                                                     {<<"subscribers">>, UsersSub},
                                                     {<<"exchange">>, ExchangeName}
                                                    ])}
    end,
    State;

process_message([{<<"authenticate">>, AssumedUserId},{<<"token">>, Token}], State ) ->
    {ok, AuthenticateUrl} = application:get_env(carotene, authenticate_url),
    case httpc:request(post, {AuthenticateUrl, [], "application/x-www-form-urlencoded", "user_id="++binary_to_list(AssumedUserId)++"&token="++binary_to_list(Token)}, [], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} -> 
            AuthResult = jsx:decode(binary:list_to_bin(Body)),
            {UserID, UserData} = case AuthResult of
                                     % TODO: Can return in different order
                                     [{<<"authenticated">>, true}, {<<"user_data">>, ResUserData}] ->
                                         self() ! {just_send, <<"Authenticated">>},
                                         {AssumedUserId, ResUserData};
                                     _ -> self() ! {just_send, <<"Authentication failed">>},
                                          {undefined, undefined}
                                 end,
            State#state{user_id = UserID, user_data = UserData};
        _ -> self() ! {just_send, <<"Authentication error: Bad response from server">>},
            State
    end;

process_message(_, State) ->
    io:format("unknown message~n"),
    State .
