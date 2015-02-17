-module(ws_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

-record(state, {
          user_id,
          user_data,
          exchanges = [],
          queues = []
}).

init(Req, _Opts) ->
    {cowboy_websocket, Req, #state{exchanges=dict:new(), queues=dict:new()}}.

websocket_handle({text, Data}, Req, State) ->
    % TODO: check it is json
    Msg = jsx:decode(Data),
    StateNew = process_message(Msg, State),
    {ok, Req, StateNew};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({presence_response, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info({received_message, Msg, exchange, _ExchangeName}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info({just_send, Msg}, Req, State) ->
    {reply, {text, jsx:encode([{<<"info">>, Msg}])}, Req, State};
websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

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
