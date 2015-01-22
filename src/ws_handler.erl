-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
          user_id,
          user_data,
          exchanges = [],
          queues = [],
          authenticate_url
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, [AuthenticateUrl]) ->
    {ok, Req, #state{exchanges=dict:new(), queues=dict:new(), authenticate_url=AuthenticateUrl}}.

websocket_handle({text, Data}, Req, State) ->
    Msg = jsx:decode(Data),
    StateNew = process_message(Msg, State),
    {ok, Req, StateNew};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({received_message, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

process_message([{<<"joinexchange">>, ExchangeName}], State = #state{exchanges=Exs, queues=Qs, user_id=UserId}) ->
    {ok, ExchangePid} = supervisor:start_child(msg_exchange_sup, [ExchangeName, UserId]),
    {ok, QueuePid} = supervisor:start_child(msg_queue_sup, [ExchangeName, UserId, self()]),
    % TODO: add only once
    State#state{exchanges = dict:append(ExchangeName, ExchangePid, Exs), queues = dict:append(ExchangeName, QueuePid, Qs)};

process_message([{<<"send">>, Message}, {<<"exchange">>, ExchangeName}], State = #state{exchanges=Exs, user_id=UserId, user_data=UserData}) ->
    % TODO: make robust
    {ok, [ExchangePid]} = dict:find(ExchangeName, Exs),
    gen_server:call(ExchangePid, {send, jsx:encode([{<<"message">>, Message},
                                                    {<<"exchange">>, ExchangeName}, 
                                                    {<<"user_id">>, UserId},
                                                    {<<"user_data">>, UserData}
                                                   ])}),
    State;

process_message([{<<"authenticate">>, AssumedUserId},{<<"token">>, Token}], State = #state{authenticate_url=AuthenticateUrl}) ->
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {AuthenticateUrl, [], "application/x-www-form-urlencoded", "user_id="++binary_to_list(AssumedUserId)++"&token="++binary_to_list(Token)}, [], []),
    AuthResult = jsx:decode(binary:list_to_bin(Body)),
    {UserID, UserData} = case AuthResult of
                             [{<<"authenticated">>, <<"true">>}, {<<"userdata">>, ResUserData}] -> {AssumedUserId, ResUserData};
                             _ -> {undefined, undefined}
                         end,
    State#state{user_id = UserID, user_data = UserData};

process_message(_, State) ->
    io:format("unknown message~n"),
    State .
