-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
          user_id,
          username,
          exchanges = [],
          queues = []
}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, #state{exchanges=dict:new(), queues=dict:new()}}.

websocket_handle({text, Data}, Req, State) ->
    io:format("~p~n", [Data]),
    Msg = jsx:decode(Data),
    io:format("~p~n", [Msg]),
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

process_message([{<<"joinexchange">>, Exchange}], State = #state{exchanges=Exs, queues=Qs}) ->
    {ok, ExchangePid} = msg_exchange_serv:start(Exchange),
    {ok, QueuePid} = msg_queue_serv:start(Exchange, self()),
    % TODO: add only once
    State#state{exchanges = dict:append(Exchange, ExchangePid, Exs), queues = dict:append(Exchange, QueuePid, Qs)};
process_message([{<<"send">>, Message}, {<<"exchange">>, Exchange}], State = #state{exchanges=Exs}) ->
    % TODO: make robust
    {ok, [ExchangePid]} = dict:find(Exchange, Exs),
    io:format(" Pid~p ~n", [ExchangePid]),
    gen_server:call(ExchangePid, {send, jsx:encode([{<<"message">>, Message},{<<"exchange">>, Exchange}])}),
    State;

process_message(_, State) ->
    io:format("unknown message~n"),
    State .
