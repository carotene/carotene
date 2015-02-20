-module(ws_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, _Opts) ->
    {ok, ConnectionPid} = supervisor:start_child(connection_sup, [self()]),
    {cowboy_websocket, Req, ConnectionPid}.

websocket_handle({text, Data}, Req, Connection) ->
    gen_server:cast(Connection, {process_message, Data}),
    {ok, Req, Connection};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({text, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(Info, Req, State) ->
    {ok, Req, State}.

