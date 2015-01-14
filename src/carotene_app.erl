-module(carotene_app).

-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", ws_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 1, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]),
    io:format("~s~n", ["Server started"]),
    websocket_sup:start_link(),

    carotene_sup:start_link().
   
stop(_State) ->
    ok.
