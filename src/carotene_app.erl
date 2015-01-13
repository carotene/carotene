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

    carotene_sup:start_link(),
    {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'queue.declare'{queue = <<"hello">>}),
    io:format(" [*] Waiting for messages. To exit press CTRL+C~n"),

    amqp_channel:subscribe(Channel, #'basic.consume'{queue = <<"hello">>,
                                                     no_ack = true}, self()),
    receive
        #'basic.consume_ok'{} -> ok
    end,
    loop(Channel).
   
loop(Channel) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
            io:format(" [x] Received ~p~n", [Body]),
            loop(Channel)
    end.

stop(_State) ->
    ok.
