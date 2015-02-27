-module(carotene_admin_connection_tests).

-include_lib("eunit/include/eunit.hrl").

start_test_() ->
    {"It should be possible to start a admin connection",
     [start_and_test_running()]
    }.

publish_test_() ->
    {"Admin connection can publish",
     {setup, 
        fun start/0,
        fun stop/1,
        fun(Connection) ->
                [try_publish(Connection)]
        end
     }}.

subscribe_test_() ->
    {"Admin connection can subcribe",
     {setup, 
        fun start/0,
        fun stop/1,
        fun(Connection) ->
                [try_subscribe(Connection)]
        end
     }}.

die_after_subscribe_test_() ->
    {"Admin connection dies and then unsibscribes",
     {setup, 
        fun start/0,
        fun(_) -> ok end,
        fun(Connection) ->
                [try_subscribe_and_die(Connection)]
        end
     }}.

%% Setups/Teardowns
start() ->
    Res = carotene_admin_connection:start(),
    {ok, Pid} = Res,
    Pid.

stop(Publisher) ->
    publisher:stop(Publisher).

%% Tests
start_and_test_running() ->
    Res = carotene_admin_connection:start(),
    gen_server:call(carotene_admin_connection, stop),
    ?_assertMatch({ok, _}, Res).

try_publish(Connection) ->
    meck:new(router),
    meck:expect(router, publish, fun(_, _) -> ok end),
    Res = gen_server:call(Connection, {publish, {channel, <<"room1">>}, {message, <<"hi there">>}}),
    meck:validate(router),
    meck:unload(router),
    ?_assertEqual(ok, Res).

try_subscribe(Connection) ->
    meck:new(router),
    meck:expect(router, subscribe, fun(<<"room1">>, _Connection, server) -> ok end),
    Res = gen_server:call(Connection, {subscribe, {channel, <<"room1">>}}),
    meck:validate(router),
    meck:unload(router),
    ?_assertEqual(ok, Res).

try_subscribe_and_die(Connection) ->
    meck:new(router),
    meck:expect(router, subscribe, fun(<<"room1">>, _Connection, server) -> ok end),
    meck:expect(router, unsubscribe_channels, fun([<<"room1">>], _Connection, server) -> 
                                                      ok end),
    Res = gen_server:call(Connection, {subscribe, {channel, <<"room1">>}}),

    gen_server:call(Connection, stop),
    meck:validate(router),
    meck:unload(router),
    ?_assertEqual(ok, Res).
