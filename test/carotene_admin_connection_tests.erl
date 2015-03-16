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


%% Setups/Teardowns
start() ->
    Res = carotene_admin_connection:start(),
    {ok, Pid} = Res,
    Pid.

stop(Connection) ->
    carotene_admin_connection:stop(Connection).

%% Tests
start_and_test_running() ->
    Res = carotene_admin_connection:start(),
    gen_server:call(carotene_admin_connection, stop),
    ?_assertMatch({ok, _}, Res).

try_publish(Connection) ->
    meck:new(carotene_router),
    meck:expect(carotene_router, publish, fun(_, _) -> ok end),
    Res = gen_server:call(Connection, {publish, {channel, <<"room1">>}, {message, <<"\"hi there\"">>}}),
    ?assertEqual(true, meck:validate(carotene_router)),
    meck:unload(carotene_router),
    ?_assertEqual(ok, Res).

