-module(carotene_admin_connection_tests).

-include_lib("eunit/include/eunit.hrl").

start_test_() ->
    {"It should be possible to start a admin connection",
     [start_and_test_running()]
    }.

%% Tests
start_and_test_running() ->
    Res = carotene_admin_connection:start(),
    gen_server:call(carotene_admin_connection, stop),
    ?_assertMatch({ok, _}, Res).


