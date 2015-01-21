-module(exchange_tests).

-include_lib("eunit/include/eunit.hrl").

start_test_() ->
    {"It should be possible to start an exchange",
              [start_and_test_running()]
      }.

start_and_test_running() ->
    Res = msg_exchange_serv:start(<<"room1">>),
    ?debugFmt("my result~p~n", [Res]),
    ?_assertMatch({ok, _}, Res).
