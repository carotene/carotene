-module(publisher_tests).

-include_lib("eunit/include/eunit.hrl").

start_test_() ->
    {"It should be possible to start a publisher",
              [start_and_test_running()]
      }.

publish_auth_config_test_() ->
    {"If there is no authorization info in config we can always publish",
     {setup, 
        fun start_as_anonymous_with_no_config/0,
        fun stop/1,
        fun(Publisher) ->
                [try_send_success(Publisher)]
        end
     }}.

publish_asks_for_auth_test_() ->
    {"If there is authorization config will ask for authorization",
     {setup, 
        fun start_as_anonymous_with_config_level_anonymous/0,
        fun stop/1,
        fun(Publisher) ->
                [try_send_asks_auth(Publisher)]
        end
     }}.

%% Helpers
start(UserId) ->
    Res = publisher:start(<<"room1">>, UserId, self()),
    {ok, Pid} = Res,
    Pid.

%% Setups/Teardowns
start_as_anonymous_with_no_config() ->
    application:unset_env(carotene, publish_authorization),
    start(anonymous).

start_as_anonymous_with_config_level_anonymous() ->
    application:set_env(carotene, publish_authorization, [{level, anonymous}]),
    start(<<"user1">>).

stop(Publisher) ->
    publisher:stop(Publisher).

%% Tests
start_and_test_running() ->
    application:set_env(carotene, publish_authorization, nothing),
    Res = publisher:start(<<"room1">>, <<"user1">>, self()),
    ?_assertMatch({ok, _}, Res).

try_send_success(Publisher) ->
    Res = gen_server:call(Publisher, {publish, <<"hi">>}),
    ?_assertEqual(ok, Res).

try_send_asks_auth(Publisher) ->
    meck:new(carotene_authorization),
    meck:expect(carotene_authorization, check_authorization, fun(_, _, _) -> true end),
    Res = gen_server:call(Publisher, {publish, <<"hi">>}),
    ?assertEqual(true, meck:validate(carotene_authorization)),
    meck:unload(carotene_authorization),
    ?_assertEqual(ok, Res).
