-module(carotene_subscriber_tests).

-include_lib("eunit/include/eunit.hrl").

start_test_() ->
    {"It should be possible to start a subscriber",
              [start_and_test_running()]
      }.

subscribe_auth_config_test_() ->
    {"If there is no authorization info in config we can always subscribe",
     {setup, 
        fun start_as_anonymous_with_no_config/0,
        fun stop/1,
        fun(Subscriber) ->
                [try_subscribe_success(Subscriber)]
        end
     }}.

%subscribe_asks_for_auth_test_() ->
%    {"If there is authorization config will ask for authorization",
%     {setup, 
%        fun start_as_anonymous_with_config_level_anonymous/0,
%        fun stop/1,
%        fun(Subscriber) ->
%                [try_subscribe_asks_auth(Subscriber)]
%        end
%     }}.
%
%% Helpers
start(UserId) ->
    Res = carotene_subscriber:start(<<"room1">>, UserId, self()),
    {ok, Pid} = Res,
    Pid.

%% Setups/Teardowns
start_as_anonymous_with_no_config() ->
    application:unset_env(carotene, subscribe_authorization),
    start(anonymous).

start_as_anonymous_with_config_level_anonymous() ->
    application:set_env(carotene, subscribe_authorization, [{level, anonymous}]),
    start(<<"user1">>).

stop(Subscriber) ->
    carotene_subscriber:stop(Subscriber).

%% Tests
start_and_test_running() ->
    application:set_env(carotene, subscribe_authorization, nothing),
    Res = carotene_subscriber:start(<<"room1">>, <<"user1">>, self()),
    ?_assertMatch({ok, _}, Res).

try_subscribe_success(Subscriber) ->
    Res = gen_server:call(Subscriber, subscribe),
    ?_assertEqual(ok, Res).

try_subscribe_asks_auth(Subscriber) ->
    meck:new(carotene_authorization),
    meck:expect(carotene_authorization, check_authorization, fun(_, _, _) -> true end),
    Res = gen_server:call(Subscriber, subscribe),
    ?assertEqual(true, meck:validate(carotene_authorization)),
    meck:unload(carotene_authorization),
    ?_assertEqual(ok, Res).
