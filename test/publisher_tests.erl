-module(publisher_tests).

-include_lib("eunit/include/eunit.hrl").

start_test_() ->
    {"It should be possible to start a publisher",
              [start_and_test_running()]
      }.

publish_auth_config() ->
    {"If there is no authorization info in config we can always publish",
     {setup, 
        fun start_as_anonymous_with_no_config/0,
        fun stop/1,
        fun(Publisher) ->
                [try_send_success(Publisher)]
        end
     }}.

publish_asks_for_auth() ->
    {"If there is authorization config will ask for authorization",
     {setup, 
        fun start_as_anonymous_with_config_level_anonymous/0,
        fun stop/1,
        fun(Publisher) ->
                [try_send_asks_auth(Publisher)]
        end
     }}.

%publish_anonymous_authlevel_test_() ->
%    {"Can't publish if I am anonymous and level is authenticated",
%     {setup, 
%        fun start_as_anonymous_with_config_level_authenticated/0,
%        fun stop/1,
%        fun(Publisher) ->
%                [try_send_and_needs_authentication(Publisher)]
%        end
%     }}.

%publish_anonymous_asklevel_test_() ->
%    {"Can't publish if I am anonymous and asking is false",
%     {setup, 
%        fun start_as_identified_with_config_level_ask/0,
%        fun stop/1,
%        fun(Publisher) ->
%                [try_send_and_auth_wrong(Publisher)]
%        end
%     }}.

%% Helpers
set_config_level_anonymous() ->
    application:set_env(carotene, publish_authorization, [{level, anonymous}
                                                         ]).
set_config_level_authenticated() ->
    application:set_env(carotene, publish_authorization, [ {level, authenticated}
                                                         ]).

set_config_level_ask() ->
    application:set_env(carotene, publish_authorization, [{authorization_url, "http://example.com"},
                                                 {level, ask}
                                                ]).

start(UserId) ->
    Res = publisher:start(<<"room1">>, UserId, self()),
    {ok, Pid} = Res,
    Pid.

%% Setups/Teardowns
start_as_anonymous_with_no_config() ->
    start(anonymous).

start_as_anonymous_with_config_level_anonymous() ->
    set_config_level_anonymous(),
    start(<<"user1">>).

start_as_anonymous_with_config_level_authenticated() ->
    set_config_level_authenticated(),
    start(anonymous).

start_as_identified_with_config_level_ask() ->
    set_config_level_ask(),
    start(<<"user1">>).

stop(Publisher) ->
    publisher:stop(Publisher).

%% Tests
start_and_test_running() ->
    application:set_env(carotene, publish_authorization, nothing),
    Res = publisher:start(<<"room1">>, <<"user1">>, self()),
    ?_assertMatch({ok, _}, Res).

try_send_and_needs_authentication(Publisher) ->
    Res = gen_server:call(Publisher, {publish, <<"hi">>}),
    ?_assertEqual({error, needs_authentication}, Res).

try_send_and_authorization_wrong(Publisher) ->
    meck:new(httpc),
    meck:expect(httpc, request, fun(post, _, _, _) ->
                                        {ok, {{v, 200, rp}, h, binary:bin_to_list(jsx:encode([{<<"authorized">>, <<"false">>}]))}} end),
    Res = gen_server:call(Publisher, {publish, <<"hi">>}),
    meck:unload(httpc),
    ?_assertEqual({error, no_authorization}, Res).
    

try_send_success(Publisher) ->
    Res = gen_server:call(Publisher, {publish, <<"hi">>}),
    ?_assertEqual(ok, Res).

try_send_asks_auth(Publisher) ->
    meck:new(carotene_authorization),
    meck:expect(carotene_authorization, check_authorization, fun(_, _, _) -> ok end),
    Res = gen_server:call(Publisher, {publish, <<"hi">>}),
    meck:unload(carotene_authorization),
    ?_assertEqual(ok, Res).
