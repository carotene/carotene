-module(exchange_tests).

-include_lib("eunit/include/eunit.hrl").

start_test_() ->
    {"It should be possible to start an exchange",
              [start_and_test_running()]
      }.

publish_anonymous_authlevel_test_() ->
    {"Can't publish if I am anonymous and level is auth",
     {setup, 
        fun start_as_anonymous_with_config_level_auth/0,
        fun stop/1,
        fun(ExchangeServ) ->
                [try_send_and_needs_auth(ExchangeServ)]
        end
     }}.

publish_anonymous_asklevel_test_() ->
    {"Can't publish if I am anonymous asking is false",
     {setup, 
        fun start_as_identified_with_config_level_ask/0,
        fun stop/1,
        fun(ExchangeServ) ->
                [try_send_and_auth_wrong(ExchangeServ)]
        end
     }}.

%% Helpers
set_config_level_auth() ->
    application:set_env(carotene, publish_auth, [{enabled, true},
                                                 {level, auth}
                                                ]).

set_config_level_ask() ->
    application:set_env(carotene, publish_auth, [{enabled, true},
                                                 {authorization_url, "http://example.com"},
                                                 {level, ask}
                                                ]).

start(UserId) ->
    meck:new(broker_sup),
    meck:expect(broker_sup, get_broker, fun() ->
                                                {broker_module_mock, broker_mock} end),
    meck:new(broker_module_mock, [non_strict]),
    meck:expect(broker_module_mock, start_exchange, fun(_Broker) ->
                                                            {ok, some_exchange} end),
    meck:expect(broker_module_mock, declare_exchange, fun(_Exchange, _ExchangeSpecs) ->
                                                              ok end),
    Res = msg_exchange_serv:start(<<"room1">>, UserId),
    
    meck:unload(broker_sup),
    meck:unload(broker_module_mock),
    {ok, Pid} = Res,
    Pid.

%% Setups/Teardowns
start_as_anonymous_with_config_level_auth() ->
    set_config_level_auth(),
    start(undefined).

start_as_identified_with_config_level_ask() ->
    set_config_level_ask(),
    start(<<"user1">>).

stop(ExchangeServ) ->
    msg_exchange_serv:stop(ExchangeServ).

%% Tests
start_and_test_running() ->
    meck:new(broker_sup),
    meck:expect(broker_sup, get_broker, fun() ->
                                                {broker_module_mock, broker_mock} end),
    meck:new(broker_module_mock, [non_strict]),
    meck:expect(broker_module_mock, start_exchange, fun(_Broker) ->
                                                            {ok, some_broker_exchange} end),
    meck:expect(broker_module_mock, declare_exchange, fun(_Exchange, _ExchangeSpecs) ->
                                                              ok end),
    application:set_env(carotene, publish_auth, nothing),
    Res = msg_exchange_serv:start(<<"room1">>, <<"user1">>),
    meck:unload(broker_sup),
    meck:unload(broker_module_mock),
    ?_assertMatch({ok, _}, Res).

try_send_and_needs_auth(ExchangeServ) ->
    Res = gen_server:call(ExchangeServ, {send, <<"hi">>}),
    ?_assertEqual({error, needs_authentication}, Res).

try_send_and_auth_wrong(ExchangeServ) ->
    meck:new(httpc),
    meck:expect(httpc, request, fun(post, _, _, _) ->
                                        {ok, {{v, 200, rp}, h, binary:bin_to_list(jsx:encode([{<<"authorized">>, <<"false">>}]))}} end),
    Res = gen_server:call(ExchangeServ, {send, <<"hi">>}),
    meck:unload(httpc),
    ?_assertEqual({error, no_authorization}, Res).
    
