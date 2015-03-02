-module(carotene_http_tests).

-include_lib("eunit/include/eunit.hrl").

http_test_() ->
    {"if config has not ssl entry then we start http",
     [http_run()]
    }.

https_test_() ->
    {"if config has ssl entry then we start https",
     [https_run()]
    }.

https_nocacert_test_() ->
    {"if config has ssl entry but no cacert file then we get an exception",
     [https_nocacert_run()]
    }.

https_nocert_test_() ->
    {"if config has ssl entry but no cert file then we get an exception",
     [https_nocert_run()]
    }.

https_nokeyfile_test_() ->
    {"if config has ssl entry but no key file then we get an exception",
     [https_nokey_run()]
    }.

%% Tests
http_run() ->
    application:unset_env(carotene, ssl),
    meck:new(cowboy),
    meck:new(cowboy_router),
    meck:expect(cowboy, start_http, fun(http, _, _, _) -> {ok, ok} end),
    meck:expect(cowboy_router, compile, fun(_) -> ok end),
    Res = carotene_http:init([]),
    meck:validate(cowboy),
    meck:unload(cowboy),
    meck:unload(cowboy_router),
    % Heay lifting by meck
    ?_assertMatch({ok, _}, Res).

https_run() ->
    application:set_env(carotene, ssl, [
                                        {cacertfile, "somefile"},
                                        {certfile, "somefile"},
                                        {keyfile, "somefile"}
                                       ]),
    meck:new(cowboy),
    meck:new(cowboy_router),
    meck:expect(cowboy, start_https, fun(https, _, _, _) -> {ok, ok} end),
    meck:expect(cowboy_router, compile, fun(_) -> ok end),
    carotene_http:init([]),
    MeckRes = meck:validate(cowboy),
    meck:unload(cowboy),
    meck:unload(cowboy_router),
    % Heay lifting by meck
    ?_assertEqual(true, MeckRes).

https_nocacert_run() ->
    application:set_env(carotene, ssl, [
                                        {certfile, "somefile"},
                                        {keyfile, "somefile"}
                                       ]),
    ?_assertThrow({ssl_config_error, "SSL enabled but no cacertfile specified"}, carotene_http:init([])).

https_nocert_run() ->
    application:set_env(carotene, ssl, [
                                        {cacertfile, "somefile"},
                                        {keyfile, "somefile"}
                                       ]),
    ?_assertThrow({ssl_config_error, "SSL enabled but no certfile specified"}, carotene_http:init([])).

https_nokey_run() ->
    application:set_env(carotene, ssl, [
                                        {cacertfile, "somefile"},
                                        {certfile, "somefile"}
                                       ]),
    ?_assertThrow({ssl_config_error, "SSL enabled but no keyfile specified"}, carotene_http:init([])).
