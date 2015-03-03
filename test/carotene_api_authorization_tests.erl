-module(carotene_api_authorization_tests).

-include_lib("eunit/include/eunit.hrl").

noconfig_test_() ->
    {"if config has not restrict api access ip entry then is authorized",
     [no_config()]
    }.

config_nomatch_test_() ->
    {"if config restrict api access ip and does not match then fails",
     [config_nomatch()]
    }.

config_match_test_() ->
    {"if config restrict api access ip and does match then succeeds",
     [config_match()]
    }.

%% Tests
no_config() ->
    application:unset_env(carotene, restrict_api_access_to),
    ?_assertEqual(true, carotene_api_authorization:authorize({127,0,0,1})).

config_nomatch() ->
    application:set_env(carotene, restrict_api_access_to, {192,168,1,66}),
    ?_assertEqual(false, carotene_api_authorization:authorize({127,0,0,1})).

config_match() ->
    application:set_env(carotene, restrict_api_access_to, {192,168,1,66}),
    ?_assertEqual(true, carotene_api_authorization:authorize({192,168,1,66})).
