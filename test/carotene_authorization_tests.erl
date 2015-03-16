-module(carotene_authorization_tests).

-include_lib("eunit/include/eunit.hrl").

level_must_exist_test() ->
    Res = carotene_authorization:check_authorization(anonymous, <<"room1">>, []),
    ?assertEqual(<<"bad value for level in configuration">>, Res).

level_must_be_an_allowed_value_test() ->
    Res = carotene_authorization:check_authorization(anonymous, <<"room1">>, [{level, whatever_man}]),
    ?assertEqual(<<"bad value for level in configuration">>, Res).

when_level_anonymous_always_success_test() ->
    Res = carotene_authorization:check_authorization(anonymous, <<"room1">>, [{level, anonymous}]),
    ?assertEqual(true, Res).

when_level_authenticated_anonymous_fail_others_succeed_test() ->
    Res = carotene_authorization:check_authorization(anonymous, <<"room1">>, [{level, authenticated}]),
    ?assertEqual(<<"needs authentication">>, Res),
    Res2 = carotene_authorization:check_authorization(<<"someuser">>, <<"room1">>, [{level, authenticated}]),
    ?assertEqual(true, Res2).

when_level_ask_anonymous_fail_test() ->
    Res = carotene_authorization:check_authorization(anonymous, <<"room1">>, [{level, ask}]),
    ?assertEqual(<<"needs authentication">>, Res).

when_level_ask_fails_if_authorization_url_not_present_test() ->
    Res = carotene_authorization:check_authorization(<<"user1">>, <<"room1">>, [{level, ask}]),
    ?assertEqual(<<"bad value for authorization url in configuration">>, Res).

when_level_ask_user_asks_for_authorization_test() ->
    meck:new(httpc),
    meck:expect(httpc, request, fun(post, _, _, _) ->
                                        {ok, {{v, 200, rp}, h, binary:bin_to_list(jsonx:encode([{<<"authorized">>, false}]))}} end),
    Res = carotene_authorization:check_authorization(<<"user1">>, <<"room1">>, [{level, ask}, {authorization_url, <<"http://someurl.com">>}]),
    meck:unload(httpc),
    ?assertEqual(<<"no authorization">>, Res).

when_level_ask_user_asks_for_authorization_can_succeed_test() ->
    meck:new(httpc),
    meck:expect(httpc, request, fun(post, _, _, _) ->
                                        {ok, {{v, 200, rp}, h, binary:bin_to_list(jsonx:encode([{<<"authorized">>, true}]))}} end),
    Res = carotene_authorization:check_authorization(<<"user1">>, <<"room1">>, [{level, ask}, {authorization_url, <<"http://someurl.com">>}]),
    meck:unload(httpc),
    ?assertEqual(true, Res).

malformed_json_from_server_in_authorization_fails_test() ->
    meck:new(httpc),
    meck:expect(httpc, request, fun(post, _, _, _) ->
                                        {ok, {{v, 200, rp}, h, binary:bin_to_list(<<"authorized true">>)}} end),
    Res = carotene_authorization:check_authorization(<<"user1">>, <<"room1">>, [{level, ask}, {authorization_url, <<"http://someurl.com">>}]),
    meck:unload(httpc),
    ?assertEqual(<<"malformed json from server on authorization">>, Res).

bad_response_from_server_on_authorization_failt_test() ->
    meck:new(httpc),
    meck:expect(httpc, request, fun(post, _, _, _) ->
                                        {ok, {{v, 200, rp}, h, binary:bin_to_list(jsonx:encode([{<<"server_says">>, <<"some_stuff">>}]))}} end),
    Res = carotene_authorization:check_authorization(<<"user1">>, <<"room1">>, [{level, ask}, {authorization_url, <<"http://someurl.com">>}]),
    meck:unload(httpc),
    ?assertEqual(<<"bad response from server on authorization">>, Res).
