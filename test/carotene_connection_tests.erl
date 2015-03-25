-module(carotene_connection_tests).

-include_lib("eunit/include/eunit.hrl").

start_test_() ->
    {"It should be possible to start a connection",
              [start_and_test_running_permanent()]
    }.

non_json_message_test_() ->
    {"If process_message receives a non-json message replies with error",
     {setup, 
        fun start_connection/0,
        fun stop/1,
        fun(Connection) ->
                [try_process_non_json(Connection)]
        end
     }}.

unknown_message_test_() ->
    {"If process_message receives an unknown message replies with error",
     {setup, 
        fun start_connection/0,
        fun stop/1,
        fun(Connection) ->
                [try_process_unknown_message(Connection)]
        end
     }}.

can_subscribe_to_channel_test_() ->
    {"Subscription can succeed",
     {setup, 
        fun start_connection/0,
        fun stop/1,
        fun(Connection) ->
                [try_succesful_subscribe(Connection)]
        end
     }}.

failed_subscription_will_send_reply_test_() ->
    {"Subscription can succeed",
     {setup, 
        fun start_connection/0,
        fun stop/1,
        fun(Connection) ->
                [try_failed_subscribe(Connection)]
        end
     }}.

can_publish_to_channel_test_() ->
    {"Publish can succeed",
     {setup, 
        fun start_connection/0,
        fun stop/1,
        fun(Connection) ->
                [try_succesful_publish(Connection)]
        end
     }}.

wont_authenticate_if_not_authenticate_url_test_() ->
    {"Authentication fails if no authenticate_url in config",
     {setup, 
        fun start_connection/0,
        fun stop/1,
        fun(Connection) ->
                [try_authenticate_no_config(Connection)]
        end
     }}.

bad_response_from_server_on_authentication_test_() ->
    {"Bad response from server on authentication sends error message",
     {setup, 
        fun start_connection/0,
        fun stop/1,
        fun(Connection) ->
                [try_authenticate_bad_response_from_server(Connection)]
        end
     }}.

bad_json_from_server_on_authentication_test_() ->
    {"Bad json from server on authentication sends error message",
     {setup, 
        fun start_connection/0,
        fun stop/1,
        fun(Connection) ->
                [try_authenticate_bad_json_from_server(Connection)]
        end
     }}.

not_authenticated_from_server_on_authentication_test_() ->
    {"Failed authentication in server sends error message",
     {setup, 
        fun start_connection/0,
        fun stop/1,
        fun(Connection) ->
                [try_authenticate_fails(Connection)]
        end
     }}.

authenticated_from_server_on_authentication_test_() ->
    {"Succesful authentication in server sends success message",
     {setup, 
        fun start_connection/0,
        fun stop/1,
        fun(Connection) ->
                [try_authenticate_success(Connection)]
        end
     }}.

authentication_after_subscription_test_() ->
    {"Succesful authentication after subscription updates subscriber",
     {setup, 
        fun start_connection/0,
        fun stop/1,
        fun(Connection) ->
                [try_subscribe_then_authenticate_success(Connection)]
        end
     }}.

cannot_ask_for_presence_if_not_configured_test_() ->
    {"Cannot ask for presence if not configures",
     {setup, 
        fun start_connection/0,
        fun stop/1,
        fun(Connection) ->
                [try_ask_presence_not_configured(Connection)]
        end
     }}.

can_ask_for_presence_test_() ->
    {"Can ask for presence",
     {setup, 
        fun start_connection/0,
        fun stop/1,
        fun(Connection) ->
                [try_ask_presence(Connection)]
        end
     }}.

cannot_ask_for_presence_when_no_subscribed_test_() ->
    {"Can ask for presence",
     {setup, 
        fun start_connection/0,
        fun stop/1,
        fun(Connection) ->
                [try_ask_presence_not_subscribed(Connection)]
        end
     }}.

intermittent_connection_sets_timeout_test_() ->
    test_set_timeout_intermittent().

intermittent_connection_sends_lists_test_() ->
    {"In long polling we will receive lists of messages",
     {setup, 
        fun start_intermittent_connection/0,
        fun stop/1,
        fun(Connection) ->
                [intermittent_receive_a_list(Connection)]
        end
     }}.

intermittent_hiatus_test_() ->
    {"When long polling is discontinued we buffer the messages",
     {setup, 
        fun start_intermittent_connection/0,
        fun stop/1,
        fun(Connection) ->
                [intermittent_receive_after_hiatus(Connection)]
        end
     }}.

%% Helpers
start_connection() ->
    {ok, Connection} = carotene_connection:start(self(), permanent),
    Connection.

start_intermittent_connection() ->
    {ok, Connection} = carotene_connection:start(self(), intermittent),
    Connection.

stop(Connection) ->
    carotene_connection:stop(Connection).

subscribe(Connection) ->
    meck:new(carotene_subscriber_sup, [passthrough]),
    meck:expect(carotene_subscriber_sup, start_child, fun(_Args) -> {ok, somepid} end),
    meck:new(carotene_subscriber, [passthrough]),
    meck:expect(carotene_subscriber, subscribe, fun(_Somepid) -> ok end),
    gen_server:cast(Connection, {process_message, jsonx:encode([{<<"subscribe">>, <<"room1">>}])}),
    % wait a bit for cast to work
    timer:sleep(10),
    ?assertEqual(true, meck:validate(carotene_subscriber)),
    ?assertEqual(true, meck:validate(carotene_subscriber_sup)),
    meck:unload(carotene_subscriber),
    meck:unload(carotene_subscriber_sup).

flush() ->
    receive
        _ -> 
            flush()
    after 0 ->
        ok
    end.

%% Tests
start_and_test_running_permanent() ->
    Res = carotene_connection:start(self(), permanent),
    ?_assertMatch({ok, _}, Res).

try_process_non_json(Connection) ->
    gen_server:cast(Connection, {process_message, <<"something non jsoney">>}),
    Expected = {text, jsonx:encode([{<<"type">>, <<"info">>}, {<<"payload">>, <<"Non JSON message received">>}])},
    Obtained = receive
                  Message -> Message
              after 2000 -> false
              end,
    ?_assertEqual(Expected, Obtained).

try_process_unknown_message(Connection) ->
    gen_server:cast(Connection, {process_message, jsonx:encode([{<<"I just want to">>, <<"chat a bit about life">>}])}),
    Expected = {text, jsonx:encode([{<<"type">>, <<"info">>}, {<<"payload">>, <<"Unknown message received">>}])},
    Obtained = receive
                  Message -> Message
              after 2000 -> false
              end,
    ?_assertEqual(Expected, Obtained).

try_succesful_subscribe(Connection) ->
    subscribe(Connection),
    % heavy work in meck
    ?_assertEqual(true, true).

try_failed_subscribe(Connection) ->
    meck:new(carotene_subscriber_sup, [passthrough]),
    meck:expect(carotene_subscriber_sup, start_child, fun(_Args) -> {ok, somepid} end),
    meck:new(carotene_subscriber, [passthrough]),
    meck:expect(carotene_subscriber, subscribe, fun(_Somepid) -> {error, <<"someerror">>} end),
    gen_server:cast(Connection, {process_message, jsonx:encode([{<<"subscribe">>, <<"room1">>}])}),
    Expected = {text, jsonx:encode([{<<"type">>, <<"info">>}, {<<"payload">>, <<"someerror">>}])},
    Obtained = receive
                  Message -> Message
              after 2000 -> false
              end,
    ?assertEqual(true, meck:validate(carotene_subscriber)),
    ?assertEqual(true, meck:validate(carotene_subscriber_sup)),
    meck:unload(carotene_subscriber),
    meck:unload(carotene_subscriber_sup),
    ?_assertEqual(Expected, Obtained).

try_succesful_publish(Connection) ->
    meck:new(carotene_publisher_sup, [passthrough]),
    meck:expect(carotene_publisher_sup, start_child, fun(_Args) -> {ok, somepid} end),
    meck:new(carotene_publisher),
    meck:expect(carotene_publisher, publish, fun(_Somepid, _SomeMessage) -> ok end),
    gen_server:cast(Connection, {process_message, jsonx:encode([{<<"channel">>, <<"room1">>}, {<<"publish">>, <<"hi">>}])}),
    % wait a bit for cast to work
    timer:sleep(10),
    ?assertEqual(true, meck:validate(carotene_publisher)),
    ?assertEqual(true, meck:validate(carotene_publisher_sup)),
    meck:unload(carotene_publisher),
    meck:unload(carotene_publisher_sup),
    % heavy work in meck
    ?_assertEqual(true, true).

try_authenticate_no_config(Connection) ->
    application:unset_env(carotene, authenticate_url),
    gen_server:cast(Connection, {process_message, jsonx:encode([{<<"authenticate">>, <<"user1">>}, {<<"token">>, <<"O_O">>}])}),
    Expected = {text, jsonx:encode([{<<"type">>, <<"info">>}, {<<"payload">>, <<"Authentication error: no authentication_url in config">>}])},
    Obtained = receive
                  Message -> Message
              after 2000 -> false
              end,
    ?_assertEqual(Expected, Obtained).

try_authenticate_bad_response_from_server(Connection) ->
    meck:new(httpc),
    application:set_env(carotene, authenticate_url, <<"http://some_url.com">>),
    meck:expect(httpc, request, fun(post, _, _, _) ->
                                        <<"HAHAHA">> end),
    gen_server:cast(Connection, {process_message, jsonx:encode([{<<"authenticate">>, <<"user1">>}, {<<"token">>, <<"O_O">>}])}),
    Expected = {text, jsonx:encode([{<<"type">>, <<"info">>}, {<<"payload">>, <<"Authentication error: Bad response from server">>}])},
    Obtained = receive
                  Message -> Message
              after 2000 -> false
              end,
    meck:unload(httpc),
    ?_assertEqual(Expected, Obtained).

try_authenticate_bad_json_from_server(Connection) ->
    meck:new(httpc),
    application:set_env(carotene, authenticate_url, <<"http://some_url.com">>),
    meck:expect(httpc, request, fun(post, _, _, _) ->
                                        {ok, {{v, 200, rp}, h, binary:bin_to_list(<<"you are authenticated, but this is not a json, that is how we roll">>)}} end),
    gen_server:cast(Connection, {process_message, jsonx:encode([{<<"authenticate">>, <<"user1">>}, {<<"token">>, <<"O_O">>}])}),
    Expected = {text, jsonx:encode([{<<"type">>, <<"info">>}, {<<"payload">>, <<"Authentication error: Bad JSON response from server">>}])},
    Obtained = receive
                  Message -> Message
              after 2000 -> false
              end,
    meck:unload(httpc),
    ?_assertEqual(Expected, Obtained).

try_authenticate_fails(Connection) ->
    meck:new(httpc),
    application:set_env(carotene, authenticate_url, <<"http://some_url.com">>),
    meck:expect(httpc, request, fun(post, _, _, _) ->
                                        {ok, {{v, 200, rp}, h, binary:bin_to_list(jsonx:encode([{<<"authenticated">>, false}]))}} end),
    gen_server:cast(Connection, {process_message, jsonx:encode([{<<"authenticate">>, <<"user1">>}, {<<"token">>, <<"O_O">>}])}),
    Expected = {text, jsonx:encode([{<<"type">>, <<"info">>}, {<<"payload">>, <<"Authentication failed">>}])},
    Obtained = receive
                  Message -> Message
              after 2000 -> false
              end,
    meck:unload(httpc),
    ?_assertEqual(Expected, Obtained).

try_authenticate_success(Connection) ->
    meck:new(httpc),
    application:set_env(carotene, authenticate_url, <<"http://some_url.com">>),
    meck:expect(httpc, request, fun(post, _, _, _) ->
                                        {ok, {{v, 200, rp}, h, binary:bin_to_list(jsonx:encode([{<<"authenticated">>, true}, {<<"user_data">>, <<"somedata">>}]))}} end),
    gen_server:cast(Connection, {process_message, jsonx:encode([{<<"authenticate">>, <<"user1">>}, {<<"token">>, <<"O_O">>}])}),
    Expected = {text, jsonx:encode([{<<"type">>, <<"info">>}, {<<"payload">>, <<"Authenticated">>}])},
    Obtained = receive
                  Message -> Message
              after 2000 -> false
              end,
    meck:unload(httpc),
    ?_assertEqual(Expected, Obtained).

try_subscribe_then_authenticate_success(Connection) ->
    subscribe(Connection),
    meck:new(httpc),
    meck:new(carotene_subscriber, [passthrough]),
    meck:expect(carotene_subscriber, update_user, fun(_Somepid, _UserId) -> ok end),
    gen_server:cast(Connection, {process_message, jsonx:encode([{<<"subscribe">>, <<"room1">>}])}),
    % wait a bit for cast to work
    timer:sleep(10),
    application:set_env(carotene, authenticate_url, <<"http://some_url.com">>),
    meck:expect(httpc, request, fun(post, _, _, _) ->
                                        {ok, {{v, 200, rp}, h, binary:bin_to_list(jsonx:encode([{<<"authenticated">>, true}, {<<"user_data">>, <<"somedata">>}]))}} end),
    gen_server:cast(Connection, {process_message, jsonx:encode([{<<"authenticate">>, <<"user1">>}, {<<"token">>, <<"O_O">>}])}),
    Expected = {text, jsonx:encode([{<<"type">>, <<"info">>}, {<<"payload">>, <<"Authenticated">>}])},
    Obtained = receive
                  Message -> Message
              after 2000 -> false
              end,
    ?assertEqual(true, meck:validate(carotene_subscriber)),
    meck:unload(httpc),
    meck:unload(carotene_subscriber),
    ?_assertEqual(Expected, Obtained).

try_ask_presence(Connection) ->
    application:set_env(carotene, presence, true),
    subscribe(Connection),
    meck:new(carotene_presence),
    meck:expect(carotene_presence, presence, fun(_Channel) -> [<<"someuser">>] end),
    gen_server:cast(Connection, {process_message, jsonx:encode([{<<"presence">>, <<"room1">>}])}),
    Expected = {text, jsonx:encode([{<<"type">>, <<"presence">>}, {<<"subscribers">>, [<<"someuser">>]}, {<<"channel">>, <<"room1">>}])},
    Obtained = receive
                  Message -> Message
              after 2000 -> false
              end,
    ?assertEqual(true, meck:validate(carotene_presence)),
    meck:unload(carotene_presence),
    ?_assertEqual(Expected, Obtained).

try_ask_presence_not_subscribed(Connection) ->
    application:set_env(carotene, presence, true),
    gen_server:cast(Connection, {process_message, jsonx:encode([{<<"presence">>, <<"room1">>}])}),
    Expected = {text, jsonx:encode([{<<"type">>, <<"info">>}, {<<"payload">>, <<"Cannot ask for presence when not subscribed to the channel">>}])},
    Obtained = receive
                  Message -> Message
              after 2000 -> false
              end,
    ?_assertEqual(Expected, Obtained).

try_ask_presence_not_configured(Connection) ->
    application:unset_env(carotene, presence),
    subscribe(Connection),
    gen_server:cast(Connection, {process_message, jsonx:encode([{<<"presence">>, <<"room1">>}])}),
    Expected = {text, jsonx:encode([{<<"type">>, <<"info">>}, {<<"payload">>, <<"Presence is disabled">>}])},
    Obtained = receive
                  Message -> Message
              after 2000 -> false
              end,
    ?_assertEqual(Expected, Obtained).

test_set_timeout_intermittent() ->
    {ok, State} = carotene_connection:init([self(), intermittent]),
    ?_assertNotEqual(false, carotene_connection:timer_status(State)).

intermittent_receive_a_list(Connection) ->
    flush(),
    Connection ! {received_message, hola},
    Obtained = receive
                  Message -> 
                       Message
              after 2000 -> false
              end,
    ?_assertEqual({list, [hola]}, Obtained).

intermittent_receive_after_hiatus(Connection) ->
    flush(),
    Connection ! transport_hiatus,
    Connection ! {received_message, hola},
    Connection ! {received_message, amigo},
    gen_server:cast(Connection, {keepalive, self()}),
    Obtained = receive
                  Message -> 
                       Message
              after 2000 -> false
              end,
    ?_assertEqual({list, [hola, amigo]}, Obtained).
