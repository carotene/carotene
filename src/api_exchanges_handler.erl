-module(api_exchanges_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([from_json/2, config_exchange/2, config_exchange/3]).


-export([exchange_to_json/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, exchange_to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, from_json}
    ], Req, State}.

from_json(Req, State) ->
    case cowboy_req:binding(exchange_name, Req) of
        undefined ->
            config_exchange(Req, State);
        ExchangeName ->
            config_exchange(ExchangeName, Req, State)
    end.

publish_in_exchange(ExchangeName, Req, State) ->
    {ok, PostParams, Req2} = cowboy_req:body_qs(Req),
    {_, Message} = lists:keyfind(<<"message">>, 1, PostParams),
    gen_server:call(admin_serv, {publish, {exchange_name, ExchangeName}, {message, Message}}),
    {true, Req2, State}.

config_exchange(ExchangeName, Req, State) ->
    {ok, PostParams, Req2} = cowboy_req:body_qs(Req),
    {_, Url} = lists:keyfind(<<"subscribe_callback_url">>, 1, PostParams),
    {_, Subscribe} = lists:keyfind(<<"subscribe">>, 1, PostParams),

    SaneSubscribe = case Subscribe of
                        <<"false">> -> false;
                        <<"true">> -> true;
                        _ -> "unrecognized option in subscribe"
                    end,

    ok = gen_server:call(admin_serv, {config_exchange, {exchange_name, ExchangeName}, {subscribe, SaneSubscribe}, {subscribe_callback_url, Url}}),

    case cowboy_req:method(Req2) of
        <<"POST">> ->
            {{true, ExchangeName}, Req2, State};
        _ ->
            {true, Req2, State}
    end.
config_exchange(Req, State) ->
    {ok, PostParams, Req2} = cowboy_req:body_qs(Req),
    {_, ExchangeName} = lists:keyfind(<<"exchange_name">>, 1, PostParams),
    {_, Url} = lists:keyfind(<<"subscribe_callback_url">>, 1, PostParams),
    {_, Subscribe} = lists:keyfind(<<"subscribe">>, 1, PostParams),

    SaneSubscribe = case Subscribe of
                        <<"false">> -> false;
                        <<"true">> -> true;
                        _ -> "unrecognized option in subscribe"
                    end,

    ok = gen_server:call(admin_serv, {cconfig_exchange, {exchange_name, ExchangeName}, {subscribe, SaneSubscribe}, {subscribe_callback_url, Url}}),

    case cowboy_req:method(Req2) of
        <<"POST">> ->
            {{true, ExchangeName}, Req2, State};
        _ ->
            {true, Req2, State}
    end.

exchange_to_json(Req, {index, Exchanges}) ->
    Body = jsx:encode(Exchanges),
    {Body, Req, Exchanges};
exchange_to_json(Req, ExchangeName) ->
    Body = jsx:encode([{<<"exchange_name">>, ExchangeName}]),
    {Body, Req, ExchangeName}.

resource_exists(Req, _State) ->
    {ok, Exs} = gen_server:call(admin_serv, get_subscribed),
    case cowboy_req:binding(exchange_name, Req) of
        undefined ->
            {true, Req, {index, Exs}};
        ExchangeName ->
            case lists:member(ExchangeName, Exs) of
                true -> {true, Req, ExchangeName};
                false -> {false, Req, ExchangeName}
            end
    end.
