-module(api_exchanges_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([create_exchange/2]).


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
      {{<<"application">>, <<"json">>, []}, create_exchange},
      {{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_exchange}
    ], Req, State}.

create_exchange(Req, State) ->
    {ok, [{<<"exchange_name">>, ExchangeName}], Req2} = cowboy_req:body_qs(Req),

    ok = gen_server:call(admin_serv, {join_exchange, ExchangeName}),

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
    {ok, Exs} = gen_server:call(admin_serv, get_exchanges),
    case cowboy_req:binding(exchange_name, Req) of
        undefined ->
            {true, Req, {index, Exs}};
        ExchangeName ->
            case lists:member(ExchangeName, Exs) of
                true -> {true, Req, ExchangeName};
                false -> {false, Req, ExchangeName}
            end
    end.
