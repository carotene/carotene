-module(api_presence_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([presence_to_json/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, presence_to_json}
     ], Req, State}.

presence_to_json(Req, ExchangeName) ->
    {_, UsersSub} = presence_serv:presence(ExchangeName),
    Body = jsx:encode(UsersSub),
    {Body, Req, ExchangeName}.

resource_exists(Req, _State) ->
    ExchangeName = cowboy_req:binding(exchange_name, Req),
    {true, Req, ExchangeName}.
