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

presence_to_json(Req, Channel) ->
    {_, UsersSub} = presence_serv:presence(Channel),
    Body = jsx:encode(UsersSub),
    {Body, Req, Channel}.

resource_exists(Req, _State) ->
    Channel = cowboy_req:binding(channel, Req),
    {true, Req, Channel}.
