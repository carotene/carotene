-module(api_channels_handler).

-export([init/3]).
-export([terminate/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([channel_to_json/2]).

init(_Type, Req, _Opts) ->
    {{IP, _Port}, _} = cowboy_req:peer(Req),
    case carotene_api_authorization:authorize(IP) of
        true -> {upgrade, protocol, cowboy_rest};
        false ->
            {ok, Req2} = cowboy_req:reply(500, [
                                                {<<"content-type">>, <<"text/plain">>}
                                               ], "You are not authorized to access this endpoint. Check your configuration.", Req),
            {shutdown, Req2, no_state}
    end.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, channel_to_json}
     ], Req, State}.

channel_to_json(Req, {index, Channels}) ->
    Body = jsx:encode(Channels),
    {Body, Req, Channels};
channel_to_json(Req, Channel) ->
    Body = jsx:encode([{<<"channel">>, Channel}]),
    {Body, Req, Channel}.

resource_exists(Req, _State) ->
    {ok, Exs} = gen_server:call(carotene_admin_connection, get_subscribed),
    case cowboy_req:binding(channel, Req) of
        undefined ->
            {true, Req, {index, Exs}};
        {Channel, _Bin} ->
            case lists:member(Channel, Exs) of
                true -> {true, Req, Channel};
                false -> {false, Req, Channel}
            end
    end.

terminate(_Reason, _Req, _State) ->
    ok.
