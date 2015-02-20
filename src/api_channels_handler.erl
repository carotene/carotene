-module(api_channels_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([from_json/2, subscribe_channel/2, subscribe_channel/3]).


-export([channel_to_json/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, channel_to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, from_json}
    ], Req, State}.

from_json(Req, State) ->
    case cowboy_req:binding(channel, Req) of
        undefined ->
            subscribe_channel(Req, State);
        Channel ->
            subscribe_channel(Channel, Req, State)
    end.

subscribe_channel(Channel, Req, State) ->
    ok = gen_server:call(admin_serv, {subscribe, {channel, Channel}}),

    case cowboy_req:method(Req) of
        <<"POST">> ->
            {{true, Channel}, Req, State};
        <<"PUT">> ->
            {{true, Channel}, Req, State};
        _ ->
            {true, Req, State}
    end.

subscribe_channel(Req, State) ->
    {ok, PostParams, Req2} = cowboy_req:body_qs(Req),
    {_, Channel} = lists:keyfind(<<"channel">>, 1, PostParams),

    ok = gen_server:call(admin_serv, {subscribe, {channel, Channel}}),

    case cowboy_req:method(Req2) of
        <<"POST">> ->
            {{true, Channel}, Req2, State};
        <<"PUT">> ->
            {{true, Channel}, Req2, State};
        _ ->
            {true, Req2, State}
    end.

channel_to_json(Req, {index, Channels}) ->
    Body = jsx:encode(Channels),
    {Body, Req, Channels};
channel_to_json(Req, Channel) ->
    Body = jsx:encode([{<<"channel">>, Channel}]),
    {Body, Req, Channel}.

resource_exists(Req, _State) ->
    {ok, Exs} = gen_server:call(admin_serv, get_subscribed),
    case cowboy_req:binding(channel, Req) of
        undefined ->
            {true, Req, {index, Exs}};
        Channel ->
            case lists:member(Channel, Exs) of
                true -> {true, Req, Channel};
                false -> {false, Req, Channel}
            end
    end.
