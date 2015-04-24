-module(api_messages_handler).

-export([init/3]).
-export([terminate/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([publish_in_channel/3]).
-export([publish_message/2]).

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
    {[<<"POST">>], Req, State}.

%content_types_provided(Req, State) ->
%    {[
%      {{<<"application">>, <<"json">>, []}, channel_to_json}
%     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"x-www-form-urlencoded">>, []}, publish_message}
    ], Req, State}.

publish_message(Req, State) ->
    {Channel, _Bin} = cowboy_req:binding(channel, Req),
    publish_in_channel(Channel, Req, State).

publish_in_channel(Channel, Req, State) ->
    {ok, PostParams, Req2} = cowboy_req:body_qs(Req),
    {_, Message} = lists:keyfind(<<"message">>, 1, PostParams),
    gen_server:call(carotene_admin_connection, {publish, {channel, Channel}, {message, Message}}),
    {true, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
