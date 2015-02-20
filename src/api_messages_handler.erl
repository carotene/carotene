-module(api_messages_handler).

-export([init/2]).
-export([allowed_methods/2]).
%-export([content_types_provided/2]).
-export([content_types_accepted/2]).
%-export([resource_exists/2]).
-export([publish_in_channel/3]).
-export([from_json/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

%content_types_provided(Req, State) ->
%    {[
%      {{<<"application">>, <<"json">>, []}, channel_to_json}
%     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, from_json}
    ], Req, State}.

from_json(Req, State) ->
    Channel = cowboy_req:binding(channel, Req),
    publish_in_channel(channel, Req, State).

publish_in_channel(Channel, Req, State) ->
    {ok, PostParams, Req2} = cowboy_req:body_qs(Req),
    {_, Message} = lists:keyfind(<<"message">>, 1, PostParams),
    gen_server:call(admin_serv, {publish, {channel, Channel}, {message, Message}}),
    {true, Req2, State}.
