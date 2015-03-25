-module(carotene_cors).

-behaviour(cowboy_middleware).

-export([execute/2, set_cors_headers/1]).

execute(Req, Env) ->
    {ok, ReqWithCorsHeaders} = set_cors_headers(Req),
    {Method, ReqMethod} = cowboy_req:method(ReqWithCorsHeaders),

    case Method of
        <<"OPTIONS">> ->
            {ok, ReqFinal} = cowboy_req:reply(200, ReqMethod),
            {halt, ReqFinal};
        _ ->
            %% continue as normal
            {ok, ReqMethod, Env}
    end.
%execute(Req, Env) ->
%    {ok, ReqWithCorsHeaders} = set_cors_headers(Req),
%    {Method, ReqMethod} = cowboy_req:method(ReqWithCorsHeaders),
%
%    case Method of
%        <<"OPTIONS">> ->
%            {ok, ReqFinal} = cowboy_req:reply(200, ReqMethod),
%            {halt, ReqFinal};
%        _ ->
%            %% continue as normal
%            {ok, Req, Env}
%    end.

%% ===================================================================
%% Helpers
%% ===================================================================

set_headers(Headers, Req) ->
    ReqWithHeaders = lists:foldl(fun({Header, Value}, ReqIn) ->
                                         ReqWithHeader = cowboy_req:set_resp_header(Header, Value, ReqIn),
                                         ReqWithHeader
                                 end, Req, Headers),
    {ok, ReqWithHeaders}.


set_cors_headers(Req) ->
    Headers = [{<<"Access-Control-Allow-Origin">>, <<"*">>},
               {<<"Access-Control-Allow-Methods">>, <<"POST, GET, OPTIONS">>},
               {<<"Access-Control-Allow-Headers">>, <<"Origin, X-Requested-With, Content-Type, Accept, X-Socket-Transport, Connection-id">>},
               {<<"Access-Control-Expose-Headers">>, <<"connection-id">>},
               {<<"Access-Control-Max-Age">>, <<"1000">>}],
    {ok, Req2} = set_headers(Headers, Req),
    {ok, Req2}.
