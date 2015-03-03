-module(carotene_api_authorization).

-export([authorize/1]).

-include_lib("eunit/include/eunit.hrl").

authorize(Ip)  ->
    case application:get_env(carotene, restrict_api_access_to) of
        undefined -> true;
        {ok, Ip} -> true;
        SomeIp -> false
    end.
