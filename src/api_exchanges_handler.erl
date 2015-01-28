-module(api_exchanges_handler).
%
%-export([init/2]).
%-export([allowed_methods/2]).
%-export([content_types_provided/2]).
%-export([content_types_accepted/2]).
%-export([resource_exists/2]).
%
%-export([exchange_to_json/2]).
%
%-record(state, {
%          exchanges = [],
%          queues = []
%}).
%
%init(Req, Opts) ->
%    {cowboy_rest, Req, Opts}.
%
%allowed_methods(Req, State) ->
%    {[<<"GET">>, <<"POST">>], Req, State}.
%
%content_types_provided(Req, State) ->
%    {[
%        {<<"application/json">>, exchange_to_json}
%    ], Req, State}.
%
%content_types_accepted(Req, State) ->
%    {[
%        {<<"application/json">>, create_exchange}
%    ], Req, State}.
%
%create_exchange(Req, State=#state{exchanges=Exs, queues=Qs}) ->
%    {ok, [{<<"exchange_name">>, ExchangeName}], Req2} = cowboy_req:body_qs(Req),
%    io:format("User ~p joins exchange ~p~n", [admin, ExchangeName]),
%    {ok, ExchangePid} = supervisor:start_child(msg_exchange_sup, [ExchangeName, admin]),
%    {ok, QueuePid} = supervisor:start_child(msg_queue_sup, [ExchangeName, admin, self()]),
%    % TODO: add only once
%    NewState = State#state{exchanges = dict:append(ExchangeName, ExchangePid, Exs), queues = dict:append(ExchangeName, QueuePid, Qs)},
%
%    case cowboy_req:method(Req2) of
%        <<"POST">> ->
%            {{true, <<$/, ExchangeName/binary>>}, Req2, NewState};
%        _ ->
%            {true, Req2, NewState}
%    end.
%
%exchange_to_json(Req, {index, Exchanges}) ->
%    Body = jsx:encode(Exchanges),
%    {Body, Req, Exchanges};
%exchange_to_json(Req, ExchangeName) ->
%    Body = jsx:encode([{<<"exchange_name">>, ExchangeName}]),
%    {Body, Req, ExchangeName}.
%
%resource_exists(Req, _State = #state{exchanges=Exs}) ->
%    case cowboy_req:binding(exchange_name, Req) of
%        undefined ->
%            {true, Req, {index, Exs}};
%        ExchangeName ->
%            case dict:find(ExchangeName) of
%                {ok, _} -> {true, Req, ExchangeName};
%                error -> {false, Req, ExchangeName}
%            end
%    end.
