-module(msg_exchange_serv).

-behaviour(gen_server).

-export([start/1]).
-export([stop/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-record(state, {exchange, exchange_name, broker, auth_config, already_auth}).


start(Exchange) ->
    Opts = [],
    gen_server:start(?MODULE, [Exchange], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([ExchangeName]) ->
    {BrokerModule, Broker} = broker_sup:get_broker(),
    {ok, Exchange} = apply(BrokerModule, start_exchange, [Broker]),
    ok = apply(BrokerModule, declare_exchange, [Exchange, {ExchangeName, <<"faonout">>}]),
    {ok, AuthConfig} = application:get_env(carotene, publish_auth),
    {ok, #state{exchange = Exchange, exchange_name = ExchangeName, broker = Broker, auth_config = AuthConfig, already_auth = false}}.

handle_info(shutdown, State) ->
    {stop, normal, State}.

handle_call({send, Message, UserId}, _From, State = #state{exchange = Exchange, exchange_name = ExchangeName, auth_config = AuthConfig}) ->
    case already_auth of
        true -> ok = gen_server:call(Exchange, {publish,  Message}),
                {reply, ok, State};
        _ ->
            case can_publish(UserId, AuthConfig, ExchangeName) of
                ok ->
                    ok = gen_server:call(Exchange, {publish,  Message}),
                    {reply, ok, State#state{already_auth = true}};
                Error -> {reply, {error, Error}, State}
            end
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal
can_publish(UserId, AuthConfig, ExchangeName) ->
    case lists:keyfind(enabled, 1, AuthConfig) of
        false -> ok;
        {enabled, false} -> ok;
        {enabled, true} -> case lists:keyfind(level, 1, AuthConfig) of
                               false -> bad_configuration;
                               {level, anonymous} -> ok;
                               {level, auth} -> case UserId of
                                                    undefined -> needs_authentication;
                                                    _ -> ok
                                                end;
                               {level, ask} -> case ask_authentication(UserId, AuthConfig, ExchangeName) of
                                                   true -> ok;
                                                   Error -> Error
                                               end
                           end;
        _ -> ok
    end.

ask_authentication(UserId, AuthConfig, ExchangeName) ->
    case lists:keyfind(authorization_url, 1, AuthConfig) of
        false -> bad_configuration;
        {authorization_url, AuthorizeUrl} ->
            {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {AuthorizeUrl, [], "application/x-www-form-urlencoded", "user_id="++binary_to_list(UserId)++"&exchange="++binary_to_list(ExchangeName)}, [], []),
            case jsx:decode(binary:list_to_bin(Body)) of
                [{<<"authorized">>, <<"true">>}] -> true;
                [{<<"authorized">>, <<"false">>}] -> no_authorization;
                Tell -> 
                    bad_server_response_on_authorization
            end
    end.



