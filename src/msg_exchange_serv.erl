-module(msg_exchange_serv).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export([start/1]).
-export([stop/1]).


-record(state, {exchange, broker, auth_config}).


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
    {ok, #state{exchange = Exchange, broker = Broker, auth_config = AuthConfig}}.

handle_info(shutdown, State) ->
    {stop, normal, State}.

handle_call({send, Message, UserId}, _From, State = #state{exchange = Exchange}) ->
    ok = gen_server:call(Exchange, {publish,  Message}),
    {reply, ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
