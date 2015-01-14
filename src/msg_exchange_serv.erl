-module(msg_exchange_serv).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export([start/1]).
-export([stop/1]).


-record(state, {exchange, broker}).


start(Exchange) ->
    Opts = [],
    gen_server:start(?MODULE, [Exchange], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([Exchange]) ->
    Broker = broker_sup:get_broker(),
    ok = gen_server:call(Broker, {declare_exchange, {Exchange, <<"fanout">>}}),

    {ok, #state{exchange = Exchange, broker = Broker}}.

handle_info(shutdown, State) ->
    {stop, normal, State}.

handle_call({send, Message}, _From, State = #state{exchange = Exchange, broker = Broker}) ->
    ok = gen_server:call(Broker, {publish, {Exchange, Message}}),
    {reply, ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
