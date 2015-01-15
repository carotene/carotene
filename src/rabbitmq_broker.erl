-module(rabbitmq_broker).

-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).
-export([start_link/1, start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

-record(state, {channel, supervisor, connection, exchange_supervisor}).

start_link(Sup) ->
    Opts = [],
    gen_server:start_link(?MODULE, [Sup], Opts).

start(Sup) ->
    Opts = [],
    gen_server:start(?MODULE, [Sup], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([Sup]) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    self() ! {start_rabbitmq_exchange_sup, Channel},

    {ok, #state{channel = Channel, connection = Connection, supervisor = Sup}}.

handle_info({start_rabbitmq_exchange_sup, Channel}, State = #state{supervisor = Sup}) ->
    io:format("~s~n", ["STARTIN SUP"]),
    {ok, ExchangeSup} = supervisor:start_child(Sup, {rabbitmq_exchange_sup,
          {rabbitmq_exchange_sup, start_link, [Channel]},
          permanent,
          infinity,
          worker,
          [rabbitmq_exchange]}),
    link(ExchangeSup),

    {noreply, State#state{exchange_supervisor = ExchangeSup}};
handle_info(shutdown, State) ->
    {stop, normal, State}.

handle_call(get_channel, _From, State = #state{channel = Channel}) ->
    {reply, {ok, Channel}, State};
handle_call(start_exchange, _From, State = #state{channel = Channel, exchange_supervisor = ExchangeSup}) ->
    {ok, Exchange} = supervisor:start_child(ExchangeSup, []),

    {reply, {ok, Exchange}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, #state{channel = Channel, connection = Connection}) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.
