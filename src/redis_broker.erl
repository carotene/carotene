-module(redis_broker).

-behaviour(gen_server).
-export([start_link/1, start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

-record(state, {client, subclient, supervisor, exchange_supervisor, queue_supervisor, smart_sub}).

start_link(Sup) ->
    Opts = [],
    gen_server:start_link(?MODULE, [Sup], Opts).

start(Sup) ->
    Opts = [],
    gen_server:start(?MODULE, [Sup], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([Sup]) ->
    {ok, Client} = eredis:start_link(),
    {ok, SubClient} = eredis_sub:start_link(),
    {ok, SmartSubClient} = eredis_smart_sub:start_link(SubClient),
    self() ! {start_redis_exchange_sup, Client},
    self() ! {start_redis_queue_sup, SmartSubClient},

    {ok, #state{client = Client, supervisor = Sup}}.

handle_info({start_redis_exchange_sup, Client}, State = #state{supervisor = Sup}) ->
    {ok, ExchangeSup} = supervisor:start_child(Sup, {redis_exchange_sup,
          {redis_exchange_sup, start_link, [Client]},
          permanent,
          infinity,
          supervisor,
          [redis_exchange_sup]}),
    {noreply, State#state{exchange_supervisor = ExchangeSup}};

handle_info({start_redis_queue_sup, SmartSubClient}, State = #state{supervisor = Sup}) ->
    {ok, QueueSup} = supervisor:start_child(Sup, {redis_queue_sup,
          {redis_queue_sup, start_link, [SmartSubClient]},
          permanent,
          infinity,
          supervisor,
          [redis_queue_sup]}),
    {noreply, State#state{queue_supervisor = QueueSup}};

handle_info(shutdown, State) ->
    {stop, normal, State}.

handle_call(start_exchange, _From, State = #state{exchange_supervisor = ExchangeSup}) ->
    {ok, Exchange} = supervisor:start_child(ExchangeSup, []),

    {reply, {ok, Exchange}, State};
handle_call(start_queue, _From, State = #state{queue_supervisor = QueueSup}) ->
    {ok, Queue} = supervisor:start_child(QueueSup, []),

    {reply, {ok, Queue}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.
