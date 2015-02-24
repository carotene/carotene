-module(rabbitmq_broker).

-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).
-export([start_link/1, start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

-record(state, {channel, supervisor, connection, queue_supervisor}).

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
    self() ! {start_rabbitmq_queue_sup, Channel},

    {ok, #state{channel = Channel, connection = Connection, supervisor = Sup}}.

handle_info({start_rabbitmq_queue_sup, Channel}, State = #state{supervisor = Sup}) ->
    {ok, QueueSup} = supervisor:start_child(Sup, {rabbitmq_queue_sup,
          {rabbitmq_queue_sup, start_link, [Channel]},
          permanent,
          infinity,
          worker,
          [rabbitmq_queue_sup]}),
    {noreply, State#state{queue_supervisor = QueueSup}};

handle_info(shutdown, State) ->
    {stop, normal, State}.

handle_call(start_subscriber, _From, State = #state{queue_supervisor = QueueSup}) ->
    {ok, Queue} = supervisor:start_child(QueueSup, []),

    {reply, {ok, Queue}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({publish, Message, channel, Channel}, State = #state{channel = RabbitChannel}) ->
    amqp_channel:call(RabbitChannel, #'exchange.declare'{exchange = Channel,
                                                   type = <<"fanout">>,
                                                   durable = false,
                                                   passive = true,
                                                   internal = false
                                                  }),
    amqp_channel:cast(RabbitChannel,
                      #'basic.publish'{
                        exchange = Channel,
                        routing_key = <<"">>},
                      #amqp_msg{payload = Message}),
    {noreply, State};

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, #state{channel = Channel, connection = Connection}) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.
