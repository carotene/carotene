-module(rabbitmq_queue).

-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).
-export([start_link/1, start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

-record(state, {channel, exchange, reply_pid}).

start_link(Channel) ->
    Opts = [],
    gen_server:start_link(?MODULE, [Channel], Opts).

start(Channel) ->
    Opts = [],
    gen_server:start(?MODULE, [Channel], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([Channel]) ->
    {ok, #state{channel = Channel}}.

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info(#'basic.cancel_ok'{}, State) ->
    {stop, normal, State};

handle_info({#'basic.deliver'{}, #amqp_msg{payload = Msg}}, #state{exchange = Exchange, reply_pid = ReplyPid} = State) ->
    ReplyPid ! {received_message, Msg, exchange, Exchange},
    {noreply, State};
handle_info(shutdown, State) ->
    {stop, normal, State}.

handle_call({declare_queue}, _From, State = #state{channel = Channel}) ->
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{exclusive = true}),
    {reply, {ok, Queue}, State};
handle_call({queue_bind, Queue, Exchange}, _From, State = #state{channel = Channel}) ->
    amqp_channel:call(Channel, #'queue.bind'{queue = Queue,
                                             exchange = Exchange
                                            }),
    {reply, ok, State#state{ exchange = Exchange}};
handle_call({consume, Queue, ReplyPid}, _From, State = #state{channel = Channel}) ->
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue,
                                                     no_ack = true}, self()),
    {reply, ok, State#state{reply_pid = ReplyPid}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.
