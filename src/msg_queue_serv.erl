-module(msg_queue_serv).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export([start/2]).
-export([stop/1]).


-record(state, {queue_server, reply_pid}).

start(ExchangeName, ReplyPid) ->
    Opts = [],
    gen_server:start(?MODULE, [ExchangeName, ReplyPid], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([ExchangeName, ReplyPid]) ->
    Broker = broker_sup:get_broker(),
    {ok, QueueServer} = gen_server:call(Broker, start_queue),
    {ok, Queue} = gen_server:call(QueueServer, {declare_queue}),
    ok = gen_server:call(QueueServer, {queue_bind, Queue, ExchangeName}),
    ok = gen_server:call(QueueServer, {consume, Queue, self()}),
    {ok, #state{queue_server = QueueServer, reply_pid = ReplyPid}}.

handle_info({received_message, Msg}, State = #state{reply_pid = ReplyPid}) ->
    ReplyPid ! {received_message, Msg},
    {noreply, State};

handle_info(shutdown, State) ->
    {stop, normal, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.
