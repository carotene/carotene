-module(redis_queue).

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

init([Channel1]) ->
    {ok, Channel} = eredis_sub:start_link(),
    {ok, #state{channel = Channel}}.

handle_info({subscribed, Exchange, Pid}, State) ->
    io:format("Really subscribed ~p~n", [Exchange]),
    eredis_sub:ack_message(Pid),
    {noreply, State};
handle_info({message, _, Msg, Pid}, #state{reply_pid = ReplyPid} = State) ->
    io:format("message received~p~n", [Msg]),
    io:format("replying to~p~n", [ReplyPid]),

    eredis_sub:ack_message(Pid),
    ReplyPid ! {received_message, Msg},
    {noreply, State};
handle_info(shutdown, State) ->
    {stop, normal, State}.

handle_call({message, _, Msg, Pid}, _From, #state{reply_pid = ReplyPid} = State) ->
    io:format("message received~n"),
    eredis:ack_message(Pid),
    ReplyPid ! {received_message, Msg},
    {noreply, State};
handle_call({declare_queue}, _From, State = #state{channel = Channel}) ->
    {reply, {ok, dummy}, State};
handle_call({queue_bind, Queue, Exchange}, _From, State = #state{channel = Channel1}) ->
    {ok, Channel} = eredis_sub:start_link(),
    eredis_sub:controlling_process(Channel),
    io:format("Subscribed to ~p~n", [Exchange]),
    eredis_sub:subscribe(Channel, [Exchange]),
    {reply, ok, State#state{ exchange = Exchange}};
handle_call({consume, Queue, ReplyPid}, _From, State) ->
    {reply, ok, State#state{reply_pid = ReplyPid}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.
