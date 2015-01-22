-module(redis_queue).

-behaviour(gen_server).
-export([start_link/1, start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

-record(state, {client, channel, reply_pid}).

start_link(Client) ->
    Opts = [],
    gen_server:start_link(?MODULE, [Client], Opts).

start(Client) ->
    Opts = [],
    gen_server:start(?MODULE, [Client], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([Client]) ->
    {ok, #state{client = Client}}.

handle_info({message, Msg}, #state{reply_pid = ReplyPid} = State) ->
    ReplyPid ! {received_message, Msg},
    {noreply, State};
handle_info(shutdown, State) ->
    {stop, normal, State}.

handle_call({message, _, Msg, Pid}, _From, #state{reply_pid = ReplyPid} = State) ->
    ReplyPid ! {received_message, Msg},
    {noreply, State};
handle_call({declare_queue}, _From, State = #state{client = Client}) ->
    {reply, {ok, dummy}, State};
handle_call({queue_bind, Queue, Channel}, _From, State = #state{client = Client}) ->
    gen_server:cast(Client, {subscribe, [Channel], self()}),
    {reply, ok, State#state{ channel = Channel}};
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
