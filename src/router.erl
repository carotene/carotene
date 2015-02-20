-module(router).

-behaviour(gen_server).

-export([start/0, start_link/0]).
-export([stop/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).


start_link() ->
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], Opts).

start() ->
    Opts = [],
    gen_server:start({local, ?MODULE}, ?MODULE, [], Opts).

init([]) ->
    ets:new(router_subscribers, [bag, private, named_table]),
    {ok, s}.

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_cast({publish, Message, channel, Channel}, State) ->
    Subs = ets:lookup_element(router_subscribers, Channel, 2),
    broadcast({received_message, Message}, Subs),
    {noreply, State};

handle_cast({subscribe, Channel, from, ReplyTo}, State) ->
    ets:insert(router_subscribers, {Channel, ReplyTo}),
    {noreply, State};

handle_cast({unsubscribe, Channel, from, ReplyTo}, State) ->
    ets:delete(router_subscribers, {Channel, ReplyTo}),
    {noreply, State};

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

broadcast(Msg, [Pid|Pids]) ->
    Pid ! Msg,
    broadcast(Msg, Pids);
broadcast(_, []) ->
    true.
