-module(redis_exchange).

-behaviour(gen_server).
-export([start_link/1, start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

-record(state, {client, exchange}).

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

handle_info(shutdown, State) ->
    {stop, normal, State}.

handle_call({declare_exchange, {Exchange, Type}}, _From, State = #state{client = Client}) ->
    {reply, ok, State#state{exchange = Exchange}};

handle_call({publish, Message}, _From, State = #state{client = Client, exchange = Exchange}) ->
    eredis:q(Client, ["PUBLISH", Exchange, Message]),
    {reply, ok, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.
