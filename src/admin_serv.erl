-module(admin_serv).

-behaviour(gen_server).

-export([start_link/0]).
-export([stop/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-record(state, {exchanges, queues}).

start_link() ->
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([]) ->
    State = #state{exchanges = dict:new(), queues = dict:new()},
    {ok, State}.

handle_call({join_exchange, ExchangeName}, _From, State=#state{exchanges=Exs, queues=Qs}) ->

    {ok, ExchangePid} = supervisor:start_child(msg_exchange_sup, [ExchangeName, admin, self()]),
    {ok, QueuePid} = supervisor:start_child(msg_queue_sup, [ExchangeName, admin, self()]),
    % TODO: add only once
    NewState = State#state{exchanges = dict:append(ExchangeName, ExchangePid, Exs), queues = dict:append(ExchangeName, QueuePid, Qs)},
    {reply, ok, NewState};

handle_call(get_exchanges, _From, State=#state{exchanges=Exs}) ->
    {reply, {ok, dict:fetch_keys(Exs)}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(stop, State) ->
    {stop, shutdown, State};

handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
