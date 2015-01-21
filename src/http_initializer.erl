-module(http_initializer).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

-record(state, {supervisor}).

start_link(Sup) ->
    Opts = [],
    gen_server:start(?MODULE, [Sup], Opts).

init([Sup]) ->
    self() ! {start_broker, Sup},
    {ok, #state{supervisor = Sup}}.

handle_call(alloc, _From, State) ->
    {reply, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({start_broker, Sup}, State = #state{supervisor = Sup}) ->
    inets:start(),
    %TODO: monitor broker instead of making it permanent so we update the ref
    {ok, _} = supervisor:start_child(Sup, {broker,
         {broker_sup, start_link, []},
          permanent,
          infinity,
          worker,
          [broker_sup]}),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", ws_handler, ["http://carotene_test.lo/app_dev.php/authenticate"]}
        ]}
    ]),
    {ok, Port} = application:get_env(carotene, http_port),
    {ok, _} = cowboy:start_http(http, 1, [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]),
    {noreply, State};
handle_info(shutdown, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    inets:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
