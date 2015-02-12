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

handle_call({publish, {exchange_name, ExchangeName}, {message, Message}}, _From, State=#state{exchanges=Exs}) ->
    Payload = jsx:encode([{<<"message">>, Message},
                          {<<"exchange">>, ExchangeName}, 
                          {<<"from_server">>, <<"true">>}
                         ]),
    NewExs = case dict:find(ExchangeName, Exs) of
                 error -> {BrokerModule, Broker} = broker_sup:get_broker(),
                          {ok, Exchange} = apply(BrokerModule, start_exchange, [Broker]),
                          ok = apply(BrokerModule, declare_exchange, [Exchange, {ExchangeName, <<"fanout">>}]),
                          gen_server:call(Exchange, {publish, Payload}),
                          dict:store(ExchangeName, Exchange, Exs);
                 {ok, Exchange} -> gen_server:call(Exchange, {publish,  Payload}),
                                   Exs
             end,

    {reply, ok, State#state{exchanges = NewExs}};

handle_call({subscribe, {exchange_name, ExchangeName}}, _From, State=#state{queues=Qs}) ->
    subscribe(ExchangeName),
    NewQs = case lists:member(ExchangeName, Qs) of
                true -> Qs;
                false -> lists:append(ExchangeName, Qs)
            end,
    {reply, ok, State#state{queues = NewQs}};

handle_call(get_subscribed, _From, State=#state{queues=Qs}) ->
    {reply, {ok, Qs}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({received_message, Msg, exchange, ExchangeName}, State) ->
    {ok, Url} = application:get_env(carotene, subscribe_url),
    httpc:request(post, {Url, [], "application/x-www-form-urlencoded", "nmessage="++binary_to_list(Msg)++"&exchange="++binary_to_list(ExchangeName)}, [], []),
    {noreply, State};

handle_info(stop, State) ->
    {stop, shutdown, State};

handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

subscribe(ExchangeName) ->
    {_BrokerModule, Broker} = broker_sup:get_broker(),
    % TODO: This is particular to rabbitmq
    {ok, QueueServer} = gen_server:call(Broker, start_queue),
    {ok, Queue} = gen_server:call(QueueServer, {declare_queue}),
    ok = gen_server:call(QueueServer, {queue_bind, Queue, ExchangeName}),
    ok = gen_server:call(QueueServer, {consume, Queue, self()}).
