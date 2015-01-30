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
    % TODO error handling
    {ok, Exchange} = dict:find(ExchangeName, Exs),
    ok = gen_server:call(Exchange, {publish,  Message}),
    {reply, ok, State};

handle_call({create_exchange, {exchange_name, ExchangeName}, {subscribe, SaneSubscribe}, {publish, SanePublish}, {subscribe_callback_url, Url}}, _From, State=#state{exchanges=Exs, queues=Qs}) ->

    NewQs = case SaneSubscribe of
                true -> 
                    subscribe(ExchangeName),
                    dict:store(ExchangeName, Url, Qs);
                false -> Qs
            end,

    NewExs = case SanePublish of
                 true -> {BrokerModule, Broker} = broker_sup:get_broker(),
                         {ok, Exchange} = apply(BrokerModule, start_exchange, [Broker]),
                         ok = apply(BrokerModule, declare_exchange, [Exchange, {ExchangeName, <<"fanout">>}]),
                         case dict:find(ExchangeName, Exs) of
                             error -> dict:store(ExchangeName, Exchange, Exs);
                             {ok, _} -> Exs
                         end;
                 false -> Exs
             end,

    {reply, ok, State#state{exchanges = NewExs, queues = NewQs}};

handle_call(get_exchanges, _From, State=#state{exchanges=Exs}) ->
    {reply, {ok, dict:fetch_keys(Exs)}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({received_message, Msg, exchange, ExchangeName}, State = #state{queues = Qs}) ->
    % TODO robust
    case dict:find(ExchangeName, Qs) of
        error -> error;
        {ok, Url} -> httpc:request(post, {Url, [], "application/x-www-form-urlencoded", "nmessage="++binary_to_list(Msg)++"&exchange="++binary_to_list(ExchangeName)}, [], [])
    end,

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
