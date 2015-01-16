-module(redis_smart_sub).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {client, subscriptions}).

start_link(Client) ->
    gen_server:start_link(?MODULE, [Client], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init([Client]) ->
    eredis_sub:controlling_process(Client),
    State = #state{client = Client, subscriptions = dict:new()},
    {ok, State}.

handle_info(stop, State) ->
    {stop, shutdown, State};

handle_info({subscribed, Channel, Pid}, State) ->
    eredis_sub:ack_message(Pid),
    {noreply, State};
handle_info({message, Channel, Msg, Pid}, #state{client = Client, subscriptions = Subscriptions} = State) ->
    eredis_sub:ack_message(Pid),
    case dict:find(Channel, Subscriptions) of
        error -> ok;
        {ok, Subscribed} -> [ReplyTo ! {received_message, Msg} || ReplyTo <- Subscribed]
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.

handle_cast({subscribe, Channels, From}, #state{client = Client, subscriptions = Subscriptions} = State) ->
    SubFun = fun(Channel, Subs) ->
                     case dict:find(Channel, Subscriptions) of
                         error -> 
                             eredis_sub:subscribe(Client, [Channel]);
                         _ -> ok
                     end,
                     dict:append(Channel, From, Subscriptions)
             end,
    NewSubscriptions = lists:foldl(SubFun, Subscriptions, Channels),
    {noreply, State#state{subscriptions = NewSubscriptions}}.

%handle_cast({unsubscribe, Channels, From}, _From, #state{subscriptions = Subscriptions} = State) ->
%    case dict:find(Channel, Subscriptions) of
%        error -> _;
%        {ok, Processes} -> 
%             eredis_sub:subscribe(Client, Channels);
%    end,
%    dict:append(Channel, From, Subscriptions),
%    {noreply, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
