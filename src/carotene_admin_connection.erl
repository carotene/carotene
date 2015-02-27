-module(carotene_admin_connection).

-behaviour(gen_server).

-export([start_link/0, start/0]).
-export([stop/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-record(state, {subscribers, broker}).

start_link() ->
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], Opts).

start() ->
    Opts = [],
    gen_server:start({local, ?MODULE}, ?MODULE, [], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([]) ->
    State = #state{subscribers = dict:new()},
    {ok, State}.

handle_call({publish, {channel, Channel}, {message, Message}}, _From, State) ->
    Payload = jsx:encode([{<<"message">>, Message},
                          {<<"channel">>, Channel}, 
                          {<<"from_server">>, <<"true">>}
                         ]),
    router:publish(Payload, Channel),

    {reply, ok, State};

handle_call({subscribe, {channel, Channel}}, _From, State=#state{subscribers=Subs}) ->
    subscribe(Channel),
    NewSubs = case lists:member(Channel, Subs) of
                true -> Subs;
                false -> lists:append(Channel, Subs)
            end,
    {reply, ok, State#state{subscribers = NewSubs}};

handle_call(get_subscribed, _From, State=#state{subscribers=Subs}) ->
    Channels = dict:fetch_keys(Subs),
    {reply, {ok, Channels}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({received_message, Msg, channel, Channel}, State) ->
    {ok, Url} = application:get_env(carotene, subscribe_url),
    httpc:request(post, {Url, [], "application/x-www-form-urlencoded", "nmessage="++binary_to_list(Msg)++"&channel="++binary_to_list(Channel)}, [], []),
    {noreply, State};

handle_info(stop, State) ->
    {stop, shutdown, State};

handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.

terminate(_Reason, State=#state{subscribers=Subs}) ->
    Channels = dict:fetch_keys(Subs),
    router:unsubscribe_channels(Channels, self(), server). 

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

subscribe(Channel) ->
    router:subscribe(Channel, self(), server).
