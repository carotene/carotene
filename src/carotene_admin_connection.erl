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
    State = #state{subscribers = []},
    self() ! subscribe_to_channels,
    {ok, State}.

handle_call({publish, {channel, Channel}, {message, Message}}, _From, State) ->
    Payload = jsx:encode([{<<"message">>, Message},
                          {<<"channel">>, Channel}, 
                          {<<"from_server">>, <<"true">>}
                         ]),
    carotene_router:publish(Payload, Channel),

    {reply, ok, State};

handle_call(get_subscribed, _From, State=#state{subscribers=Subs}) ->
    {reply, {ok, Subs}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({received_message, Msg, channel, Channel}, State) ->
    case application:get_env(carotene, subscribe_url) of
        {ok, Url} -> httpc:request(post, {Url, [], "application/x-www-form-urlencoded", "nmessage="++binary_to_list(Msg)++"&channel="++binary_to_list(Channel)}, [], []);
        % TODO: log this
        _ -> ok
    end,
    {noreply, State};

handle_info(subscribe_to_channels, State = #state{subscribers=Subs}) ->
    NewSubs = case application:get_env(carotene, subscribed_channels) of
               {ok, Channels} -> subscribe(Channels, Subs);
               _ -> Subs
           end,
    {noreply, State#state{subscribers = NewSubs}};

handle_info(stop, State) ->
    {stop, shutdown, State};

handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.

terminate(_Reason, #state{subscribers=Subs}) ->
    carotene_router:unsubscribe_channels(Subs, self(), server). 

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

subscribe([], Subs) -> Subs;
subscribe([Channel|Channels], Subs) ->
    BinaryChannel = case is_list(Channel) of
                        true -> list_to_binary(Channel);
                        false -> Channel
                    end,
    NewSubs = case lists:member(BinaryChannel, Subs) of
                  true -> Subs;
                  false -> carotene_router:subscribe(BinaryChannel, self(), server),
                           [BinaryChannel| Subs]
              end,
    subscribe(Channels, NewSubs).
