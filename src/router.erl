-module(router).

-behaviour(gen_server).

-export([start/0, start_link/0]).
-export([stop/1]).
-export([local_presence/1]).
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

handle_call({local_presence, Channel}, _From, State) ->
    UsersWithDupes = try ets:lookup_element(router_subscribers, Channel, 3) of
               Users -> Users
           catch _:_ -> []
           end,
    {reply, UsersWithDupes, State}.

handle_cast({publish, Message, channel, Channel}, State) ->
    io:format("channel: ~p~n", [Channel]),
    Subs = try ets:lookup_element(router_subscribers, Channel, 2) of
               Subs1 -> Subs1
           catch _:_ -> []
           end,

    broadcast({received_message, Message}, Subs),
    broadcast_cluster({cluster_publish, Message, channel, Channel}, nodes()),
    {noreply, State};

handle_cast({cluster_publish, Message, channel, Channel}, State) ->
    Subs = try ets:lookup_element(router_subscribers, Channel, 2) of
               Subs1 -> Subs1
           catch _:_ -> []
           end,
    broadcast({received_message, Message}, Subs),
    {noreply, State};

handle_cast({subscribe, Channel, from, ReplyTo, user_id, UserId}, State) ->
    ets:insert(router_subscribers, {Channel, ReplyTo, UserId}),
    {noreply, State};

handle_cast({unsubscribe, Channel, from, ReplyTo, user_id, UserId}, State) ->
    ets:delete_object(router_subscribers, {Channel, ReplyTo, UserId}),
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

broadcast_cluster(Msg, [Node|Nodes]) ->
    gen_server:cast({router, Node}, Msg),
    broadcast_cluster(Msg, Nodes);
broadcast_cluster(_, []) ->
    true.

local_presence(Channel) ->
    gen_server:call(?MODULE, {local_presence, Channel}).
