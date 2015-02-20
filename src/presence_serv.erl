-module(presence_serv).

-behaviour(gen_server).

-export([start_link/0]).
-export([stop/1]).
-export([presence/1, get_channels/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-record(state, {refs_sub}).
-record(subscribers, {pid, channel, user_id}).


start_link() ->
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([]) ->
    Nodes = [node()],
    mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(subscribers,
                        [{attributes, record_info(fields, subscribers)},
                         {ram_copies, Nodes}]),
    State = #state{refs_sub=dict:new()},
    {ok, State}.


handle_info({'DOWN', _Ref, process, Pid, _}, State=#state{refs_sub=RefsSub}) ->

    NewRefsSub = case dict:find(Pid, RefsSub) of
                  error -> RefsSub;
                     {ok, RefSub} -> erlang:demonitor(RefSub, [flush]),
                                     dict:erase(Pid, RefsSub)
              end,

    F2 = fun() -> mnesia:delete({subscribers, Pid}) end,
    mnesia:activity(transaction, F2),

    {noreply, State#state{refs_sub=NewRefsSub}};

handle_info(shutdown, State) ->
    {stop, shutdown, State}.

handle_call({presence, Channel}, _From, State) ->
    PatternSub = #subscribers{_ = '_', channel = Channel},
    FSub = fun() ->
                Res = mnesia:match_object(PatternSub),
                [UserId || #subscribers{user_id=UserId} <- Res]
        end,
    UsersDupSub =mnesia:activity(transaction, FSub),
    UsersSub = sets:to_list(sets:from_list(UsersDupSub)),

    {reply, {UsersSub}, State};

handle_call({get_channels, UserId}, _From, State) ->
    PatternSub = #subscribers{_ = '_', user_id = UserId},
    FSub = fun() ->
                Res = mnesia:match_object(PatternSub),
                [Channel || #subscribers{channel=Channel} <- Res]
        end,
    ChannelsDup =mnesia:activity(transaction, FSub),
    Channels = sets:to_list(sets:from_list(ChannelsDup)),
    {reply, {Channels}, State}.
 
handle_cast({subscribe, UserId, Channel, From}, State=#state{refs_sub=Refs}) ->
    Ref = erlang:monitor(process, From),
    NewRefs = dict:store(From, Ref, Refs),
    F = fun() ->
                mnesia:write(#subscribers{pid=From,
                                          channel=Channel,
                                          user_id=UserId})
        end,
    mnesia:activity(transaction, F),

    {noreply, State#state{refs_sub=NewRefs}};

handle_cast({unsubscribe, _UserId, _Channel, From}, State=#state{refs_sub=Refs}) ->
    NewRefs = case dict:find(From, Refs) of
        error -> Refs;
        {ok, Ref} -> erlang:demonitor(Ref, [flush]),
                     dict:erase(From, Refs)
    end,
    F = fun() -> mnesia:delete({subscribers, From}) end,
    mnesia:activity(transaction, F),
    {noreply, State#state{refs_sub=NewRefs}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

presence(Channel) ->
    gen_server:call(?MODULE, {presence, Channel}).
%
get_channels(UserId) ->
    gen_server:call(?MODULE, {get_channels, UserId}).
