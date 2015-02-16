-module(presence_serv).

-behaviour(gen_server).

-export([start_link/0]).
-export([stop/1]).
-export([presence/1, get_exchanges/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-record(state, {refs_pub, refs_sub}).
-record(subscribers, {pid, exchange_name, user_id}).
-record(publishers, {pid, exchange_name, user_id}).


start_link() ->
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([]) ->
    Nodes = [node()],
    %ok = mnesia:create_schema(Nodes),
    
    application:start(mnesia),
%    mnesia:set_debug_level(debug),
    
    mnesia:create_table(subscribers,
                 [{attributes, record_info(fields, subscribers)},
                  {disc_copies, Nodes}]),
    mnesia:create_table(publishers,
                        [{attributes, record_info(fields, publishers)},
                         {disc_copies, Nodes}]),
    application:stop(mnesia),
    application:start(mnesia),
    State = #state{refs_pub=dict:new(), refs_sub=dict:new()},
    {ok, State}.


handle_info({'DOWN', _Ref, process, Pid, _}, State=#state{refs_pub=Refs, refs_sub=RefsSub}) ->
    NewRefs = case dict:find(Pid, Refs) of
                  error -> Refs;
                  {ok, Ref} -> erlang:demonitor(Ref, [flush]),
                               dict:erase(Pid)
              end,

    mnesia:delete(publishers, #publishers{pid=Pid}),
    NewRefsSub = case dict:find(Pid, RefsSub) of
                  error -> Refs;
                     {ok, RefSub} -> erlang:demonitor(RefSub, [flush]),
                                     dict:erase(Pid)
              end,

    mnesia:delete(subscribers, #publishers{pid=Pid}),

    {noreply, State#state{refs_pub=NewRefs, refs_sub=NewRefsSub}};

handle_info(shutdown, State) ->
    {stop, shutdown, State}.

handle_call({presence, ExchangeName}, _From, State) ->
    PatternPub = #publishers{_ = '_', exchange_name = ExchangeName},
    F = fun() ->
                Res = mnesia:match_object(PatternPub),
                [UserId || #publishers{user_id=UserId} <- Res]
        end,
    UsersDupPub =mnesia:activity(transaction, F),
    UsersPub = sets:to_list(sets:from_list(UsersDupPub)),
    PatternSub = #subscribers{_ = '_', exchange_name = ExchangeName},
    FSub = fun() ->
                Res = mnesia:match_object(PatternSub),
                [UserId || #subscribers{user_id=UserId} <- Res]
        end,
    UsersDupSub =mnesia:activity(transaction, FSub),
    UsersSub = sets:to_list(sets:from_list(UsersDupSub)),

    {reply, {UsersPub, UsersSub}, State};

handle_call({get_exchanges, UserId}, _From, State) ->
    PatternPub = #publishers{_ = '_', user_id = UserId},
    F = fun() ->
                Res = mnesia:match_object(PatternPub),
                [ExchangeName || #publishers{exchange_name=ExchangeName} <- Res]
        end,
    ExchangesDupPub =mnesia:activity(transaction, F),
    ExchangesPub = sets:to_list(sets:from_list(ExchangesDupPub)),
    PatternSub = #subscribers{_ = '_', user_id = UserId},
    FSub = fun() ->
                Res = mnesia:match_object(PatternSub),
                [ExchangeName || #subscribers{exchange_name=ExchangeName} <- Res]
        end,
    ExchangesDupSub =mnesia:activity(transaction, FSub),
    ExchangesSub = sets:to_list(sets:from_list(ExchangesDupSub)),
    {reply, {ExchangesPub, ExchangesSub}, State}.
 
handle_cast({join_exchange, UserId, ExchangeName, From}, State=#state{refs_pub=Refs}) ->
    Ref = erlang:monitor(process, From),
    NewRefs = dict:store(From, Ref, Refs),
    F = fun() ->
                mnesia:write(#publishers{ pid=From,
                                        exchange_name=ExchangeName,
                                        user_id=UserId})
        end,
    mnesia:activity(transaction, F),
    {noreply, State#state{refs_pub=NewRefs}};

handle_cast({leave_exchange, UserId, ExchangeName, From}, State=#state{refs_pub=Refs}) ->
    mnesia:delete(publishers, #publishers{pid=From,
                                                   exchange_name=ExchangeName,
                                                   user_id=UserId}),
    NewRefs = case dict:find(From, Refs) of
        error -> Refs;
        {ok, Ref} -> erlang:demonitor(Ref, [flush]),
                     dict:erase(From, Refs)
    end,
    {noreply, State#state{refs_pub=NewRefs}};

handle_cast({subscribe_exchange, UserId, ExchangeName, From}, State=#state{refs_sub=Refs}) ->
    Ref = erlang:monitor(process, From),
    NewRefs = dict:store(From, Ref, Refs),
    F = fun() ->
                mnesia:write(#subscribers{pid=From,
                                          exchange_name=ExchangeName,
                                          user_id=UserId})
        end,
    mnesia:activity(transaction, F),

    {noreply, State#state{refs_pub=NewRefs}};

handle_cast({unsubscribe_exchange, UserId, ExchangeName, From}, State=#state{refs_sub=Refs}) ->
    NewRefs = case dict:find(From, Refs) of
        error -> Refs;
        {ok, Ref} -> erlang:demonitor(Ref, [flush]),
                     dict:erase(From, Refs)
    end,
    mnesia:delete(publishers, #subscribers{pid=From,
                                           exchange_name=ExchangeName,
                                           user_id=UserId}),
    {noreply, State#state{refs_sub=NewRefs}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

presence(ExchangeName) ->
    gen_server:call(?MODULE, {presence, ExchangeName}).
%
get_exchanges(UserId) ->
    gen_server:call(?MODULE, {get_exchanges, UserId}).
