-module(presence_serv).

-behaviour(gen_server).

-export([start_link/0]).
-export([stop/1]).
%-export([presence/1, get_exchanges/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-record(state, {pid_exchange_user_list_pub, refs_pub, pid_exchange_user_list_sub, refs_sub}).
-record(subscriber, {{node, pid}, exchange_name, user_id}).
-record(publisher, {{node, pid}, exchange_name, user_id}).


start_link() ->
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([]) ->
    Nodes = [node()],
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(subscribers,
                        [{attributes, record_info(fields, subscriber)},
                         {disc_copies, Nodes}),
    mnesia:create_table(publishers,
                        [{attributes, record_info(fields, publisher)},
                         {disc_copies, Nodes}]),
    rpc:multicall(Nodes, application, stop, [mnesia]),
    State = #state{pid_exchange_user_list_pub = [], pid_exchange_user_list_sub = [], refs_pub=dict:new(), refs_sub=dict:new()},
    {ok, State}.


%handle_info({'DOWN', _Ref, process, Pid, _}, State=#state{pid_exchange_user_list_pub=Peul, refs_pub=Refs, pid_exchange_user_list_sub=PeulSub, refs_sub=RefsSub}) ->
%    NewRefs = case dict:find(Pid, Refs) of
%                  error -> Refs;
%                  {ok, Ref} -> erlang:demonitor(Ref, [flush]),
%                               dict:erase(Pid)
%              end,
%
%    NewPeul = case lists:keyfind(Pid, 1, Peul) of
%                  false -> Peul;
%                  Val -> lists:delete(Val, Peul)
%              end,
%    NewRefsSub = case dict:find(Pid, RefsSub) of
%                  error -> Refs;
%                     {ok, RefSub} -> erlang:demonitor(RefSub, [flush]),
%                                     dict:erase(Pid)
%              end,
%
%    NewPeulSub = case lists:keyfind(Pid, 1, PeulSub) of
%                  false -> Peul;
%                  ValSub -> lists:delete(ValSub, PeulSub)
%              end,
%
%    {noreply, State#state{pid_exchange_user_list_pub=NewPeul, refs_pub=NewRefs, pid_exchange_user_list_sub=NewPeulSub, refs_sub=NewRefsSub}};
handle_info(shutdown, State) ->
    {stop, shutdown, State}.

%handle_call({presence, ExchangeName}, _From, State=#state{pid_exchange_user_list_pub=Peul, pid_exchange_user_list_sub=PeulSub}) ->
%    PeulFound = lists:filter(fun({_Pid, ExchangeName2, _UserId}) ->
%                                     ExchangeName =:= ExchangeName2
%                             end, Peul),
%    UsersDup = lists:map(fun({_, _, User}) -> User end, PeulFound),
%    Users = sets:to_list(sets:from_list(UsersDup)),
%
%    PeulFoundSub = lists:filter(fun({_Pid, ExchangeName2, _UserId}) ->
%                                     ExchangeName =:= ExchangeName2
%                             end, PeulSub),
%    UsersDupSub = lists:map(fun({_, _, User}) -> User end, PeulFoundSub),
%    UsersSub = sets:to_list(sets:from_list(UsersDupSub)),
%
%    {reply, {Users, UsersSub}, State};
%
%handle_call({get_exchanges, UserId}, _From, State=#state{pid_exchange_user_list_pub=Peul, pid_exchange_user_list_sub=PeulSub}) ->
%    PeulFound = lists:filter(fun({_Pid, _ExchangeName, UserId2}) ->
%                                     UserId =:= UserId2
%                             end, Peul),
%    ExsDup = lists:map(fun({_, Exch, _}) -> Exch end, PeulFound),
%    Exs = sets:to_list(sets:from_list(ExsDup)),
%
%    PeulFoundSub = lists:filter(fun({_Pid, _ExchangeName, UserId2}) ->
%                                     UserId =:= UserId2
%                             end, PeulSub),
%    ExsDupSub = lists:map(fun({_, Exch, _}) -> Exch end, PeulFoundSub),
%    ExsSub = sets:to_list(sets:from_list(ExsDupSub)),
%    {reply, {Exs, ExsSub}, State}.
 
handle_cast({join_exchange, UserId, ExchangeName, From}, State=#state{refs_pub=Refs}) ->
    Ref = erlang:monitor(process, From),
    NewRefs = dict:store(From, Ref, Refs),
    F = fun() ->
                mnesia:write(#publisher{node=node(),
                                        pid=From,
                                        exchange_name=ExchangeName,
                                        user_id=UserId})
        end,
    mnesia:activity(transaction, F).
    {noreply, State#state{refs_pub=NewRefs}};

handle_cast({leave_exchange, UserId, ExchangeName, From}, State=#state{pid_exchange_user_list_pub=Peul, refs_pub=Refs}) ->
%    NewPeul = lists:delete({From, ExchangeName, UserId}, Peul),
%    NewRefs = case dict:find(From, Refs) of
%        error -> Refs;
%        {ok, Ref} -> erlang:demonitor(Ref, [flush]),
%                     dict:erase(From, Refs)
%    end,
%    {noreply, State#state{pid_exchange_user_list_pub=NewPeul, refs_pub=NewRefs}};
%
%handle_cast({subscribe_exchange, UserId, ExchangeName, From}, State=#state{pid_exchange_user_list_sub=Peul, refs_sub=Refs}) ->
%    Ref = erlang:monitor(process, From),
%    NewRefs = dict:store(From, Ref, Refs),
%
%    NewPeul = [{From, ExchangeName, UserId}|Peul],
%    {noreply, State#state{pid_exchange_user_list_sub=NewPeul, refs_pub=NewRefs}};
%
%handle_cast({unsubscribe_exchange, UserId, ExchangeName, From}, State=#state{pid_exchange_user_list_sub=Peul, refs_sub=Refs}) ->
%    NewPeul = lists:delete({From, ExchangeName, UserId}, Peul),
%    NewRefs = case dict:find(From, Refs) of
%        error -> Refs;
%        {ok, Ref} -> erlang:demonitor(Ref, [flush]),
%                     dict:erase(From, Refs)
%    end,
%    {noreply, State#state{pid_exchange_user_list_sub=NewPeul, refs_sub=NewRefs}}.
%
%terminate(_Reason, _State) ->
%    ok.
%
%code_change(_OldVsn, State, _Extra) ->
%    {ok, State}.
%
%presence(ExchangeName) ->
%    gen_server:call(?MODULE, {presence, ExchangeName}).
%
%get_exchanges(UserId) ->
%    gen_server:call(?MODULE, {get_exchanges, UserId}).
