-module(presence_serv).

-behaviour(gen_server).

-export([start_link/0]).
-export([stop/1]).
-export([presence/1, get_exchanges/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-record(state, {pid_exchange_user_list, refs}).


start_link() ->
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([]) ->
    State = #state{pid_exchange_user_list = [], refs=dict:new()},
    {ok, State}.

handle_info({'DOWN', _Ref, process, Pid, _}, State=#state{pid_exchange_user_list=Peul, refs=Refs}) ->
    NewRefs = case dict:find(Pid, Refs) of
                  error -> Refs;
                  {ok, Ref} -> erlang:demonitor(Ref, [flush]),
                               dict:erase(Pid)
              end,

    NewPeul = case lists:keyfind(Pid, 1, Peul) of
                  false -> Peul;
                  Val -> lists:delete(Val, Peul)
              end,

    {noreply, State#state{pid_exchange_user_list=NewPeul, refs=NewRefs}};
handle_info(shutdown, State) ->
    {stop, shutdown, State}.

handle_call({presence, ExchangeName}, _From, State=#state{pid_exchange_user_list=Peul}) ->
    PeulFound = lists:filter(fun({_Pid, ExchangeName2, _UserId}) ->
                                     ExchangeName =:= ExchangeName2
                             end, Peul),
    UsersDup = lists:map(fun({_, _, User}) -> User end, PeulFound),
    Users = sets:to_list(sets:from_list(UsersDup)),

    {reply, Users, State};

handle_call({get_exchanges, UserId}, _From, State=#state{pid_exchange_user_list=Peul}) ->
    PeulFound = lists:filter(fun({_Pid, _ExchangeName, UserId2}) ->
                                     UserId =:= UserId2
                             end, Peul),
    ExsDup = lists:map(fun({_, Exch, _}) -> Exch end, PeulFound),
    Exs = sets:to_list(sets:from_list(ExsDup)),
    {reply, Exs, State}.

handle_cast({join_exchange, UserId, ExchangeName, From}, State=#state{pid_exchange_user_list=Peul, refs=Refs}) ->
    Ref = erlang:monitor(process, From),
    NewRefs = dict:store(From, Ref, Refs),
    NewPeul = [{From, ExchangeName, UserId}|Peul],
    {noreply, State#state{pid_exchange_user_list=NewPeul, refs=NewRefs}};

handle_cast({leave_exchange, UserId, ExchangeName, From}, State=#state{pid_exchange_user_list=Peul, refs=Refs}) ->
    NewPeul = lists:delete({From, ExchangeName, UserId}, Peul),
    NewRefs = case dict:find(From, Refs) of
        error -> Refs;
        {ok, Ref} -> erlang:demonitor(Ref, [flush]),
                     dict:erase(From, Refs)
    end,
    {noreply, State#state{pid_exchange_user_list=NewPeul, refs=NewRefs}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

presence(ExchangeName) ->
    gen_server:call(?MODULE, {presence, ExchangeName}).

get_exchanges(UserId) ->
    gen_server:call(?MODULE, {get_exchanges, UserId}).
