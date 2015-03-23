-module(carotene_connection_sup).

%% API.
-export([start_link/0]).
-export([start_connection/3]).

%% Supervisor internals.
-export([init/1]).
-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-record(state, {
          parent = undefined :: pid()
         }).

%% API.

start_link() ->
    {ok, Pid} = proc_lib:start_link(?MODULE, init, [self()]),
    erlang:register(?MODULE, Pid),
    {ok, Pid}.


start_connection(From, Type, Token) ->
    carotene_connection_sup ! {?MODULE, start_connection, From, Type, Token},
    receive Ret -> Ret end.

init(Parent) ->
    ets:new(carotene_conn_bypid, [set, public, named_table]),
    ets:new(carotene_conn_bytok, [set, public, named_table]),
    process_flag(trap_exit, true),
    ok = proc_lib:init_ack(Parent, {ok, self()}),
    loop(#state{parent=Parent}, 0).

loop(State=#state{parent=Parent}, CurConns) ->
    receive
        {?MODULE, start_connection, From, Type, Token} ->
            case carotene_connection:start_link(From, Type) of
                {ok, Pid} ->
                    From ! {ok, Pid},
                    case Type of 
                        intermittent ->
                            ets:insert(carotene_conn_bytok, {Token, Pid}),
                            ets:insert(carotene_conn_bypid, {Pid, Token});
                        _ -> ok
                    end,
                    loop(State, CurConns + 1);
                _ ->
                    From ! self(),
                    loop(State, CurConns)
            end;
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        {'EXIT', Pid, Reason} ->
            report_error(Pid, Reason),
            case ets:lookup(carotene_conn_bypid, Pid) of
                [{_, Token}] ->
                    ets:delete(carotene_conn_bypid, Pid),
                    ets:delete(carotene_conn_bytok, Token);
                [] ->
                    ok
            end,
            loop(State, CurConns - 1);
        {system, From, Msg} ->
            sys:handle_system_msg(Msg, From, Parent, ?MODULE, [],
                                  {State, CurConns});
        {'$gen_call', {To, Tag}, which_children} ->
            Children = ets:tab2list(carotene_conn_bypid),
            To ! {Tag, Children},
            loop(State, CurConns);
        {'$gen_call', {To, Tag}, count_children} ->
            Counts = [{supervisors, 0}, {workers, CurConns}],
            Counts2 = [{specs, 1}, {active, CurConns}|Counts],
            To ! {Tag, Counts2},
            loop(State, CurConns);
        {'$gen_call', {To, Tag}, _} ->
            To ! {Tag, {error, ?MODULE}},
            loop(State, CurConns);
        Msg ->
            error_logger:error_msg(
              "Carotene connection supervisor ~p received unexpected message ~p~n",
              [self(), Msg])
    end.

system_continue(_, _, {State, CurConns}) ->
    loop(State, CurConns).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, _) ->
    exit(Reason).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.

%% We use ~999999p here instead of ~w because the latter doesn't
%% support printable strings.
report_error(_, normal) ->
    ok;
report_error(_, shutdown) ->
    ok;
report_error(_, {shutdown, _}) ->
    ok;
report_error(Ref, Reason) ->
    error_logger:error_msg(
      "Carotene Connection supervisor ~p had connection process started with "
      "~p:start_link/4 at ~p exit with reason: ~999999p~n",
      [Ref, Reason]).
