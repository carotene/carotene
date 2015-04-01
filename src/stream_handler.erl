-module(stream_handler).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-record(state, {
          active,
          token,
          connection,
          conn_ref
         }).

init(_Transport, Req, _Opts, Active) ->

    ConnectionPid = case Active of
                        once ->
                            init_xhr_get(Req);
                        false ->
                            init_xhr_post(Req);
                        true -> 
                            init_long_lived()
                    end,
    {ok, Req, #state{active = Active, token = undefined, connection = ConnectionPid}}.

stream(<<"ping">>, Req, State) ->
    {reply, <<"pong">>, Req, State};
stream(_Data, Req, State = #state{connection = undefined}) ->
    %long polling posting message, but connection is not alive, discard it
    {ok, Req, State};
stream(Data, Req, State = #state{active = false, connection = Connection}) ->
    gen_server:cast(Connection, {process_message, Data}),
    {ok, Req, State};
stream(Data, Req, State = #state{connection = Connection}) ->
    gen_server:cast(Connection, {process_message, Data}),
    {ok, Req, State}.

info({list, Msgs}, Req, State) ->
    MsgList = erlang:iolist_to_binary([<<"[">>] ++ intersperse(<<",">>, Msgs) ++ [<<"]">>]),
    {reply, MsgList, Req, State};
info({text, Msg}, Req, State) ->
    {reply, Msg, Req, State};
info({send_token, Token}, Req, State) ->
    Req2 = cowboy_req:set_resp_header(<<"connection-id">>, Token, Req),
    Msg = "connection-id",
    {reply, Msg, Req2, State#state{token = Token}};
info({'DOWN', _Ref, process, _Pid, _Reason}, Req, State) ->
    CPid = create_connection(intermittent),
    {ok, Req, State#state{connection = CPid}};
info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Req, #state{connection = ConnectionPid, active = once, conn_ref = ConnRef}) ->
    case ConnRef of
        undefined -> ok;
        _ -> erlang:demonitor(ConnRef)
    end,

    ConnectionPid ! transport_hiatus,
    ok;
terminate(_Req, _State) ->
    ok.

%%
% Internal functions
%%

create_connection(intermittent) ->
    Token = list_to_binary(uuid:to_string(uuid:uuid4())),
    {ok, CPid} = carotene_connection_sup:start_connection(self(), intermittent, Token),
    self() ! {send_token, Token},
    erlang:monitor(process, CPid),
    CPid;
create_connection(permanent) ->
    {ok, CPid} = carotene_connection_sup:start_connection(self(), permanent, undefined),
    CPid.

init_xhr_get(Req) ->
    case cowboy_req:header(<<"connection-id">>, Req) of
        {undefined, _Req2} ->
            create_connection(intermittent);
        {ConnId, _Req2} -> 
            case ets:lookup(carotene_conn_bytok, ConnId) of
                [{_, Conn}] -> 
                    case is_process_alive(Conn) of
                        true ->
                            gen_server:cast(Conn, {keepalive, self()}),
                            Conn;
                        false ->
                            create_connection(intermittent)
                    end;
                [] ->
                    create_connection(intermittent)
            end
    end.

init_xhr_post(Req) ->
    case cowboy_req:header(<<"connection-id">>, Req) of
        {undefined, _Req2} ->
            undefined;
        {ConnId, _Req2} -> 
            case ets:lookup(carotene_conn_bytok, ConnId) of
                [{_, Conn}] ->
                    case is_process_alive(Conn) of
                        true ->
                            Conn;
                        false ->
                            undefined
                    end;
                [] -> 
                    undefined
            end
    end.

init_long_lived() ->
    create_connection(permanent).

intersperse(_, []) ->
  [];
intersperse(_, [X]) ->
  [X];
intersperse(Sep, [X | Xs]) ->
  [X, Sep | intersperse(Sep, Xs)].
