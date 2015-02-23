-module(carotene_control).

-export([start/0, stop/0]).

start() ->
    OptSpecList =
    [
     {nodename, $n, "nodename", {string, undefined}, "Node name"},
     {nodetojoin, $j, "node to join", {string, undefined}, "Node to join"},
     {help, $h, undefined, undefined, "Show this help"}
    ],
    parse_arguments(OptSpecList).

stop() ->
    ok.

parse_arguments(OptSpecList) ->
    case getopt:parse(OptSpecList, init:get_plain_arguments()) of
        {ok, {ParsedArgs, ExtraArgs}} -> 
            case lists:member(help, ParsedArgs) of
                            false -> maybe_start(ParsedArgs, ExtraArgs);
                            true -> getopt:usage(OptSpecList, "carotene_control")
                        end;
        _ -> getopt:usage(OptSpecList, "carotene_control")
    end,
    init:stop(0).

maybe_start(Args, Extra) ->
    case lists:member("start", Extra) of
        true -> case lists:keyfind(nodename, 1, Args) of 
                    {nodename, Node} -> rpc:call(list_to_atom(Node), carotene_app, start, []);
                    false -> io:format("nodename option required (run with -h for help)~n")
                end;
        false -> maybe_stop(Args, Extra) 
    end.

maybe_stop(Args, Extra) ->
    case lists:member("stop", Extra) of
        true -> case lists:keyfind(nodename, 1, Args) of 
                    {nodename, Node} -> rpc:call(list_to_atom(Node), carotene_app, stop, []);
                    false -> io:format("nodename option required (run with -h for help)~n")
                end;
        false -> maybe_join_cluster(Args, Extra) 
    end.

maybe_join_cluster(Args, Extra) ->
    case lists:member("join_cluster", Extra) of
        true -> case lists:keyfind(nodename, 1, Args) of 
                    {nodename, Node} -> 
                        case lists:keyfind(nodetojoin, 1, Args) of
                            {nodetojoin, Node2} -> io:format("~p ~n", [rpc:call(list_to_atom(Node), carotene_app, join_cluster, [list_to_atom(Node2)])]);
                            false -> io:format("--nodetojoin option required (run with -h for help)~n")
                        end;
                    false -> io:format("nodename option required (run with -h for help)~n")
                end;
        false -> ok%maybe_cluster_status(Args, Extra) 
    end.
