-module(carotene_app).

-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(application).

-export([start/0, stop/0, join_cluster/1, cluster_status/0, join_cluster_nodes/0]).
%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
    Res = carotene_sup:start_link(),
    join_cluster_nodes(),
    Res.


stop(_State) ->
    ok.

stop() ->
    application:stop(ranch),
    application:stop(cowlib),
    application:stop(crypto),
    application:stop(cowboy),
    application:stop(rabbit_common),
    application:stop(amqp_client),
    application:stop(inets),
    application:stop(carotene),
    ok.

start() ->
    application:start(ranch),
    application:start(crypto),
    application:start(cowlib),
    application:start(cowboy),
    application:start(rabbit_common),
    application:start(amqp_client),
    application:start(inets),
    application:start(carotene).

join_cluster_nodes([]) -> ok;
join_cluster_nodes([Node|Nodes]) ->
    join_cluster(Node),
    join_cluster_nodes(Nodes).

join_cluster_nodes() ->
    case application:get_env(carotene, nodes_in_cluster) of
        {ok, Nodes} ->
            join_cluster_nodes(Nodes);
        _ -> 
            % not a cluster
            ok
    end,
    ok.

join_cluster(Node) ->
    MyNode = node(),
    case Node of
        MyNode -> 
            ok;
        _ ->
            net_kernel:connect_node(Node),
            ok
    end.

cluster_status() ->
    [node()| nodes()].
