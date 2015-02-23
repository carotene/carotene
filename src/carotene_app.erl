-module(carotene_app).

-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(application).

-export([start/0, stop/0, join_cluster/1, cluster_status/0]).
%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
    carotene_sup:start_link().

stop(_State) ->
    ok.

stop() ->
    application:stop(ranch),
    application:stop(cowlib),
    application:stop(crypto),
    application:stop(cowboy),
    application:stop(rabbit_common),
    application:stop(amqp_client),
    application:stop(mnesia),
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
    application:start(mnesia),
    application:start(inets),
    application:start(carotene).

join_cluster(Node) ->
    net_kernel:connect_node(Node),
    ok.

cluster_status() ->
    [node()| nodes()].
