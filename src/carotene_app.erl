-module(carotene_app).

-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    carotene_sup:start_link().

stop(_State) ->
    ok.
