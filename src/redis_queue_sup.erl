-module(redis_queue_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Client) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Client]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Client]) ->
    {ok, { {simple_one_for_one, 5, 10}, [
             {redis_queue,
              {redis_queue, start_link, [Client]},
              temporary,
              infinity,
              worker,
              [redis_queue] 
             } ]} }.
