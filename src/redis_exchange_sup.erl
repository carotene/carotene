-module(redis_exchange_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Channel) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Channel]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Channel]) ->
    {ok, { {simple_one_for_one, 5, 10}, [
             {redis_exchange,
              {redis_exchange, start_link, [Channel]},
              permanent,
              infinity,
              worker,
              [redis_exchange] 
             } ]} }.


