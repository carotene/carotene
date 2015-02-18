-module(rabbitmq_exchange_sup).

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
             {rabbitmq_exchange,
              {rabbitmq_exchange, start_link, [Channel]},
              temporary,
              infinity,
              worker,
              [rabbitmq_exchange] 
             } ]} }.


