-module(subscriber_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [
             {subscriber,
              {subscriber, start_link, []},
              temporary,
              infinity,
              worker,
              [mgs_queue_serv] 
             } ]} }.
