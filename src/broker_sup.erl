-module(broker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, get_broker/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
             {broker,
              {rabbitmq_broker, start_link, []},
              permanent,
              infinity,
              worker,
              [rabbitmq_broker] 
             } ]} }.

get_broker() ->
    Children = supervisor:which_children(?MODULE),
    io:format("~p, ~n", Children),
    {broker, Broker, _, _} = lists:keyfind(broker, 1, Children),
    Broker.

