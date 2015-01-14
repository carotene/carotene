-module(transport_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, get_transport/0]).

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
             {transport,
              {rabbitmq_transport, start_link, []},
              permanent,
              infinity,
              worker,
              [rabbitmq_transport] 
             } ]} }.

get_transport() ->
    Children = supervisor:which_children(?MODULE),
    io:format("~p, ~n", Children),
    {transport, Transport, _, _} = lists:keyfind(transport, 1, Children),
    Transport.

