-module(msg_queue_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, presence/1]).

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
    {ok, { {simple_one_for_one, 5, 10}, [
             {broker,
              {msg_queue_serv, start_link, []},
              permanent,
              infinity,
              worker,
              [mgs_queue_serv] 
             } ]} }.

presence(ExchangeName) ->
    Children = supervisor:which_children(?MODULE).

