-module(carotene_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
                                  {http_initializer,
                                   {http_initializer, start_link, [self()]},
                                   permanent,
                                   infinity,
                                   worker,
                                   [http_initializer] 
                                  },
                                  {connection_sup,
                                   {connection_sup, start_link, []},
                                   permanent,
                                   infinity,
                                   supervisor,
                                   [connection_sup] 
                                  },
                                  {publisher_sup,
                                   {publisher_sup, start_link, []},
                                   permanent,
                                   infinity,
                                   supervisor,
                                   [publisher_sup] 
                                  },
                                  {msg_queue_sup,
                                   {msg_queue_sup, start_link, []},
                                   permanent,
                                   infinity,
                                   supervisor,
                                   [msg_queue_sup] 
                                  },
                                  {presence_serv,
                                   {presence_serv, start_link, []},
                                   permanent,
                                   infinity,
                                   supervisor,
                                   [presence_serv] 
                                  },
                                  {admin_serv,
                                   {admin_serv, start_link, []},
                                   permanent,
                                   infinity,
                                   worker,
                                   [admin_serv] 
                                  }
                                 ]} }.
