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
                                  {broker_sup,
                                   {broker_sup, start_link, []},
                                   permanent,
                                   infinity,
                                   supervisor,
                                   [broker_sup]
                                  },
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
                                   temporary,
                                   infinity,
                                   supervisor,
                                   [publisher_sup] 
                                  },
                                  {subscriber_sup,
                                   {subscriber_sup, start_link, []},
                                   temporary,
                                   infinity,
                                   supervisor,
                                   [subscriber_sup] 
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
                                  },
                                  {router,
                                   {router, start_link, []},
                                   permanent,
                                   infinity,
                                   worker,
                                   [router] 
                                  }
                                 ]} }.
