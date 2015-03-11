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
                                  {carotene_connection_sup,
                                   {carotene_connection_sup, start_link, []},
                                   permanent,
                                   infinity,
                                   supervisor,
                                   [carotene_connection_sup] 
                                  },
                                  {carotene_publisher_sup,
                                   {carotene_publisher_sup, start_link, []},
                                   temporary,
                                   infinity,
                                   supervisor,
                                   [carotene_publisher_sup] 
                                  },
                                  {carotene_subscriber_sup,
                                   {carotene_subscriber_sup, start_link, []},
                                   temporary,
                                   infinity,
                                   supervisor,
                                   [carotene_subscriber_sup] 
                                  },
                                  {carotene_http,
                                   {carotene_http, start_link, []},
                                   permanent,
                                   infinity,
                                   worker,
                                   [carotene_http] 
                                  },
                                  {carotene_router,
                                   {cerotene_router, start_link, []},
                                   permanent,
                                   infinity,
                                   worker,
                                   [carotene_router] 
                                  },
                                  {carotene_presence,
                                   {carotene_presence, start_link, []},
                                   permanent,
                                   infinity,
                                   supervisor,
                                   [carotene_presence] 
                                  },
                                  {carotene_admin_connection,
                                   {carotene_admin_connection, start_link, []},
                                   permanent,
                                   infinity,
                                   worker,
                                   [carotene_admin_connection] 
                                  }
                                 ]} }.
