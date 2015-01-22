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
    BrokerModule = read_broker_config(),

    {ok, { {one_for_one, 5, 10}, [
             {broker,
              {BrokerModule, start_link, [self()]},
              permanent,
              infinity,
              worker,
              [BrokerModule] 
             } ]} }.

read_broker_config() ->
    {ok, Broker} = application:get_env(carotene, broker),
    case Broker of
        rabbitmq -> rabbitmq_broker;
        redis -> redis_broker
    end.

get_broker() ->
    Children = supervisor:which_children(?MODULE),
    {broker, Broker, _, _} = lists:keyfind(broker, 1, Children),
    {read_broker_config(), Broker}.
