-module(carotene_broker_sup).

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
    case read_broker_config() of
        undefined -> {ok, { {one_for_one, 5, 10}, []}};
        BrokerModule -> {ok, { {one_for_one, 5, 10}, [
                                                      {broker,
                                                       {BrokerModule, start_link, [self()]},
                                                       permanent,
                                                       infinity,
                                                       worker,
                                                       [BrokerModule] 
                                                      } ]} }
    end.

read_broker_config() ->
    case application:get_env(carotene, broker) of
        undefined -> undefined;
        {ok, rabbitmq} -> rabbitmq_broker;
        {ok, redis} -> redis_broker
    end.

get_broker() ->
    case read_broker_config() of
        undefined -> 
            undefined;
        BrokerModule ->
            Children = supervisor:which_children(?MODULE),
            {broker, Broker, _, _} = lists:keyfind(broker, 1, Children),
            {BrokerModule, Broker}
    end.
