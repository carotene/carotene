-module(subscriber_sup).

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
             {subscriber,
              {subscriber, start_link, []},
              temporary,
              infinity,
              worker,
              [mgs_queue_serv] 
             } ]} }.

presence(ExchangeName) ->
    Children = supervisor:which_children(?MODULE),
    % FIXME: this is very wrong, very slow and broken, but is a first version
    MaybeUsers = lists:map(fun({_, ChildPid, _, _}) ->
                      {ok, MaybeUser} = gen_server:call(ChildPid, {presence, ExchangeName}),
                      MaybeUser
              end, Children),
    lists:usort(lists:filter(fun(MaybeUserId) ->
                         false /= MaybeUserId
                 end, MaybeUsers)).
