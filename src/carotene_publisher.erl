-module(carotene_publisher).

-behaviour(gen_server).

-export([start/3, start_link/3]).
-export([stop/1]).
-export([publish/2, update_user/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-record(state, {channel, authorization_config, already_authorized, user_id}).

start_link(Channel, UserId, From) ->
    Opts = [],
    gen_server:start_link(?MODULE, [Channel, UserId, From], Opts).

start(Channel, UserId, From) ->
    Opts = [],
    gen_server:start(?MODULE, [Channel, UserId, From], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([Channel, UserId, From]) ->
    erlang:monitor(process, From),
    {ok, #state{user_id = UserId, channel = Channel, already_authorized = false}}.

handle_info({'DOWN', _Ref, process, _Pid, _}, State) ->
    {stop, shutdown, State};
handle_info(shutdown, State) ->
    {stop, shutdown, State}.

handle_call({publish, Message}, _From, State = #state{channel = Channel, user_id = UserId}) ->
    case already_authorized of
        true -> 
                gen_server:cast(carotene_router, {publish,  Message, channel, Channel}),
                {reply, ok, State};
        _ ->
            case can_publish(UserId, Channel) of
                true ->
                    gen_server:cast(carotene_router, {publish,  Message, channel, Channel}),
                    {reply, ok, State#state{already_authorized = true}};
                Error -> {reply, {error, Error}, State}
            end
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({update_user, UserId}, State) ->
    {noreply, State#state{user_id = UserId}};

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Own exported functions
%%--------------------------------------------------------------------

publish(PublisherPid, Message) -> gen_server:call(PublisherPid, {publish, Message}).

update_user(PublisherPid, UserId) -> gen_server:cast(PublisherPid, {update_user, UserId}).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
can_publish(UserId, Channel) ->
    case application:get_env(carotene, publish_authorization) of
        % if no authorization config defined, user can publish
        undefined -> true;
        {ok, AuthConfig} -> carotene_authorization:check_authorization(UserId, Channel, AuthConfig)
    end.
