-module(publisher).

-behaviour(gen_server).

-export([start/3, start_link/3]).
-export([stop/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-record(state, {channel, auth_config, already_auth, user_id}).


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
    {ok, AuthConfig} = application:get_env(carotene, publish_auth),
    {ok, #state{user_id = UserId, channel = Channel, auth_config = AuthConfig, already_auth = false}}.

handle_info({'DOWN', _Ref, process, _Pid, _}, State) ->
    {stop, shutdown, State};
handle_info(shutdown, State) ->
    {stop, shutdown, State}.

handle_call({publish, Message}, _From, State = #state{channel = Channel, auth_config = AuthConfig, user_id = UserId}) ->
    case already_auth of
        true -> 
                gen_server:cast(router, {publish,  Message, channel, Channel}),
                {reply, ok, State};
        _ ->
            case can_publish(UserId, AuthConfig, Channel) of
                ok ->
                    gen_server:cast(router, {publish,  Message, channel, Channel}),
                    {reply, ok, State#state{already_auth = true}};
                Error -> {reply, {error, Error}, State}
            end
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal
can_publish(UserId, AuthConfig, Channel) ->
    case lists:keyfind(enabled, 1, AuthConfig) of
        false -> ok;
        {enabled, false} -> ok;
        {enabled, true} -> case lists:keyfind(level, 1, AuthConfig) of
                               false -> bad_configuration;
                               {level, anonymous} -> ok;
                               {level, auth} -> case UserId of
                                                    undefined -> needs_authentication;
                                                    _ -> ok
                                                end;
                               {level, ask} -> case ask_authentication(UserId, AuthConfig, Channel) of
                                                   true -> ok;
                                                   Error -> Error
                                               end
                           end;
        _ -> ok
    end.

ask_authentication(UserId, AuthConfig, Channel) ->
    case lists:keyfind(authorization_url, 1, AuthConfig) of
        false -> bad_configuration;
        {authorization_url, AuthorizeUrl} ->
            {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {AuthorizeUrl, [], "application/x-www-form-urlencoded", "user_id="++binary_to_list(UserId)++"&channel="++binary_to_list(Channel)}, [], []),
            % TODO: This can crash
            case jsx:decode(binary:list_to_bin(Body)) of
                [{<<"authorized">>, <<"true">>}] -> true;
                [{<<"authorized">>, <<"false">>}] -> no_authorization;
                _ -> 
                    bad_server_response_on_authorization
            end
    end.
