-module(subscriber).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export([start/3, start_link/3]).
-export([stop/1]).

-record(state, {channel, reply_pid, auth_config, already_auth, user_id}).

start_link(Channel, UserId, ReplyPid) ->
    Opts = [],
    gen_server:start_link(?MODULE, [Channel, UserId, ReplyPid], Opts).

start(Channel, UserId, ReplyPid) ->
    Opts = [],
    gen_server:start(?MODULE, [Channel, UserId, ReplyPid], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([Channel, UserId, ReplyPid]) ->
    {ok, AuthConfig} = application:get_env(carotene, subscribe_auth),
    erlang:monitor(process, ReplyPid),
    % TODO: things can go wrong here with authorization, but lets advance first
    ok = maybe_consume(UserId, AuthConfig, Channel),
    gen_server:cast(presence_serv, {subscribe, UserId, Channel, self()}),
    {ok, #state{reply_pid = ReplyPid, channel = Channel, user_id = UserId}}.

handle_info({'DOWN', _Ref, process, _Pid, _}, State) ->
    {stop, normal, State};

handle_info({received_message, Msg}, State = #state{reply_pid = ReplyPid}) ->
    ReplyPid ! {received_message, Msg},
    {noreply, State};

handle_info(shutdown, State) ->
    {stop, normal, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, #state{channel = Channel, user_id = UserId}) ->
    gen_server:cast(presence_serv, {unsubscribe, UserId, Channel, self()}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

%% Internal
maybe_consume(UserId, AuthConfig, Channel) ->
    case can_subscribe(UserId, AuthConfig, Channel) of
        ok -> subscribe(Channel);
        _ -> error
    end.

can_subscribe(UserId, AuthConfig, Channel) ->
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
                _ -> bad_server_response_on_authorization
            end
    end.

subscribe(Channel) ->
    ok = gen_server:cast(router, {subscribe, Channel, from, self()}).
