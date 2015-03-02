-module(carotene_http).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

start_link() ->
    Opts = [],
    gen_server:start(?MODULE, [], Opts).

init([]) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/channels/[:channel/]", api_channels_handler, []},
            {"/api/channels/:channel/messages", api_messages_handler, []},
            {"/api/channels/:channel/presence", api_presence_handler, []},
            {"/websocket", ws_handler, []}
        ]}
    ]),
    start_http(Dispatch),
    {ok, ok}.

handle_call(alloc, _From, State) ->
    {reply, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(shutdown, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_http(Dispatch) ->
    Port = case application:get_env(carotene, port) of
               undefined -> 8080;
               {ok, ConfPort} -> ConfPort
           end,
    case application:get_env(carotene, ssl) of
        undefined ->
            {ok, _} = cowboy:start_http(http, 100, [
                                                    {port, Port},
                                                    {cacertfile,  "/tmp/cowboy-ca.crt"},
                                                    {certfile, "/tmp/server.crt"},
                                                    {keyfile, "/tmp/server.key"}
                                                   ],
                                        [{env, [{dispatch, Dispatch}]}]);
        {ok, SSLConf} -> start_https(Dispatch, Port, SSLConf)
    end.

start_https(Dispatch, Port, SSLConf) ->
    % validate SSL config
    Cacertfile = case find_ssl_conf(cacertfile, SSLConf) of
                     error -> throw({ssl_config_error, "SSL enabled but no cacertfile specified"});
                     Value -> Value
                 end,
    Certfile = case find_ssl_conf(certfile, SSLConf) of
                     error -> throw({ssl_config_error, "SSL enabled but no certfile specified"});
                     Value2 -> Value2
                 end,
    Keyfile = case find_ssl_conf(keyfile, SSLConf) of
                     error -> throw({ssl_config_error, "SSL enabled but no keyfile specified"});
                     Value3 -> Value3
                 end,

    {ok, _} = cowboy:start_https(https, 100, [
                                            {port, Port},
                                            {cacertfile,  Cacertfile},
                                            {certfile, Certfile},
                                            {keyfile, Keyfile}
                                           ],
                                [{env, [{dispatch, Dispatch}]}]).

find_ssl_conf(_Option, []) -> error;
find_ssl_conf(Option, [{Option, Value}|_SSLConf]) -> Value;
find_ssl_conf(Option, [_Entry|SSLConf]) -> find_ssl_conf(Option, SSLConf).

