-module(rabbitmq_broker).

-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).
-export([start_link/0, start/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

-record(state, {channel}).

start_link() ->
    Opts = [],
    gen_server:start(?MODULE, [], Opts).

start() ->
    Opts = [],
    gen_server:start(?MODULE, [], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([]) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    {ok, #state{channel = Channel}}.

handle_info(shutdown, State) ->
    {stop, normal, State}.

handle_call(get_channel, _From, State = #state{channel = Channel}) ->
    {reply, {ok, Channel}, State};
handle_call({declare_exchange, {Exchange, Type}}, _From, State = #state{channel = Channel}) ->
    amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange,
                                                   type = Type
                                                  }),
    {reply, ok, State};

handle_call({publish, {Exchange, Message}}, _From, State = #state{channel = Channel}) ->
    amqp_channel:cast(Channel,
                      #'basic.publish'{
                        exchange = Exchange,
                        routing_key = <<"">>},
                      #amqp_msg{payload = Message}),
    {reply, ok, State};
%handle_call({send, Message}, _From, State = #state{channel = Channel, exchange = Exchange}) ->
%    amqp_channel:cast(Channel,
%                      #'basic.publish'{
%                        exchange = Exchange,
%                        routing_key = <<"">>},
%                      #amqp_msg{payload = Message}),
%
%    {reply, ok, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, #state{channel = Channel}) ->
    amqp_channel:close(Channel),
    %ok = amqp_connection:close(Connection),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.
