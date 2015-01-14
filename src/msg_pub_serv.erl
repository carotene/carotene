-module(msg_pub_serv).

-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export([start/1]).
-export([stop/1]).


-record(state, {channel, exchange}).

start(Exchange) ->
    Opts = [],
    gen_server:start(?MODULE, [Exchange], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([Exchange]) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange,
                                                   type = <<"fanout">>
                                                  }),

    {ok, #state{channel = Channel, exchange = Exchange}}.

handle_info(shutdown, State) ->
    {stop, normal, State}.

handle_call({send, Message}, _From, State = #state{channel = Channel, exchange = Exchange}) ->
    amqp_channel:cast(Channel,
                      #'basic.publish'{
                        exchange = Exchange,
                        routing_key = <<"">>},
                      #amqp_msg{payload = Message}),

    {reply, ok, State};
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
