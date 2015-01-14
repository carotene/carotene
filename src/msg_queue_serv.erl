-module(msg_queue_serv).

-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export([start/2]).
-export([stop/1]).


-record(state, {channel, reply_pid}).

start(Exchange, ReplyPid) ->
    Opts = [],
    gen_server:start(?MODULE, [Exchange, ReplyPid], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([Exchange, ReplyPid]) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{}),
    amqp_channel:call(Channel, #'queue.bind'{queue = Queue,
                                             exchange = Exchange
                                            }),

    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue,
                                                     no_ack = true}, self()),
    io:format("wat is going on"),
    {ok, #state{channel = Channel, reply_pid = ReplyPid}}.

handle_info(shutdown, State) ->
    {stop, normal, State};

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info(#'basic.cancel_ok'{}, State) ->
    {stop, normal, State};

handle_info({#'basic.deliver'{},
             #amqp_msg{payload = Msg}},
             #state{reply_pid = ReplyPid} = State) ->

    ReplyPid ! {received_message, Msg},

    {noreply, State}.

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
