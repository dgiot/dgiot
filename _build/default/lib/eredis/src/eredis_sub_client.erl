%%
%% eredis_pubsub_client
%%
%% This client implements a subscriber to a Redis pubsub channel. It
%% is implemented in the same way as eredis_client, except channel
%% messages are streamed to the controlling process. Messages are
%% queued and delivered when the client acknowledges receipt.
%%
%% There is one consuming process per eredis_sub_client.
-module(eredis_sub_client).
-behaviour(gen_server).
-include("eredis.hrl").
-include("eredis_sub.hrl").


%% API
-export([start_link/7, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%
%% API
%%

-spec start_link(Host::list(),
                 Port::integer(),
                 Password::string(),
                 Database::integer(),
                 ReconnectSleep::reconnect_sleep(),
                 MaxQueueSize::integer() | infinity,
                 QueueBehaviour::drop | exit) ->
                        {ok, Pid::pid()} | {error, Reason::term()}.
start_link(Host, Port, Password, Database, ReconnectSleep, MaxQueueSize, QueueBehaviour) ->
    Args = [Host, Port, Password, Database, ReconnectSleep, MaxQueueSize, QueueBehaviour],
    gen_server:start_link(?MODULE, Args, []).


stop(Pid) ->
    gen_server:call(Pid, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, Port, Password, Database, ReconnectSleep, MaxQueueSize, QueueBehaviour]) ->
    Sentinel = case Host of
        "sentinel:"++MasterStr -> list_to_atom(MasterStr);
         _ -> undefined
    end,
    State = #state{host            = Host,
                   port            = Port,
                   password        = list_to_binary(Password),
                   reconnect_sleep = ReconnectSleep,
                   channels        = [],
                   parser_state    = eredis_parser:init(),
                   msg_queue       = queue:new(),
                   max_queue_size  = MaxQueueSize,
                   queue_behaviour = QueueBehaviour,
                   sentinel        = Sentinel,
                   database        = read_database(Database)},

    case connect(State) of
        {ok, NewState} ->
            ok = inet:setopts(NewState#state.socket, [{active, once}]),
            {ok, NewState};
        {error, Reason} ->
            {stop, Reason}
    end.

%% Set the controlling process. All messages on all channels are directed here.
handle_call({controlling_process, Pid}, _From, State) ->
    case State#state.controlling_process of
        undefined ->
            ok;
        {OldRef, _OldPid} ->
            erlang:demonitor(OldRef)
    end,
    Ref = erlang:monitor(process, Pid),
    {reply, ok, State#state{controlling_process={Ref, Pid}, msg_state = ready}};

handle_call(get_channels, _From, State) ->
    {reply, {ok, State#state.channels}, State};


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.


%% Controlling process acks, but we have no connection. When the
%% connection comes back up, we should be ready to forward a message
%% again.
handle_cast({ack_message, Pid},
            #state{controlling_process={_, Pid}, socket = undefined} = State) ->
    {noreply, State#state{msg_state = ready}};

%% Controlling process acknowledges receipt of previous message. Send
%% the next if there is any messages queued or ask for more on the
%% socket.
handle_cast({ack_message, Pid},
            #state{controlling_process={_, Pid}} = State) ->
    NewState = case queue:out(State#state.msg_queue) of
                   {empty, _Queue} ->
                       State#state{msg_state = ready};
                   {{value, Msg}, Queue} ->
                       send_to_controller(Msg, State),
                       State#state{msg_queue = Queue, msg_state = need_ack}
               end,
    {noreply, NewState};

handle_cast({subscribe, Pid, Channels}, #state{controlling_process = {_, Pid}} = State) ->
    Command = eredis:create_multibulk(["SUBSCRIBE" | Channels]),
    ok = gen_tcp:send(State#state.socket, Command),
    NewChannels = add_channels(Channels, State#state.channels),
    {noreply, State#state{channels = NewChannels}};


handle_cast({psubscribe, Pid, Channels}, #state{controlling_process = {_, Pid}} = State) ->
    Command = eredis:create_multibulk(["PSUBSCRIBE" | Channels]),
    ok = gen_tcp:send(State#state.socket, Command),
    NewChannels = add_channels(Channels, State#state.channels),
    {noreply, State#state{channels = NewChannels}};



handle_cast({unsubscribe, Pid, Channels}, #state{controlling_process = {_, Pid}} = State) ->
    Command = eredis:create_multibulk(["UNSUBSCRIBE" | Channels]),
    ok = gen_tcp:send(State#state.socket, Command),
    NewChannels = remove_channels(Channels, State#state.channels),
    {noreply, State#state{channels = NewChannels}};



handle_cast({punsubscribe, Pid, Channels}, #state{controlling_process = {_, Pid}} = State) ->
    Command = eredis:create_multibulk(["PUNSUBSCRIBE" | Channels]),
    ok = gen_tcp:send(State#state.socket, Command),
    NewChannels = remove_channels(Channels, State#state.channels),
    {noreply, State#state{channels = NewChannels}};



handle_cast({ack_message, _}, State) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%% Receive data from socket, see handle_response/2
handle_info({tcp, _Socket, Bs}, State) ->
    ok = inet:setopts(State#state.socket, [{active, once}]),

    NewState = handle_response(Bs, State),
    case queue:len(NewState#state.msg_queue) > NewState#state.max_queue_size of
        true ->
            case State#state.queue_behaviour of
                drop ->
                    Msg = {dropped, queue:len(NewState#state.msg_queue)},
                    send_to_controller(Msg, NewState),
                    {noreply, NewState#state{msg_queue = queue:new()}};
                exit ->
                    {stop, max_queue_size, State}
            end;
        false ->
            {noreply, NewState}
    end;

handle_info({tcp_error, _Socket, _Reason}, State) ->
    %% This will be followed by a close
    {noreply, State};

%% Socket got closed, for example by Redis terminating idle
%% clients. If desired, spawn of a new process which will try to reconnect and
%% notify us when Redis is ready. In the meantime, we can respond with
%% an error message to all our clients.
handle_info({tcp_closed, _Socket}, #state{reconnect_sleep = no_reconnect} = State) ->
    %% If we aren't going to reconnect, then there is nothing else for this process to do.
    {stop, normal, State#state{socket = undefined}};

handle_info({tcp_closed, _Socket}, State) ->
    Self = self(),
    send_to_controller({eredis_disconnected, Self}, State),
    spawn(fun() -> reconnect_loop(Self, State) end),

    %% Throw away the socket. The absence of a socket is used to
    %% signal we are "down"
    {noreply, State#state{socket = undefined}};

%% Controller might want to be notified about every reconnect attempt
handle_info(reconnect_attempt, State) ->
    send_to_controller({eredis_reconnect_attempt, self()}, State),
    {noreply, State};

%% Controller might want to be notified about every reconnect failure and reason
handle_info({reconnect_failed, Reason}, State) ->
    send_to_controller({eredis_reconnect_failed, self(),
                        {error, {connection_error, Reason}}}, State),
    {noreply, State};

%% Redis is ready to accept requests, the given Socket is a socket
%% already connected and authenticated.
handle_info({connection_ready, Socket}, #state{socket = undefined} = State) ->
    send_to_controller({eredis_connected, self()}, State),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{socket = Socket}};

handle_info({sentinel, {reconnect, _MasterName, Host, Port}}, State) ->
    do_sentinel_reconnect(Host, Port, State);

%% Our controlling process is down.
handle_info({'DOWN', Ref, process, Pid, _Reason},
            #state{controlling_process={Ref, Pid}} = State) ->
    {stop, shutdown, State#state{controlling_process=undefined,
                                 msg_state=ready,
                                 msg_queue=queue:new()}};

%% eredis can be used in Poolboy, but it requires to support a simple API
%% that Poolboy uses to manage the connections.
handle_info(stop, State) ->
    {stop, shutdown, State};

handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.

terminate(_Reason, State) ->
    case State#state.socket of
        undefined -> ok;
        Socket    -> gen_tcp:close(Socket)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec remove_channels([binary()], [binary()]) -> [binary()].
remove_channels(Channels, OldChannels) ->
    lists:foldl(fun lists:delete/2, OldChannels, Channels).

-spec add_channels([binary()], [binary()]) -> [binary()].
add_channels(Channels, OldChannels) ->
    lists:foldl(fun(C, Cs) ->
        case lists:member(C, Cs) of
            true ->
                Cs;
            false ->
                [C|Cs]
        end
    end, OldChannels, Channels).

-spec handle_response(Data::binary(), State::#state{}) -> NewState::#state{}.
%% @doc: Handle the response coming from Redis. This should only be
%% channel messages that we should forward to the controlling process
%% or queue if the previous message has not been acked. If there are
%% more than a single response in the data we got, queue the responses
%% and serve them up when the controlling process is ready
handle_response(Data, #state{parser_state = ParserState} = State) ->
    case eredis_parser:parse(ParserState, Data) of
        {ReturnCode, Value, NewParserState} ->
            reply({ReturnCode, Value},
                  State#state{parser_state=NewParserState});

        {ReturnCode, Value, Rest, NewParserState} ->
            NewState = reply({ReturnCode, Value},
                             State#state{parser_state=NewParserState}),
            handle_response(Rest, NewState);

        {continue, NewParserState} ->
            State#state{parser_state = NewParserState}
    end.

%% @doc: Sends a reply to the controlling process if the process has
%% acknowledged the previous process, otherwise the message is queued
%% for later delivery.
reply({ok, [<<"message">>, Channel, Message]}, State) ->
    queue_or_send({message, Channel, Message, self()}, State);

reply({ok, [<<"pmessage">>, Pattern, Channel, Message]}, State) ->
    queue_or_send({pmessage, Pattern, Channel, Message, self()}, State);



reply({ok, [<<"subscribe">>, Channel, _]}, State) ->
    queue_or_send({subscribed, Channel, self()}, State);

reply({ok, [<<"psubscribe">>, Channel, _]}, State) ->
    queue_or_send({subscribed, Channel, self()}, State);


reply({ok, [<<"unsubscribe">>, Channel, _]}, State) ->
    queue_or_send({unsubscribed, Channel, self()}, State);


reply({ok, [<<"punsubscribe">>, Channel, _]}, State) ->
    queue_or_send({unsubscribed, Channel, self()}, State);
reply({ReturnCode, Value}, State) ->
    throw({unexpected_response_from_redis, ReturnCode, Value, State}).


queue_or_send(Msg, State) ->
    case State#state.msg_state of
        need_ack ->
            MsgQueue = queue:in(Msg, State#state.msg_queue),
            State#state{msg_queue = MsgQueue};
        ready ->
            send_to_controller(Msg, State),
            State#state{msg_state = need_ack}
    end.


connect(#state{sentinel = undefined} = State) ->
    connect1(State);
connect(#state{sentinel = Master} = State) ->
    case eredis_sentinel:get_master(Master, true) of
        {ok, {Host, Port}} ->
            connect1(State#state{host=Host, port=Port});
        {error, Error} ->
            {error, {sentinel_error, Error}}
    end.


%% @doc: Helper for connecting to Redis. These commands are
%% synchronous and if Redis returns something we don't expect, we
%% crash. Returns {ok, State} or {error, Reason}.
connect1(State) ->
    {ok, {AFamily, Addr}} = get_addr(State#state.host),
    case gen_tcp:connect(Addr, State#state.port,
                         [AFamily | ?SOCKET_OPTS], State#state.connect_timeout) of
        {ok, Socket} ->
            case authenticate(Socket, State#state.password) of
                ok ->
                    case select_database(Socket, State#state.database) of
                        ok ->
                            {ok, State#state{socket = Socket}};
                        {error, Reason} ->
                            {error, {select_error, Reason}}
                    end;
                {error, Reason} ->
                    {error, {authentication_error, Reason}}
            end;
        {error, Reason} ->
            {error, {connection_error, Reason}}
    end.

authenticate(_Socket, <<>>) ->
    ok;
authenticate(Socket, Password) ->
    eredis_client:do_sync_command(Socket, ["AUTH", " \"", Password, "\"\r\n"]).


select_database(_Socket, undefined) ->
    ok;
select_database(_Socket, <<"0">>) ->
    ok;
select_database(Socket, Database) ->
    eredis_client:do_sync_command(Socket, ["SELECT", " ", Database, "\r\n"]).

%% @doc: Loop until a connection can be established, this includes
%% successfully issuing the auth and select calls. When we have a
%% connection, give the socket to the redis client.
reconnect_loop(Client, #state{reconnect_sleep=ReconnectSleep}=State) ->
    Client ! reconnect_attempt,
    case catch(connect(State)) of
        {ok, #state{socket = Socket}} ->
            gen_tcp:controlling_process(Socket, Client),
            Client ! {connection_ready, Socket};
        {error, Reason} ->
            Client ! {reconnect_failed, Reason},
            timer:sleep(ReconnectSleep),
            reconnect_loop(Client, State);
        %% Something bad happened when connecting, like Redis might be
        %% loading the dataset and we got something other than 'OK' in
        %% auth or select
        _ ->
            timer:sleep(ReconnectSleep),
            reconnect_loop(Client, State)
    end.


send_to_controller(_Msg, #state{controlling_process=undefined}) ->
    ok;
send_to_controller(Msg, #state{controlling_process={_Ref, Pid}}) ->
    %%error_logger:info_msg("~p ! ~p~n", [Pid, Msg]),
    Pid ! Msg.

%% Handle sentinel "reconnect to new master" message
%% 1. we already connected to new master - ignore
do_sentinel_reconnect(Host, Port, #state{host=Host,port=Port}=State) ->
    {noreply, State};
%% 2. we are waiting for reconnecting already - ignore
do_sentinel_reconnect(_Host, _Port, #state{socket=undefined}=State) ->
    {noreply, State};
%% 3. we are not supposed to reconnect - stop processing
do_sentinel_reconnect(_Host, _Port, #state{reconnect_sleep=no_reconnect}=State) ->
    {stop, sentinel_reconnect, State};
%% 4. we are connected to wrong master - reconnect
do_sentinel_reconnect(Host, Port, State) ->
    {ok, StateNew} = start_reconnect(State#state{host=Host, port=Port}),
    {noreply, StateNew}.

%% @doc Start reconnecting loop, close old connection if present.
-spec start_reconnect(#state{}) -> {ok, #state{}}.
start_reconnect(#state{socket=undefined} = State) ->
    Self = self(),
    spawn(fun() -> reconnect_loop(Self, State) end),

    %% Throw away the socket and the queue, as we will never get a
    %% response to the requests sent on the old socket. The absence of
    %% a socket is used to signal we are "down"
    %% TODO shouldn't we need to send error reply to waiting clients?
    {ok, State};
start_reconnect(#state{socket=Socket} = State) ->
    gen_tcp:close(Socket),
    start_reconnect(State#state{socket=undefined}).

get_addr(Hostname) ->
    case inet:parse_address(Hostname) of
        {ok, {_,_,_,_} = Addr} ->         {ok, {inet, Addr}};
        {ok, {_,_,_,_,_,_,_,_} = Addr} -> {ok, {inet6, Addr}};
        {error, einval} ->
            case inet:getaddr(Hostname, inet6) of
                 {error, _} ->
                     case inet:getaddr(Hostname, inet) of
                         {ok, Addr}-> {ok, {inet, Addr}};
                         {error, _} = Res -> Res
                     end;
                 {ok, Addr} -> {ok, {inet6, Addr}}
            end
    end.

read_database(undefined) ->
    undefined;
read_database(Database) when is_integer(Database) ->
    list_to_binary(integer_to_list(Database)).