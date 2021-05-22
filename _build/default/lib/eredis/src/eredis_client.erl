%%
%% eredis_client
%%
%% The client is implemented as a gen_server which keeps one socket
%% open to a single Redis instance. Users call us using the API in
%% eredis.erl.
%%
%% The client works like this:
%%  * When starting up, we connect to Redis with the given connection
%%     information, or fail.
%%  * Users calls us using gen_server:call, we send the request to Redis,
%%    add the calling process at the end of the queue and reply with
%%    noreply. We are then free to handle new requests and may reply to
%%    the user later.
%%  * We receive data on the socket, we parse the response and reply to
%%    the client at the front of the queue. If the parser does not have
%%    enough data to parse the complete response, we will wait for more
%%    data to arrive.
%%  * For pipeline commands, we include the number of responses we are
%%    waiting for in each element of the queue. Responses are queued until
%%    we have all the responses we need and then reply with all of them.
%%
-module(eredis_client).
-behaviour(gen_server).
-include("eredis.hrl").

%% API
-export([start_link/6, start_link/7, stop/1, select_database/2]).

-export([do_sync_command/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          host :: string() | undefined,
          port :: integer() | undefined,
          password :: binary() | undefined,
          database :: binary() | undefined,
          sentinel :: undefined | atom(),
          reconnect_sleep :: reconnect_sleep() | undefined,
          connect_timeout :: integer() | undefined,
          socket :: port() | undefined,
          parser_state :: #pstate{} | undefined,
          queue :: eredis_queue() | undefined
}).

%%
%% API
%%

-spec start_link(
                 Host::list(),
                 Port::integer(),
                 Database::integer() | undefined,
                 Password::string(),
                 ReconnectSleep::reconnect_sleep(),
                 ConnectTimeout::integer() | undefined,
                 Options::list()
                ) -> {ok, Pid::pid()} | {error, Reason::term()}.
start_link(Host, Port, Database, Password, ReconnectSleep, ConnectTimeout, Options) ->
    gen_server:start_link(?MODULE, [Host, Port, Database, Password,
                                    ReconnectSleep, ConnectTimeout, Options], []).
-spec start_link(
                 Host::list(),
                 Port::integer(),
                 Database::integer() | undefined,
                 Password::string(),
                 ReconnectSleep::reconnect_sleep(),
                 ConnectTimeout::integer() | undefined
                ) -> {ok, Pid::pid()} | {error, Reason::term()}.
start_link(Host, Port, Database, Password, ReconnectSleep, ConnectTimeout) ->
    gen_server:start_link(?MODULE, [Host, Port, Database, Password,
                                    ReconnectSleep, ConnectTimeout, []], []).
stop(Pid) ->
    gen_server:call(Pid, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, Port, Database, Password, ReconnectSleep, ConnectTimeout, Options]) ->
    Sentinel = case Host of
        "sentinel:"++MasterStr -> list_to_atom(MasterStr);
         _ -> undefined
    end,
    erlang:put(options, Options),
    State = #state{host = Host,
                   port = Port,
                   database = read_database(Database),
                   password = list_to_binary(Password),
                   reconnect_sleep = ReconnectSleep,
                   connect_timeout = ConnectTimeout,
                   parser_state = eredis_parser:init(),
                   queue = queue:new(),
                   sentinel = Sentinel},
    case connect(State, Options) of
        {ok, NewState} ->
            {ok, NewState};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({request, Req}, From, State) ->
    do_request(Req, From, State);

handle_call({pipeline, Pipeline}, From, State) ->
    do_pipeline(Pipeline, From, State);

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

%% Send 
handle_call({send, Command}, _From, #state{socket = Socket} = State) ->
    Transport = get_tranport(),
    ok = Transport:setopts(Socket, [{active, false}]),
    case Transport:send(Socket, Command) of
        ok ->
            %% Hope there's nothing else coming down on the socket..
            case Transport:recv(Socket, 0, ?RECV_TIMEOUT) of
                {ok, <<"+OK\r\n">>} ->
                     ok = Transport:setopts(Socket, [{active, once}]),
                    {noreply, State};
                Other ->
                    {error, {unexpected_data, Other}}
            end;
        {error, Reason} ->
            {error, Reason}
    end;

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.


handle_cast({request, Req}, State) ->
    case do_request(Req, undefined, State) of
        {reply, _Reply, State1} ->
            {noreply, State1};
        {noreply, State1} ->
            {noreply, State1}
    end;

handle_cast({request, Req, Pid}, State) ->
    case do_request(Req, Pid, State) of
        {reply, Reply, State1} ->
            safe_send(Pid, {response, Reply}),
            {noreply, State1};
        {noreply, State1} ->
            {noreply, State1}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Receive data from socket, see handle_response/2. Match `Socket' to
%% enforce sanity.
handle_info({tcp, Socket, Bs}, #state{socket = Socket} = State) ->
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, handle_response(Bs, State)};

handle_info({tcp, Socket, _}, #state{socket = OurSocket} = State)
  when OurSocket =/= Socket ->
    %% Ignore tcp messages when the socket in message doesn't match
    %% our state. In order to test behavior around receiving
    %% tcp_closed message with clients waiting in queue, we send a
    %% fake tcp_close message. This allows us to ignore messages that
    %% arrive after that while we are reconnecting.
    {noreply, State};

handle_info({ssl, SSLSocket, Data}, #state{socket = SSLSocket} = State) ->
    ok = ssl:setopts(SSLSocket, [{active, once}]),
    {noreply, handle_response(Data, State)};

handle_info({tcp_error, _Socket, _Reason}, State) ->
    %% This will be followed by a close
    {noreply, State};

%% Socket got closed, for example by Redis terminating idle
%% clients. If desired, spawn of a new process which will try to reconnect and
%% notify us when Redis is ready. In the meantime, we can respond with
%% an error message to all our clients.
handle_info({tcp_closed, _Socket}, #state{reconnect_sleep = no_reconnect,
                                          queue = Queue} = State) ->
    reply_all({error, tcp_closed}, Queue),
    %% If we aren't going to reconnect, then there is nothing else for
    %% this process to do.
    {stop, normal, State#state{socket = undefined}};

handle_info({tcp_closed, _Socket}, #state{queue = Queue} = State) ->
    %% tell all of our clients what has happened.
    reply_all({error, tcp_closed}, Queue),

    %% Throw away the socket and the queue, as we will never get a
    %% response to the requests sent on the old socket. The absence of
    %% a socket is used to signal we are "down"
    {ok, StateNew} = start_reconnect(State#state{socket = undefined}),
    {noreply, StateNew};

%% Redis is ready to accept requests, the given Socket is a socket
%% already connected and authenticated.
handle_info({connection_ready, Socket, Host, Port}, #state{socket = undefined} = State) ->
    {noreply, State#state{socket = Socket, host=Host, port=Port}};

handle_info({sentinel, {reconnect, _MasterName, Host, Port}}, State) ->
    do_sentinel_reconnect(Host, Port, State);

%% eredis can be used in Poolboy, but it requires to support a simple API
%% that Poolboy uses to manage the connections.
handle_info(stop, State) ->
    {stop, shutdown, State};

handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.

terminate(_Reason, State) ->

    Transport = get_tranport(),
    case State#state.socket of
        undefined -> ok;
        Socket    -> Transport:close(Socket)
    end,
    ok.
%% Down
code_change(_, State, _) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec do_request(Req::iolist(), From::pid(), #state{}) ->
                        {noreply, #state{}} | {reply, Reply::any(), #state{}}.
%% @doc: Sends the given request to redis. If we do not have a
%% connection, returns error.
do_request(_Req, _From, #state{socket = undefined} = State) ->
    {reply, {error, no_connection}, State};

do_request(Req, From, State) ->
    Transport = get_tranport(),
    case Transport:send(State#state.socket, Req) of
        ok ->
            NewQueue = queue:in({1, From}, State#state.queue),
            {noreply, State#state{queue = NewQueue}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

-spec do_pipeline(Pipeline::pipeline(), From::pid(), #state{}) ->
                         {noreply, #state{}} | {reply, Reply::any(), #state{}}.
%% @doc: Sends the entire pipeline to redis. If we do not have a
%% connection, returns error.
do_pipeline(_Pipeline, _From, #state{socket = undefined} = State) ->
    {reply, {error, no_connection}, State};

do_pipeline(Pipeline, From, State) ->
    Transport = get_tranport(),
    case Transport:send(State#state.socket, Pipeline) of
        ok ->
            NewQueue = queue:in({length(Pipeline), From, []}, State#state.queue),
            {noreply, State#state{queue = NewQueue}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

-spec handle_response(Data::binary(), State::#state{}) -> NewState::#state{}.
%% @doc: Handle the response coming from Redis. This includes parsing
%% and replying to the correct client, handling partial responses,
%% handling too much data and handling continuations.
handle_response(Data, #state{parser_state = ParserState,
                             queue = Queue} = State) ->
    case eredis_parser:parse(ParserState, Data) of
        %% Got complete response, return value to client
        {ReturnCode, Value, NewParserState} ->
            NewQueue = reply({ReturnCode, Value}, Queue),
            State#state{parser_state = NewParserState,
                        queue = NewQueue};

        %% Got complete response, with extra data, reply to client and
        %% recurse over the extra data
        {ReturnCode, Value, Rest, NewParserState} ->
            NewQueue = reply({ReturnCode, Value}, Queue),
            handle_response(Rest, State#state{parser_state = NewParserState,
                                              queue = NewQueue});

        %% Parser needs more data, the parser state now contains the
        %% continuation data and we will try calling parse again when
        %% we have more data
        {continue, NewParserState} ->
            State#state{parser_state = NewParserState};
        _ -> ok
    end.

%% @doc: Sends a value to the first client in queue. Returns the new
%% queue without this client. If we are still waiting for parts of a
%% pipelined request, push the reply to the the head of the queue and
%% wait for another reply from redis.
reply(Value, Queue) ->
    case queue:out(Queue) of
        {{value, {1, From}}, NewQueue} ->
            safe_reply(From, Value),
            NewQueue;
        {{value, {1, From, Replies}}, NewQueue} ->
            safe_reply(From, lists:reverse([Value | Replies])),
            NewQueue;
        {{value, {N, From, Replies}}, NewQueue} when N > 1 ->
            queue:in_r({N - 1, From, [Value | Replies]}, NewQueue);
        {empty, Queue} ->
            %% Oops
            error_logger:info_msg("Nothing in queue, but got value from parser~n"),
            throw(empty_queue)
    end.

%% @doc Send `Value' to each client in queue. Only useful for sending
%% an error message. Any in-progress reply data is ignored.
-spec reply_all(any(), eredis_queue()) -> ok.
reply_all(Value, Queue) ->
    case queue:peek(Queue) of
        empty ->
            ok;
        {value, Item} ->
            safe_reply(receipient(Item), Value),
            reply_all(Value, queue:drop(Queue))
    end.

receipient({_, From}) ->
    From;
receipient({_, From, _}) ->
    From.

safe_reply(undefined, _Value) ->
    ok;
safe_reply(Pid, Value) when is_pid(Pid) ->
    safe_send(Pid, {response, Value});
safe_reply(From, Value) ->
    gen_server:reply(From, Value).

safe_send(Pid, Value) ->
    try erlang:send(Pid, Value)
    catch
        Err:Reason ->
            error_logger:info_msg("Failed to send message to ~p with reason ~p~n", [Pid, {Err, Reason}])
    end.

%% @doc: Helper for connecting to Redis, authenticating and selecting
%% the correct database. These commands are synchronous and if Redis
%% returns something we don't expect, we crash. Returns {ok, State} or
%% {SomeError, Reason}.

connect(#state{sentinel = undefined} = State, Options) ->
    connect1(State, Options);
connect(#state{sentinel = Master} = State, Options) ->
    case eredis_sentinel:get_master(Master, true) of
        {ok, {Host, Port}} ->
            connect1(State#state{host=Host, port=Port}, Options);
        {error, Error} ->
            {error, {sentinel_error, Error}}
    end.

connect1(State, Options) ->
    case proplists:get_value(ssl_options , Options, []) of
        [] ->
            connect_with_tcp(State, Options);
        _ ->
            connect_with_ssl(State, Options)
    end.

connect_with_tcp(State, Options) ->
    {ok, {AFamily, Addr}} = get_addr(State#state.host),
    TCPOptions = [AFamily] ++ get_tcp_options(State, Options),
    case gen_tcp:connect(Addr, State#state.port, TCPOptions, State#state.connect_timeout) of
        {ok, Socket} ->
            handle_connect(State, {ok, gen_tcp, Socket});
        {error, Reason} ->
            handle_connect_error(error, Reason)
    end.

connect_with_ssl(State, Options) ->
    _ = application:ensure_all_started(ssl),
    {ok, {AFamily, Addr}} = get_addr(State#state.host),
    TCPOptions = [AFamily] ++ get_tcp_options(State, Options),
    SslOptions = proplists:get_value(ssl_options , Options, []),
    case ssl:connect(Addr, State#state.port, TCPOptions ++ SslOptions, State#state.connect_timeout) of
        {ok, SSLSocket} ->
            handle_connect(State, {ok, ssl, SSLSocket});
        {error, Reason} ->
            handle_connect_error(error, Reason)
    end.

get_tcp_options(_State, Options) ->
    TcpOptions = proplists:get_value(tcp_options , Options, []),
    case TcpOptions of
        [] -> ?SOCKET_OPTS;
        _ -> TcpOptions
    end.

handle_connect(State, {ok, Transport, Socket}) ->
    case authenticate(Socket, State#state.password) of
        ok ->
            case select_database(Socket, State#state.database) of
                ok ->
                    erlang:put(transport, Transport),
                    {ok, State#state{socket = Socket}};
                {error, Reason} ->
                    {error, {select_error, Reason}}
            end;
        {error, Reason} ->
            handle_connect_error(error, {authentication_error, Reason})
    end.

handle_connect_error(error, Reason) ->
    {error, {connection_error, Reason}}.

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

select_database(_Socket, undefined) ->
    ok;
select_database(_Socket, <<"0">>) ->
    ok;
select_database(Socket, Database) ->
    do_sync_command(Socket, ["SELECT", " ", Database, "\r\n"]).

authenticate(_Socket, <<>>) ->
    ok;
authenticate(Socket, Password) ->
    do_sync_command(Socket, ["AUTH", " \"", Password, "\"\r\n"]).

%% @doc: Executes the given command synchronously, expects Redis to
%% return "+OK\r\n", otherwise it will fail.

set_transport_opts(Socket, Opts) ->
    case is_tuple(Socket) of
       true -> ok = ssl:setopts(Socket, Opts), ssl;
       false -> ok = inet:setopts(Socket, Opts), gen_tcp
    end.

do_sync_command(Socket, Command) ->
    Transport = set_transport_opts(Socket, [{active, false}]),
    case Transport:send(Socket, Command) of
        ok ->
            %% Hope there's nothing else coming down on the socket..
            case Transport:recv(Socket, 0, ?RECV_TIMEOUT) of
                {ok, <<"+OK\r\n">>} ->
                    set_transport_opts(Socket, [{active, once}]),
                    ok;
                Other ->
                    {error, {unexpected_data, Other}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
%% @doc: Loop until a connection can be established, this includes
%% successfully issuing the auth and select calls. When we have a
%% connection, give the socket to the redis client.
reconnect_loop(Client, #state{reconnect_sleep = ReconnectSleep} = State) ->
    Options = case erlang:get(options) of
        undefined -> [];
        Opts -> Opts
    end,
    case catch(connect(State, Options)) of
        {ok, #state{socket = Socket, host=Host, port=Port}} ->
            Transport = get_tranport(),
            Client ! {connection_ready, Socket, Host, Port},
            Transport:controlling_process(Socket, Client),
            Msgs = get_all_messages([]),
            [Client ! M || M <- Msgs];
        {error, _Reason} ->
            timer:sleep(ReconnectSleep),
            reconnect_loop(Client, State);
        %% Something bad happened when connecting, like Redis might be
        %% loading the dataset and we got something other than 'OK' in
        %% auth or select
        _ ->
            timer:sleep(ReconnectSleep),
            reconnect_loop(Client, State)
    end.

read_database(undefined) ->
    undefined;
read_database(Database) when is_integer(Database) ->
    list_to_binary(integer_to_list(Database)).


get_all_messages(Acc) ->
    receive
        M ->
            [M | Acc]
    after 0 ->
        lists:reverse(Acc)
    end.

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
    {ok, State#state{queue = queue:new()}};
start_reconnect(#state{socket=Socket} = State) ->
    Transport = get_tranport(),
    Transport:close(Socket),
    start_reconnect(State#state{socket=undefined}).

get_tranport() ->
    case erlang:get(transport) of
        undefined -> gen_tcp;
        Transport -> Transport
    end.
