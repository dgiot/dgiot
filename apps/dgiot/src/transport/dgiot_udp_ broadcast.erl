%%%-------------------------------------------------------------------
%%% @author stoneliu
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 2月 2022 15:15
%%%-------------------------------------------------------------------
-module('dgiot_udp_ broadcast').
-author("stoneliu").

%% API
-export([start_link/0]).

-behaviour(gen_server).
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2
]).

-record(state, {
    message = get_message() :: binary(),
    port :: pos_integer(),
    broadcast_min = get_pos_integer_application_env(broadcast_min, 1) :: pos_integer(),
    broadcast_max = get_pos_integer_application_env(broadcast_max, 3) :: pos_integer(),
    timer_min = get_pos_integer_application_env(timer_min, 1000) :: pos_integer(),
    timer_max = get_pos_integer_application_env(timer_max, 3000) :: pos_integer(),
    socket :: gen_udp:socket(),
    timer_ref :: reference() | undefined % undefined only for record initialization
}).

-define(VERSION_TAG, <<1,0,0>>).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init(_) ->
    ok = net_kernel:monitor_nodes(true),
    ListenPort = get_pos_integer_application_env(discovery_port, 47808),
    {ok, App} = application:get_application(),
    ListenOptions = application:get_env(App, listen_options, [{ip, {0,0,0,0}}]),
    {ok, UdpSocket} = gen_udp:open(ListenPort, [{active, true} | ListenOptions]),
    {ok, set_timer(#state{port = ListenPort, socket = UdpSocket})}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call(_E, _From, State) ->
    {noreply, State}.

handle_info({timeout, TimerRef, broadcast}, #state{timer_ref = TimerRef, broadcast_min = BroadcastMin, broadcast_max = BroadcastMax} = State) ->
    {ok, App} = application:get_application(),
    DiscoveryAddresses = application:get_env(App, discovery_address, ["erlang-discovery"]),
    {noreply, set_timer(broadcast_info(State, DiscoveryAddresses, rand_between(BroadcastMin, BroadcastMax)))};
handle_info({udp, Socket, _Ip, _IpPortNo, Data}, #state{socket = Socket} = State) ->
    handle_msg(Data),
    {noreply, State};
handle_info({nodedown, Node}, State) ->
    logger:debug("Node ~p DOWN: Cluster node count is ~w", [Node, length(nodes())+1]),
    {noreply, State};
handle_info({nodeup, Node}, State) ->
    logger:debug("Node ~p UP: Cluster node count is ~w", [Node, length(nodes())+1]),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.


%%====================================================================
%% Internal functions
%%====================================================================

handle_msg(Msg) ->
    Tag = ?VERSION_TAG,
    TagSize = byte_size(?VERSION_TAG),
    case catch crypto:crypto_one_time(aes_256_ecb, get_key(), Msg, false) of
        <<Tag:TagSize/binary, NameByteSize:16/integer, Name:NameByteSize/binary, _/binary>> ->
            Node = binary_to_atom(Name, latin1),
            case lists:member(Node, [node() | nodes()]) of
                true -> ok;
                _ -> logger:info("Node ~p CONNECTING: ~p", [Name, net_adm:ping(Node)])
            end;
        _ -> error
    end.

broadcast_info(State, _DiscoveryAddresses, 0) ->
    State;
broadcast_info(#state{port = Port, message = Message} = State, DiscoveryAddresses, N) ->
    % In case the reverse proxy decides to maintain the UDP "connection" open
    % to the same upstream, use a different Socket to generate the broadcast
    % packages
    {ok, Socket} = gen_udp:open(0),
    [case gen_udp:send(Socket, DiscoveryAddress, Port, Message) of
         ok -> ok;
         {error, E} -> logger:warning("Unable to send UDP to ~s: ~p", [DiscoveryAddress, E])
     end || DiscoveryAddress <- DiscoveryAddresses],
    gen_udp:close(Socket),
    broadcast_info(State, DiscoveryAddresses, N - 1).

set_timer(#state{timer_min = TimerMin, timer_max = TimerMax} = State) ->
    % As connecting to any member of the cluster means connecting to the whole
    % cluster, reduce the chance of sending a message by multiplying by the size
    % of the cluster
    Time = (length(nodes()) + 1 ) * rand_between(TimerMin, TimerMax),
    State#state{timer_ref = erlang:start_timer(Time, self(), broadcast)}.

get_pos_integer_application_env(Key, Default) ->
    {ok, App} = application:get_application(),
    Value = application:get_env(App, Key, Default),
    true = is_integer(Value) andalso Value > 0,
    Value.

rand_between(A, A) -> A;
rand_between(A, B) when A < B -> A - 1 + rand:uniform(B + 1 - A);
rand_between(A, B) -> rand_between(B, A).

get_key() ->
    crypto:hash(sha256, atom_to_binary(erlang:get_cookie(), latin1)).

get_message() ->
    Name = atom_to_binary(node(), latin1),
    Data = <<?VERSION_TAG/binary, (byte_size(Name)):16/integer, Name/binary>>,
    DataWithPadding = case byte_size(Data) band 15 of
                          0 -> Data;
                          B -> <<Data/binary, (list_to_binary(lists:seq(1, 16 - B)))/binary>>
                      end,
    crypto:crypto_one_time(aes_256_ecb, get_key(), DataWithPadding, true).
