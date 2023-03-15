%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(dgiot_udp_broadcast).
-author("johnliu").
-include("dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").

-behaviour(gen_server).
%% API
-export([start_link/1, send/3, do_connect/2, get_ipaddrs/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(connect_state, {host, port, mod, socket = undefined, freq = 30, count = 1000, child, reconnect_times = 3, reconnect_sleep = 30}).

-define(TIMEOUT, 10000).
-define(UDP_OPTIONS, [binary, {active, once}, {packet, raw}, {reuseaddr, true}, {send_timeout, ?TIMEOUT}]).
%%-define(UDP_OPTIONS, [binary, {reuseaddr, true}]).

start_link(Args) ->
    dgiot_client:start_link(?MODULE, Args).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([#{<<"channel">> := ChannelId, <<"client">> := ClientId, <<"ip">> := Ip, <<"port">> := Port, <<"mod">> := Mod} = Args]) ->
    Port1 = dgiot_utils:to_int(Port),
    UserData = #connect_state{mod = Mod, host = Ip, port = Port1, freq = 30, count = 300},
    ChildState = maps:get(<<"child">>, Args, #{}),
    Dclient = #dclient{channel = ChannelId, client = ClientId, status = ?DCLIENT_INTIALIZED, userdata = UserData, child = ChildState},
    dgiot_client:add(ChannelId, ClientId),
    case Mod:init(Dclient) of
        {ok, NewDclient} ->
            do_connect(false, NewDclient),
            {ok, NewDclient, hibernate};
        {stop, Reason} ->
            {stop, Reason}
    end.

handle_call({connection_ready, Socket}, _From, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod} = UserData} = Dclient) ->
    NewUserData = UserData#connect_state{socket = Socket},
    case Mod:handle_info(connection_ready, Dclient#dclient{userdata = NewUserData}) of
        {noreply, NewDclient} ->
            {reply, ok, NewDclient, hibernate};
        {stop, _Reason, NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),
            {reply, _Reason, NewDclient}
    end;

handle_call(Request, From, #dclient{channel = ChannelId, client = ClientId,
    userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_call(Request, From, Dclient) of
        {reply, Reply, NewDclient} ->
            {reply, Reply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),
            {reply, Reason, NewDclient}
    end.

handle_cast(Msg, #dclient{channel = ChannelId, client = ClientId,
    userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_cast(Msg, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),
            {reply, Reason, NewDclient}
    end.

%% 连接次数为0了
handle_info(do_connect, Dclient) ->
%%    ?LOG(info, "do_connect ~s:~p", [State#connect_state.host, State#connect_state.port]),
    {stop, normal, Dclient};

%% 连接次数为0了
handle_info(connect_stop, Dclient) ->
%%    ?LOG(info, "CONNECT CLOSE ~s:~p", [State#connect_state.host, State#connect_state.port]),
    {noreply, Dclient, hibernate};

handle_info({connection_ready, Socket}, #dclient{userdata = #connect_state{mod = Mod} = UserData} = Dclient) ->
%%    io:format("~s ~p connection_ready ~p ~n", [?FILE, ?LINE, Dclient]),
    NewUserData = UserData#connect_state{socket = Socket},
    case Mod:handle_info(connection_ready, Dclient#dclient{userdata = NewUserData}) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            {stop, Reason, NewDclient}
    end;

%% 往udp server 发送报文
handle_info(_, #dclient{userdata = #connect_state{socket = undefined}} = Dclient) ->
    {noreply, Dclient, hibernate};

handle_info({send, PayLoad}, #dclient{userdata = #connect_state{host = Ip, port = Port, socket = Socket}} = Dclient) ->
%%    io:format("~s ~p ~p send to ~p:~p : ~p ~n", [?FILE, ?LINE, self(), dgiot_utils:get_ip(Ip), Port, dgiot_utils:to_hex(PayLoad)]),
    gen_udp:send(Socket, Ip, Port, PayLoad),
    {noreply, Dclient, hibernate};

handle_info({ssl, _RawSock, Data}, Dclient) ->
    handle_info({ssl, _RawSock, Data}, Dclient);

handle_info({udp, Socket, Ip, Port, Binary} = _A, #dclient{userdata = #connect_state{socket = Socket, mod = Mod}} = Dclient) ->
%%    io:format("~s ~p ~p send from ~p:~p : ~p ~n", [?FILE, ?LINE, self(), dgiot_utils:get_ip(Ip), _Port, dgiot_utils:to_hex(Binary)]),
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);
            _ ->
                Binary
        end,
    case Mod:handle_info({udp, Ip, Port, NewBin}, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            {noreply, Reason, NewDclient, hibernate}
    end;

handle_info({udp_error, _Socket, _Reason}, Dclient) ->
    {noreply, Dclient, hibernate};

handle_info({udp_closed, _Sock}, #dclient{channel = ChannelId, client = ClientId,
    userdata = #connect_state{socket = Socket, mod = Mod}} = Dclient) ->
    gen_udp:close(Socket),
    case Mod:handle_info(udp_closed, Dclient) of
        {noreply, #dclient{userdata = Userdata} = NewDclient} ->
            NewDclient1 = NewDclient#dclient{userdata = Userdata#connect_state{socket = undefined}},
            case is_integer(Userdata#connect_state.reconnect_sleep) of
                false ->
                    dgiot_client:stop(ChannelId, ClientId),
                    {noreply, NewDclient, hibernate};
                true ->
                    Now = erlang:system_time(second),
                    Sleep =
                        case get(last_closed) of
                            Time when is_integer(Time) andalso Now - Time < Userdata#connect_state.reconnect_sleep ->
                                true;
                            _ ->
                                false
                        end,
                    put(last_closed, Now),
                    {noreply, do_connect(Sleep, NewDclient1), hibernate}
            end;
        {stop, _Reason, NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),
            {noreply, NewDclient, hibernate}
    end;

handle_info(Info, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod, socket = Socket}} = Dclient) ->
    case Mod:handle_info(Info, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, _Reason, NewDclient} ->
            gen_udp:close(Socket),
            timer:sleep(10),
            dgiot_client:stop(ChannelId, ClientId),
            {noreply, NewDclient, hibernate}
    end.

terminate(Reason, #connect_state{mod = Mod, child = ChildState}) ->
    Mod:terminate(Reason, ChildState).

code_change(OldVsn, #connect_state{mod = Mod, child = ChildState} = State, Extra) ->
    {ok, NewChildState} = Mod:code_change(OldVsn, ChildState, Extra),
    {ok, State#connect_state{child = NewChildState}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
send(ChannelId, ClientId, Payload) ->
    case dgiot_client:get(ChannelId, ClientId) of
        {ok, Pid} ->
            Pid ! {send, Payload};
        _ ->
            pass
    end.

do_connect(Sleep, #dclient{userdata = Connect_state} = State) ->
    Client = self(),
    spawn(
        fun() ->
            Sleep andalso timer:sleep(Connect_state#connect_state.reconnect_sleep * 1000),
            connect(Client, State)
        end),
    State.

connect(Client, #dclient{userdata = #connect_state{port = Port, reconnect_times = Times, reconnect_sleep = Sleep} = Connect_state} = State) ->
    case is_process_alive(Client) of
        true ->
            SocketConfig = [binary, {active, true}, {broadcast, true}],
            case gen_udp:open(Port, SocketConfig) of
                {ok, Socket} ->
                    case catch gen_server:call(Client, {connection_ready, Socket}, 5000) of
                        ok ->
                            gen_udp:controlling_process(Socket, Client);
                        _ ->
                            ok
                    end;
                {error, Reason} ->
                    case is_integer(Times) of
                        true when Times - 1 > 0 ->
                            Client ! {connection_error, Reason},
                            timer:sleep(Sleep * 1000),
                            connect(Client, State#dclient{userdata = Connect_state#connect_state{reconnect_times = Times - 1}});
                        false when is_atom(Times) ->
                            Client ! {connection_error, Reason},
                            timer:sleep(Sleep * 1000),
                            connect(Client, State);
                        _ ->
                            Client ! connect_stop
                    end
            end
    end.


get_ipaddrs() ->
    {ok, NetConfigs} = inet:getifaddrs(),
    lists:foldl(fun get_broadcast_addresses/2, [], NetConfigs).

get_broadcast_addresses(NetConfig, AlreadyFoundAddresses) ->
    case get_broadcast_address(NetConfig) of
        none -> AlreadyFoundAddresses;
        Address -> [Address | AlreadyFoundAddresses]
    end.

get_broadcast_address({_NetName, Opts}) ->
    proplists:get_value(broadaddr, Opts, none).
