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

%% @doc 通用UDP通信模块
%% 支持UDP服务器功能和UDP广播客户端功能
-module(dgiot_udp_server).
-author("johnliu").
-include("dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").

%% API导出
-export([start_link/1, start_link/3, send/2, send/3, send/4]).
-export([do_connect/2, child_spec/3, child_spec/4]).
-export([join_multicast_group/2, leave_multicast_group/2, is_multicast_ip/1]).
-export([test_udp/0, test_udp_mutilcast/0, test_udp_client/0,test_udp_broadcast_client/0, test_udp_multilcast_client/0, test_suite/0]).

%% gen_server回调函数导出
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

%% 连接状态记录定义
-record(connect_state, {
    host,           % 目标主机地址
    port,           % 目标端口号
    mod,            % 业务逻辑模块
    socket = undefined, % UDP套接字
    transport,      % 传输协议模块
    freq = 30,      % 频率
    count = 1000,   % 计数
    child,          % 子模块状态
    reconnect_times = 3,    % 重连次数
    reconnect_sleep = 30,   % 重连间隔（秒）
    broadcast = false,      % 是否启用广播模式
    multicast = false,      % 是否启用多播模式
    multicast_groups = [],  % 多播组列表
    broadcast_addrs = [],   % 广播地址列表
    mode = client          % 模式: client | server
}).

%% 服务器状态记录定义
-record(state, {
    sock,           % 套接字
    chid,           % 通道ID
    responder,      % 响应器
    options,        % 选项
    mod,            % 业务逻辑模块
    incoming_bytes = 0, % 接收字节数
    child = #udp{}  % 子状态
}).

-define(TIMEOUT, 10000).
-define(UDP_OPTIONS, [binary, {active, once}, {packet, raw}, {reuseaddr, true}, {send_timeout, ?TIMEOUT}]).
-define(UDP_BROADCAST_OPTIONS, [binary, {active, once}, {packet, raw}, {broadcast, true}, {reuseaddr, true}, {send_timeout, ?TIMEOUT}]).
-define(UDP_MULTICAST_OPTIONS, [binary, {active, once}, {packet, raw}, {reuseaddr, true}, {multicast_ttl, 4}, {send_timeout, ?TIMEOUT}]).
-define(SOCKOPTS, [binary, {reuseaddr, true}]).

%% @doc 启动UDP客户端或服务器
%% Args: 包含通道ID、客户端ID、IP、端口和业务模块的映射
start_link(Args) ->
    dgiot_client:start_link(?MODULE, Args).

%% @doc 创建子进程规格
child_spec(Mod, Port, State) ->
    child_spec(Mod, Port, State, []).

child_spec(Mod, Port, State, Opts) ->
    Name = Mod,
    ok = esockd:start(),
    case dgiot_transport:get_opts(udp, Port) of
        {ok, DefActiveN, DefRateLimit, UDPOpts} ->
            ActiveN = proplists:get_value(active_n, Opts, DefActiveN),
            RateLimit = proplists:get_value(rate_limit, Opts, DefRateLimit),
            Opts1 = lists:foldl(fun(Key, Acc) -> proplists:delete(Key, Acc) end, Opts, [active_n, rate_limit]),
            NewOpts = [{active_n, ActiveN}, {rate_limit, RateLimit}] ++ Opts1,
            MFArgs = {?MODULE, start_link, [Mod, NewOpts, State]},
            esockd:udp_child_spec(Name, Port, UDPOpts, MFArgs);
        _ ->
            []
    end.

%% @doc 启动UDP服务器
start_link(Mod, Opts, State) ->
    gen_server:start_link(?MODULE, [server, Mod, Opts, State], []).

%% @doc 初始化UDP服务器或客户端
init([server, Mod, Opts, State]) ->
    process_flag(trap_exit, true),
    Port = proplists:get_value(port, Opts, 0),
    Multicast = proplists:get_value(multicast, Opts, false),
    MulticastGroups = proplists:get_value(multicast_groups, Opts, []),
    
    % 根据是否多播选择不同的UDP选项
    UdpOptions = case Multicast of
        true -> ?UDP_MULTICAST_OPTIONS;
        false -> ?UDP_OPTIONS
    end,
    
    case gen_udp:open(Port, UdpOptions) of
        {ok, Socket} ->
            % 加入多播组（如果配置了多播）
            lists:foreach(fun(Group) ->
                join_multicast_group(Socket, Group)
            end, MulticastGroups),
            
            ChildState = #udp{socket = Socket, register = false, transport = gen_udp, state = State},
            case Mod:init(ChildState) of
                {ok, NewChildState} ->
                    GState = #state{
                        sock = Socket,
                        options = Opts,
                        mod = Mod,
                        child = NewChildState
                    },
                    dgiot_metrics:inc(dgiot_bridge, <<"udp_server">>, 1),
                    {ok, GState};
                {error, Reason} ->
                    gen_udp:close(Socket),
                    {stop, Reason}
            end;
        {error, Reason} ->
            {stop, Reason}
    end;

%% @doc 初始化UDP客户端
init([#{<<"channel">> := ChannelId, <<"client">> := ClientId, <<"ip">> := Host, <<"port">> := Port, <<"mod">> := Mod} = Args]) ->
    Mode = maps:get(<<"mode">>, Args, client), % client | server
    Transport = gen_udp,
    Ip = dgiot_utils:to_list(Host),
    Port1 = dgiot_utils:to_int(Port),
    
    % 检查是否启用广播模式
    Broadcast = maps:get(<<"broadcast">>, Args, false),
    
    % 检查是否启用多播模式
    Multicast = maps:get(<<"multicast">>, Args, false),
    MulticastGroups = maps:get(<<"multicast_groups">>, Args, []),
    
    % 获取广播地址（如果启用广播模式）
    BroadcastAddrs = case Broadcast of
        true -> get_broadcast_addrs();
        false -> []
    end,
    
    UserData = #connect_state{
        mod = Mod, 
        host = Ip, 
        port = Port1, 
        freq = 30, 
        count = 300, 
        transport = Transport,
        broadcast = Broadcast,
        multicast = Multicast,
        multicast_groups = MulticastGroups,
        broadcast_addrs = BroadcastAddrs,
        mode = Mode
    },
    
    ChildState = maps:get(<<"child">>, Args, #{}),
    StartTime = dgiot_client:get_time(maps:get(<<"starttime">>, Args, dgiot_datetime:now_secs())),
    EndTime = dgiot_client:get_time(maps:get(<<"endtime">>, Args, dgiot_datetime:now_secs() + 1000000000)),
    Freq = maps:get(<<"freq">>, Args, 30),
    NextTime = dgiot_client:get_nexttime(StartTime, Freq),
    Count = dgiot_client:get_count(StartTime, EndTime, Freq),
    Rand =
        case maps:get(<<"rand">>, Args, true) of
            true -> 0;
            _ -> dgiot_client:get_rand(Freq)
        end,
    Clock = #dclock{freq = Freq, nexttime = NextTime + Rand, count = Count, round = 0},
    Dclient = #dclient{channel = ChannelId, client = ClientId, status = ?DCLIENT_INTIALIZED, clock = Clock, userdata = UserData, child = ChildState},
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
        {stop, _Reason, NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),
            {noreply, NewDclient}
    end.

%% 连接次数为0了
handle_info(do_connect, Dclient) ->
    {stop, normal, Dclient};

%% 连接次数为0了
handle_info(connect_stop, Dclient) ->
    {noreply, Dclient, hibernate};

handle_info({connection_ready, Socket}, #dclient{userdata = #connect_state{mod = Mod, multicast = Multicast, multicast_groups = Groups} = UserData} = Dclient) ->
    NewUserData = UserData#connect_state{socket = Socket},
    
    % 加入多播组（如果启用多播模式）
    case Multicast of
        true ->
            lists:foreach(fun(Group) ->
                join_multicast_group(Socket, Group)
            end, Groups);
        false ->
            ok
    end,
    
    case Mod:handle_info(connection_ready, Dclient#dclient{userdata = NewUserData}) of
        {noreply, NewDclient} ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            {stop, Reason, NewDclient}
    end;

%% 往udp server 发送报文
handle_info({send, _PayLoad}, #dclient{userdata = #connect_state{socket = undefined}} = Dclient) ->
    {noreply, Dclient, hibernate};

handle_info({send, PayLoad}, #dclient{userdata = #connect_state{host = Ip, port = Port, transport = Transport, socket = Socket, broadcast = Broadcast, multicast = Multicast, broadcast_addrs = Addrs}} = Dclient) ->
    case {Broadcast, Multicast} of
        {true, _} ->
            % 广播模式：发送到所有广播地址
            lists:foreach(fun(Addr) ->
                Transport:send(Socket, Addr, Port, PayLoad)
            end, Addrs);
        {_, true} ->
            % 多播模式：发送到多播地址
            Transport:send(Socket, Ip, Port, PayLoad);
        {false, false} ->
            % 普通模式：发送到指定地址
            Transport:send(Socket, Ip, Port, PayLoad)
    end,
    {noreply, Dclient, hibernate};

handle_info({send, PayLoad, Opts}, #dclient{userdata = #connect_state{host = Ip, port = Port, transport = Transport, socket = Socket, broadcast_addrs = Addrs}} = Dclient) ->
    % 支持发送选项，例如指定是否广播或多播
    case proplists:get_value(broadcast, Opts, false) of
        true ->
            % 发送到所有广播地址
            lists:foreach(fun(Addr) ->
                Transport:send(Socket, Addr, Port, PayLoad)
            end, Addrs);
        false ->
            % 检查是否多播发送
            case proplists:get_value(multicast, Opts, false) of
                true ->
                    % 多播发送
                    Transport:send(Socket, Ip, Port, PayLoad);
                false ->
                    % 单播发送
                    Transport:send(Socket, Ip, Port, PayLoad)
            end
    end,
    {noreply, Dclient, hibernate};

%% 处理UDP数据报（服务器模式）
handle_info({udp, _Socket, Ip, InPortNo, Data}, #state{mod = Mod, child = #udp{register = false, buff = Buff} = ChildState} = State) ->
    dgiot_metrics:inc(dgiot_bridge, <<"udp_server_recv">>, 1),
    Binary = iolist_to_binary(Data),
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);
            _ ->
                Binary
        end,
    Cnt = byte_size(NewBin),
    NewChildState = ChildState#udp{buff = <<>>},
    
    % 检查是否是多播地址 (224.0.0.0 - 239.255.255.255)
    IsMulticast = is_multicast_ip(Ip),
    
    case Mod:handle_info({udp, Ip, InPortNo, <<Buff/binary, NewBin/binary>>, IsMulticast}, NewChildState) of
        {noreply, #udp{register = true, clientid = ClientId} = NewChild} ->
            dgiot_cm:register_channel(ClientId, self(), #{conn_mod => Mod}),
            dgiot_cm:insert_channel_info(ClientId, #{ip => Ip, port => InPortNo, online => dgiot_datetime:now_microsecs(), multicast => IsMulticast}, [{udp_recv, 1}]),
            {noreply, State#state{child = NewChild, incoming_bytes = Cnt}, hibernate};
        {noreply, NewChild} ->
            {noreply, State#state{child = NewChild, incoming_bytes = Cnt}, hibernate};
        {stop, Reason, NewChild} ->
            {stop, Reason, State#state{child = NewChild}}
    end;

handle_info({udp, _Socket, Ip, InPortNo, Data}, #state{mod = Mod, child = #udp{buff = Buff} = ChildState} = State) ->
    dgiot_metrics:inc(dgiot_bridge, <<"udp_server_recv">>, 1),
    Binary = iolist_to_binary(Data),
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);
            _ ->
                Binary
        end,
    Cnt = byte_size(NewBin),
    NewChildState = ChildState#udp{buff = <<>>},
    
    % 检查是否是多播地址
    IsMulticast = is_multicast_ip(Ip),
    
    case NewChildState of
        #udp{clientid = ClientId, register = true} ->
            dgiot_device:online(ClientId),
            dgiot_tracer:check_trace(ClientId, ClientId, dgiot_utils:binary_to_hex(Binary), ?MODULE, ?LINE);
        _ ->
            pass
    end,
    case Mod:handle_info({udp, Ip, InPortNo, <<Buff/binary, NewBin/binary>>, IsMulticast}, NewChildState) of
        {noreply, NewChild} ->
            {noreply, State#state{child = NewChild, incoming_bytes = Cnt}, hibernate};
        {stop, Reason, NewChild} ->
            {stop, Reason, State#state{child = NewChild}}
    end;

%% 处理接收到的UDP数据（客户端模式）
handle_info({udp, Socket, Ip, Port, Binary} = _A, #dclient{userdata = #connect_state{socket = Socket, mod = Mod}} = Dclient) ->
    % 处理大二进制数据，避免内存问题
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);
            _ ->
                Binary
        end,
    
    % 检查是否是多播地址
    IsMulticast = is_multicast_ip(Ip),
    
    % 调用业务模块处理接收到的UDP数据，传递多播信息
    case Mod:handle_info({udp, Ip, Port, NewBin, IsMulticast}, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, _Reason, NewDclient} ->
            {noreply, NewDclient}
    end;

handle_info({udp_error, _Sock, Reason}, #state{child = _ChildState} = State) ->
    ?LOG(error, "UDP error: ~p", [Reason]),
    {stop, {shutdown, Reason}, State};

%% 处理UDP错误
handle_info({udp_error, _Socket, _Reason}, Dclient) ->
    ?LOG(error, "UDP error: ~p", [_Reason]),
    {noreply, Dclient, hibernate};

%% 处理UDP连接关闭
handle_info({udp_closed, _Sock}, #dclient{channel = ChannelId, client = ClientId,
    userdata = #connect_state{socket = Socket, mod = Mod, reconnect_sleep = ReconnectSleep, multicast_groups = Groups} = _UserData} = Dclient) ->
    % 离开多播组（如果已加入）
    lists:foreach(fun(Group) ->
        leave_multicast_group(Socket, Group)
    end, Groups),
    
    % 关闭套接字
    gen_udp:close(Socket),
    
    % 调用业务模块处理连接关闭事件
    case Mod:handle_info(udp_closed, Dclient) of
        {noreply, #dclient{userdata = Userdata} = NewDclient} ->
            % 更新连接状态，清空套接字
            NewDclient1 = NewDclient#dclient{userdata = Userdata#connect_state{socket = undefined}},
            case is_integer(ReconnectSleep) of
                false ->
                    % 如果不支持重连，停止客户端
                    dgiot_client:stop(ChannelId, ClientId),
                    {noreply, NewDclient1, hibernate};
                true ->
                    % 计算是否需要休眠后再重连
                    Now = erlang:system_time(second),
                    Sleep =
                        case get(last_closed) of
                            Time when is_integer(Time) andalso Now - Time < ReconnectSleep ->
                                true;
                            _ ->
                                false
                        end,
                    put(last_closed, Now),
                    % 启动重连过程
                    {noreply, do_connect(Sleep, NewDclient1), hibernate}
            end;
        {stop, _Reason, NewDclient} ->
            % 停止客户端
            dgiot_client:stop(ChannelId, ClientId),
            {noreply, NewDclient, hibernate}
    end;

%% 处理关闭消息
handle_info({shutdown, Reason}, #state{child = #udp{clientid = ClientId, register = true, socket = Socket} = ChildState} = State) ->
    ?LOG(error, "shutdown, ~p, ~p~n", [Reason, ChildState#udp.state]),
    % 离开多播组（如果已加入）
    MulticastGroups = proplists:get_value(multicast_groups, State#state.options, []),
    lists:foreach(fun(Group) ->
        leave_multicast_group(Socket, Group)
    end, MulticastGroups),
    
    dgiot_cm:unregister_channel(ClientId),
    dgiot_device:offline(ClientId),
    {stop, normal, State#state{child = ChildState#udp{socket = undefined}}};

handle_info({shutdown, Reason}, #state{child = #udp{socket = Socket} = ChildState} = State) ->
    ?LOG(error, "shutdown, ~p, ~p~n", [Reason, ChildState#udp.state]),
    % 离开多播组（如果已加入）
    MulticastGroups = proplists:get_value(multicast_groups, State#state.options, []),
    lists:foreach(fun(Group) ->
        leave_multicast_group(Socket, Group)
    end, MulticastGroups),
    
    {stop, normal, State#state{child = ChildState#udp{socket = undefined}}};

handle_info({udp_closed, Sock}, #state{mod = Mod, child = #udp{socket = Sock} = ChildState} = State) ->
    ?LOG(error, "udp_closed ~p", [ChildState#udp.state]),
    % 离开多播组（如果已加入）
    MulticastGroups = proplists:get_value(multicast_groups, State#state.options, []),
    lists:foreach(fun(Group) ->
        leave_multicast_group(Sock, Group)
    end, MulticastGroups),
    
    case Mod:handle_info(udp_closed, ChildState) of
        {noreply, NewChild} ->
            {stop, normal, State#state{child = NewChild#udp{socket = undefined}}};
        {stop, _Reason, NewChild} ->
            {stop, normal, State#state{child = NewChild#udp{socket = undefined}}}
    end;

handle_info(Info, #state{mod = Mod, child = ChildState} = State) ->
    io:format("~s ~p Info: ~p~n", [?FILE, ?LINE, Info]),
    case Mod:handle_info(Info, ChildState) of
        {noreply, NewChildState} ->
            {noreply, State#state{child = NewChildState}, hibernate};
        {stop, Reason, NewChildState} ->
            {stop, Reason, State#state{child = NewChildState}}
    end;

handle_info(Info, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod, socket = Socket, multicast_groups = Groups}} = Dclient) ->
    case Mod:handle_info(Info, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, _Reason, NewDclient} ->
            % 离开多播组（如果已加入）
            lists:foreach(fun(Group) ->
                leave_multicast_group(Socket, Group)
            end, Groups),
            
            % 关闭套接字并停止客户端
            gen_udp:close(Socket),
            timer:sleep(10),
            dgiot_client:stop(ChannelId, ClientId),
            {noreply, NewDclient, hibernate}
    end;

handle_info({timeout, _TrId, _Event}, State) ->
    {noreply, State, hibernate};

% handle_info({request_complete, #coap_message{token = _Token, id = _Id}}, State) ->
%     {noreply, State, hibernate};

handle_info({'EXIT', Resp, Reason}, State = #state{responder = Resp}) ->
    ?LOG(info, "channel received exit from responder: ~p, reason: ~p", [Resp, Reason]),
    {stop, Reason, State};

handle_info({'EXIT', _Pid, _Reason}, State = #state{}) ->
    ?LOG(error, "channel received exit from stranger: ~p, reason: ~p", [_Pid, _Reason]),
    {noreply, State, hibernate};

handle_info(Info, State) ->
    ?LOG(warning, "unexpected massage ~p~n", [Info]),
    {noreply, State, hibernate}.

terminate(Reason, #dclient{userdata = #connect_state{mod = Mod, child = ChildState, socket = Socket, multicast_groups = Groups}}) ->
    % 离开多播组（如果已加入）
    lists:foreach(fun(Group) ->
        leave_multicast_group(Socket, Group)
    end, Groups),
    
    Mod:terminate(Reason, ChildState);

terminate(Reason, #state{mod = Mod, child = #udp{clientid = ClientId, register = true, socket = Socket} = ChildState}) ->
    % 离开多播组（如果已加入）
    MulticastGroups = proplists:get_value(multicast_groups, ChildState#udp.state, []),
    lists:foreach(fun(Group) ->
        leave_multicast_group(Socket, Group)
    end, MulticastGroups),
    
    dgiot_cm:unregister_channel(ClientId),
    dgiot_metrics:dec(dgiot_bridge, <<"udp_server">>, 1),
    Mod:terminate(Reason, ChildState);

terminate(Reason, #state{mod = Mod, child = #udp{socket = Socket} = ChildState}) ->
    % 离开多播组（如果已加入）
    MulticastGroups = proplists:get_value(multicast_groups, ChildState#udp.state, []),
    lists:foreach(fun(Group) ->
        leave_multicast_group(Socket, Group)
    end, MulticastGroups),
    
    dgiot_metrics:dec(dgiot_bridge, <<"udp_server">>, 1),
    Mod:terminate(Reason, ChildState).

code_change(OldVsn, #dclient{userdata = #connect_state{mod = Mod, child = ChildState}} = State, Extra) ->
    {ok, NewChildState} = Mod:code_change(OldVsn, ChildState, Extra),
    {ok, State#dclient{userdata = State#dclient.userdata#connect_state{child = NewChildState}}};

code_change(OldVsn, #state{mod = Mod, child = ChildState} = State, Extra) ->
    {ok, NewChildState} = Mod:code_change(OldVsn, ChildState, Extra),
    {ok, State#state{child = NewChildState}}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 发送数据
send(#udp{clientid = ClientId, register = true, transport = Transport, socket = Socket}, Payload) ->
    dgiot_tracer:check_trace(ClientId, ClientId, dgiot_utils:binary_to_hex(Payload), ?MODULE, ?LINE),
    dgiot_metrics:inc(dgiot_bridge, <<"udp_server_send">>, 1),
    case Socket == undefined of
        true ->
            {error, disconnected};
        false ->
            Transport:send(Socket, Payload)
    end;

send(#udp{transport = Transport, socket = Socket}, Payload) ->
    dgiot_metrics:inc(dgiot_bridge, <<"udp_server_send">>, 1),
    case Socket == undefined of
        true ->
            {error, disconnected};
        false ->
            Transport:send(Socket, Payload)
    end.

%% @doc 发送数据到指定通道和客户端
%% ChannelId: 通道ID
%% ClientId: 客户端ID
%% Payload: 要发送的数据
send(ChannelId, ClientId, Payload) ->
    case dgiot_client:get(ChannelId, ClientId) of
        {ok, Pid} ->
            Pid ! {send, Payload};
        _ ->
            pass
    end.

%% @doc 发送数据到指定通道和客户端（带选项）
%% ChannelId: 通道ID
%% ClientId: 客户端ID
%% Payload: 要发送的数据
%% Opts: 发送选项，如[{broadcast, true}]
send(ChannelId, ClientId, Payload, Opts) ->
    case dgiot_client:get(ChannelId, ClientId) of
        {ok, Pid} ->
            Pid ! {send, Payload, Opts};
        _ ->
            pass
    end.

%% @doc 启动连接过程
%% Sleep: 是否需要先休眠
%% State: 客户端状态
do_connect(Sleep, #dclient{userdata = Connect_state} = State) ->
    Client = self(),
    spawn(
        fun() ->
            Sleep andalso timer:sleep(Connect_state#connect_state.reconnect_sleep * 1000),
            connect(Client, State)
        end),
    State.

%% @doc 建立UDP连接
connect(Client, #dclient{userdata = #connect_state{host = Host, port = Port, reconnect_times = Times, reconnect_sleep = Sleep, broadcast = Broadcast, multicast = Multicast} = Connect_state} = State) ->
    case is_process_alive(Client) of
        true ->
            % 根据是否广播或多播模式选择不同的UDP选项
            UdpOptions = case {Broadcast, Multicast} of
                {true, _} -> ?UDP_BROADCAST_OPTIONS;
                {_, true} -> ?UDP_MULTICAST_OPTIONS;
                _ -> ?UDP_OPTIONS
            end,
            
            case gen_udp:open(0, UdpOptions) of
                {ok, Socket} ->
                    case {Broadcast, Multicast} of
                        {true, _} ->
                            % 广播模式：不建立连接，直接使用套接字
                            case catch gen_server:call(Client, {connection_ready, Socket}, 5000) of
                                ok ->
                                    inet:setopts(Socket, [{active, once}]),
                                    gen_udp:controlling_process(Socket, Client);
                                _ ->
                                    ok
                            end;
                        {_, true} ->
                            % 多播模式：加入多播组
                            lists:foreach(fun(Group) ->
                                join_multicast_group(Socket, Group)
                            end, Connect_state#connect_state.multicast_groups),
                            
                            case catch gen_server:call(Client, {connection_ready, Socket}, 5000) of
                                ok ->
                                    inet:setopts(Socket, [{active, once}]),
                                    gen_udp:controlling_process(Socket, Client);
                                _ ->
                                    ok
                            end;
                        _ ->
                            % 普通模式：建立连接
                            case gen_udp:connect(Socket, dgiot_utils:to_list(Host), Port) of
                                ok ->
                                    case catch gen_server:call(Client, {connection_ready, Socket}, 5000) of
                                        ok ->
                                            inet:setopts(Socket, [{active, once}]),
                                            gen_udp:controlling_process(Socket, Client);
                                        _ ->
                                            ok
                                    end;
                                {error, Reason} ->
                                    handle_connect_error(Client, Reason, Times, Sleep, Connect_state, State)
                            end
                    end;
                {error, Reason} ->
                    handle_connect_error(Client, Reason, Times, Sleep, Connect_state, State)
            end
    end.

%% @doc 处理连接错误
handle_connect_error(Client, Reason, Times, Sleep, Connect_state, State) ->
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
    end.

%% @doc 获取所有网络接口的广播地址
get_broadcast_addrs() ->
    {ok, Interfaces} = inet:getifaddrs(),
    lists:foldl(fun get_broadcast_addresses/2, [], Interfaces).

%% @doc 获取网络接口的广播地址
get_broadcast_addresses(NetConfig, AlreadyFoundAddresses) ->
    case get_broadcast_address(NetConfig) of
        none -> AlreadyFoundAddresses;
        Address -> [Address | AlreadyFoundAddresses]
    end.

%% @doc 从网络配置中提取广播地址
get_broadcast_address({_NetName, Opts}) ->
    proplists:get_value(broadaddr, Opts, none).

%% @doc 检查IP地址是否为多播地址
is_multicast_ip(Ip) when is_tuple(Ip) andalso tuple_size(Ip) =:= 4 ->
    {A, B, C, D} = Ip,
    (A >= 224 andalso A =< 239) orelse
    (A =:= 239 andalso B =:= 255 andalso C =:= 255 andalso D =:= 255);
is_multicast_ip(_) ->
    false.

%% @doc 加入多播组
join_multicast_group(Socket, MulticastGroup) when is_tuple(MulticastGroup) ->
    case inet:getifaddrs() of
        {ok, Interfaces} ->
            lists:foreach(fun({_IfName, Opts}) ->
                case proplists:get_value(addr, Opts) of
                    {A, _B, _C, _D} = LocalIp when A =/= 127 ->
                        try
                            inet:setopts(Socket, [{add_membership, {MulticastGroup, LocalIp}}]),
                            ?LOG(info, "Joined multicast group ~p on interface ~p", [MulticastGroup, LocalIp])
                        catch
                            _:Error ->
                                ?LOG(error, "Failed to join multicast group ~p on interface ~p: ~p", [MulticastGroup, LocalIp, Error])
                        end;
                    _ ->
                        ok
                end
            end, Interfaces);
        {error, Reason} ->
            ?LOG(error, "Failed to get network interfaces: ~p", [Reason])
    end;
join_multicast_group(Socket, MulticastGroup) when is_list(MulticastGroup) ->
    case inet:parse_address(MulticastGroup) of
        {ok, Ip} -> join_multicast_group(Socket, Ip);
        {error, Reason} -> ?LOG(error, "Invalid multicast address ~p: ~p", [MulticastGroup, Reason])
    end.

%% @doc 离开多播组
leave_multicast_group(Socket, MulticastGroup) when is_tuple(MulticastGroup) ->
    case inet:getifaddrs() of
        {ok, Interfaces} ->
            lists:foreach(fun({_IfName, Opts}) ->
                case proplists:get_value(addr, Opts) of
                    {A, _B, _C, _D} = LocalIp when A =/= 127 ->
                        try
                            inet:setopts(Socket, [{drop_membership, {MulticastGroup, LocalIp}}]),
                            ?LOG(info, "Left multicast group ~p on interface ~p", [MulticastGroup, LocalIp])
                        catch
                            _:Error ->
                                ?LOG(error, "Failed to leave multicast group ~p on interface ~p: ~p", [MulticastGroup, LocalIp, Error])
                        end;
                    _ ->
                        ok
                end
            end, Interfaces);
        {error, Reason} ->
            ?LOG(error, "Failed to get network interfaces: ~p", [Reason])
    end;
leave_multicast_group(Socket, MulticastGroup) when is_list(MulticastGroup) ->
    case inet:parse_address(MulticastGroup) of
        {ok, Ip} -> leave_multicast_group(Socket, Ip);
        {error, Reason} -> ?LOG(error, "Invalid multicast address ~p: ~p", [MulticastGroup, Reason])
    end.

 

% 1. 普通UDP服务器模式

%% @doc UDP测试桩函数
%% 调用dgiot_udp_sdk模块实现
test_udp() ->
    case dgiot_udp_sdk:start_server(8888) of
        {ok, ServerRef} ->
            case dgiot_udp_sdk:send(ServerRef, <<"Test">>) of
                ok -> 
                    ?LOG(info, "UDP test initiated"),
                    {ok, test_initiated};
                Error ->
                    ?LOG(error, "UDP send failed: ~p", [Error]),
                    Error
            end;
        Error ->
            ?LOG(error, "UDP server start failed: ~p", [Error]),
            Error
    end.

%% @doc 完整的UDP测试套件
%% @doc 测试套件桩函数
test_suite() ->
    ?LOG(info, "Starting UDP test suite (stub)"),
    case dgiot_udp_sdk:test_available() of
        true -> 
            ?LOG(info, "UDP SDK available, tests can be run"),
            {ok, sdk_available};
        false ->
            ?LOG(warning, "UDP SDK not available"),
            {error, sdk_not_available}
    end.


% 2. UDP多播服务器模式

test_udp_mutilcast()->
    % 启动多播UDP服务器
    Opts = [
    {port, 8888},
    {multicast, true},
    {multicast_groups, ["226.0.0.80"]}
    ],
    State = #udp{},
    {ok, _Pid} = dgiot_udp_server:start_link(your_business_module, Opts, State).

% 3. UDP客户端模式
test_udp_client() ->
% 启动UDP客户端
    Args = #{
    <<"channel">> => <<"channel1">>,
    <<"client">> => <<"client1">>,
    <<"ip">> => "192.168.1.100",
    <<"port">> => 8888,
    <<"mod">> => your_business_module,
    <<"broadcast">> => false,
    <<"multicast">> => false
    },
    {ok, _Pid} = dgiot_udp_server:start_link(Args),

    % 发送数据
    dgiot_udp_server:send(<<"channel1">>, <<"client1">>, <<"Hello">>).

    % 4. UDP广播客户端模式
 test_udp_broadcast_client() ->
    % 启动广播UDP客户端
    Args = #{
    <<"channel">> => <<"channel2">>,
    <<"client">> => <<"client2">>,
    <<"ip">> => "192.168.1.255",  % 广播地址
    <<"port">> => 8888,
    <<"mod">> => your_business_module,
    <<"broadcast">> => true
    },
    {ok, _Pid} = dgiot_udp_server:start_link(Args),

    % 发送广播数据
    dgiot_udp_server:send(<<"channel2">>, <<"client2">>, <<"Broadcast Message">>).

    % 5. UDP多播客户端模式
test_udp_multilcast_client() ->
        % 启动多播UDP客户端
    Args = #{
    <<"channel">> => <<"channel3">>,
    <<"client">> => <<"client3">>,
    <<"ip">> => "226.0.0.80",  % 多播地址
    <<"port">> => 8001,
    <<"mod">> => your_business_module,
    <<"multicast">> => true,
    <<"multicast_groups">> => ["226.0.0.80"]
    },
    {ok, _Pid} = dgiot_udp_server:start_link(Args),

    % 发送多播数据
    dgiot_udp_server:send(<<"channel3">>, <<"client3">>, <<"Multicast Message">>).
