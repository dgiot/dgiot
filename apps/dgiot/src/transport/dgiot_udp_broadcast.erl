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

%% @doc 通用UDP广播服务框架模块
%% 提供UDP广播通信的基础功能，支持自定义业务逻辑模块
-module(dgiot_udp_broadcast).
-author("johnliu").
-include("dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").

%% 实现gen_server行为模式
-behaviour(gen_server).

%% API导出
-export([start_link/1, send/3, do_connect/2, get_ipaddrs/0]).
%% gen_server回调函数导出
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% 连接状态记录定义
-record(connect_state, {
    host,           % 目标主机地址
    port,           % 目标端口号
    mod,            % 业务逻辑模块
    socket = undefined, % UDP套接字
    freq = 30,      % 频率（未使用）
    count = 1000,   % 计数（未使用）
    child,          % 子模块状态
    reconnect_times = 3,    % 重连次数
    reconnect_sleep = 30    % 重连间隔（秒）
}).

%% 定义超时时间和UDP选项
-define(TIMEOUT, 10000).
-define(UDP_OPTIONS, [binary, {active, once}, {packet, raw}, {broadcast, true}, {reuseaddr, true}, {send_timeout, ?TIMEOUT}]).

%% @doc 启动UDP广播客户端
%% Args: 包含通道ID、客户端ID、IP、端口和业务模块的映射
start_link(Args) ->
    dgiot_client:start_link(?MODULE, Args).

%%%===================================================================
%%% gen_server回调函数实现
%%%===================================================================

%% @doc 初始化UDP广播客户端
%% 参数: [包含通道ID、客户端ID、IP、端口和业务模块的映射]
init([#{<<"channel">> := ChannelId, <<"client">> := ClientId, <<"ip">> := Ip, <<"port">> := Port, <<"mod">> := Mod} = Args]) ->
    % 转换端口号为整数
    Port1 = dgiot_utils:to_int(Port),
    % 获取子模块状态，默认为空映射
    ChildState = maps:get(<<"child">>, Args, #{}),
    % 初始化连接状态记录
    UserData = #connect_state{mod = Mod, host = Ip, port = Port1, freq = 30, count = 300, child = ChildState},
    % 创建客户端状态记录
    Dclient = #dclient{channel = ChannelId, client = ClientId, status = ?DCLIENT_INTIALIZED, userdata = UserData, child = ChildState},
    % 添加客户端到管理器
    dgiot_client:add(ChannelId, ClientId),
    % 调用业务模块的初始化函数
    case Mod:init(Dclient) of
        {ok, NewDclient} ->
            % 启动连接过程
            do_connect(false, NewDclient),
            {ok, NewDclient, hibernate};
        {stop, Reason} ->
            {stop, Reason}
    end.

%% @doc 处理连接就绪的同步调用
%% 当UDP套接字创建成功后调用此函数
handle_call({connection_ready, Socket}, _From, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod} = UserData} = Dclient) ->
    % 更新连接状态中的套接字信息
    NewUserData = UserData#connect_state{socket = Socket},
    % 调用业务模块处理连接就绪事件
    case Mod:handle_info(connection_ready, Dclient#dclient{userdata = NewUserData}) of
        {noreply, NewDclient} ->
            {reply, ok, NewDclient, hibernate};
        {stop, _Reason, NewDclient} ->
            % 停止客户端
            dgiot_client:stop(ChannelId, ClientId),
            {reply, _Reason, NewDclient}
    end;

%% @doc 处理其他同步调用
%% 将调用转发给业务模块处理
handle_call(Request, From, #dclient{channel = ChannelId, client = ClientId,
    userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_call(Request, From, Dclient) of
        {reply, Reply, NewDclient} ->
            {reply, Reply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            % 停止客户端
            dgiot_client:stop(ChannelId, ClientId),
            {reply, Reason, NewDclient}
    end.

%% @doc 处理异步调用
%% 将调用转发给业务模块处理
handle_cast(Msg, #dclient{channel = ChannelId, client = ClientId,
    userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_cast(Msg, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            % 停止客户端
            dgiot_client:stop(ChannelId, ClientId),
            {reply, Reason, NewDclient}
    end.

%% @doc 处理连接指令
%% 当连接次数为0时停止服务
handle_info(do_connect, Dclient) ->
    {stop, normal, Dclient};

%% @doc 处理连接停止指令
%% 保持当前状态不变
handle_info(connect_stop, Dclient) ->
    {noreply, Dclient, hibernate};

%% @doc 处理连接就绪消息
%% 更新套接字信息并通知业务模块
handle_info({connection_ready, Socket}, #dclient{userdata = #connect_state{mod = Mod} = UserData} = Dclient) ->
    NewUserData = UserData#connect_state{socket = Socket},
    case Mod:handle_info(connection_ready, Dclient#dclient{userdata = NewUserData}) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            {stop, Reason, NewDclient}
    end;

%% @doc 处理发送请求（当套接字未定义时）
%% 忽略发送请求，保持休眠状态
handle_info(_, #dclient{userdata = #connect_state{socket = undefined}} = Dclient) ->
    {noreply, Dclient, hibernate};

%% @doc 处理发送请求
%% 向指定主机和端口发送UDP数据
handle_info({send, PayLoad}, #dclient{userdata = #connect_state{host = Ip, port = Port, socket = Socket}} = Dclient) ->
    gen_udp:send(Socket, Ip, Port, PayLoad),
    {noreply, Dclient, hibernate};

%% @doc 处理SSL消息（未实现）
%% 目前只是转发给自身处理
handle_info({ssl, _RawSock, Data}, Dclient) ->
    handle_info({ssl, _RawSock, Data}, Dclient);

%% @doc 处理接收到的UDP数据
%% 将数据转发给业务模块处理
handle_info({udp, Socket, Ip, Port, Binary} = _A, #dclient{userdata = #connect_state{socket = Socket, mod = Mod}} = Dclient) ->
    % 处理大二进制数据，避免内存问题
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);
            _ ->
                Binary
        end,
    % 调用业务模块处理接收到的UDP数据
    case Mod:handle_info({udp, Ip, Port, NewBin}, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            {noreply, Reason, NewDclient, hibernate}
    end;

%% @doc 处理UDP错误
%% 忽略错误，保持当前状态
handle_info({udp_error, _Socket, _Reason}, Dclient) ->
    {noreply, Dclient, hibernate};

%% @doc 处理UDP连接关闭
%% 关闭套接字并尝试重连
handle_info({udp_closed, _Sock}, #dclient{channel = ChannelId, client = ClientId,
    userdata = #connect_state{socket = Socket, mod = Mod}} = Dclient) ->
    % 关闭套接字
    gen_udp:close(Socket),
    % 调用业务模块处理连接关闭事件
    case Mod:handle_info(udp_closed, Dclient) of
        {noreply, #dclient{userdata = Userdata} = NewDclient} ->
            % 更新连接状态，清空套接字
            NewDclient1 = NewDclient#dclient{userdata = Userdata#connect_state{socket = undefined}},
            case is_integer(Userdata#connect_state.reconnect_sleep) of
                false ->
                    % 如果不支持重连，停止客户端
                    dgiot_client:stop(ChannelId, ClientId),
                    {noreply, NewDclient, hibernate};
                true ->
                    % 计算是否需要休眠后再重连
                    Now = erlang:system_time(second),
                    Sleep =
                        case get(last_closed) of
                            Time when is_integer(Time) andalso Now - Time < Userdata#connect_state.reconnect_sleep ->
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

%% @doc 处理其他信息
%% 将信息转发给业务模块处理
handle_info(Info, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod, socket = Socket}} = Dclient) ->
    case Mod:handle_info(Info, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, _Reason, NewDclient} ->
            % 关闭套接字并停止客户端
            gen_udp:close(Socket),
            timer:sleep(10),
            dgiot_client:stop(ChannelId, ClientId),
            {noreply, NewDclient, hibernate}
    end.

%% @doc 清理资源
%% 调用业务模块的terminate函数
terminate(Reason, #dclient{userdata = #connect_state{mod = Mod, child = ChildState}}) ->
    Mod:terminate(Reason, ChildState).

%% @doc 代码热更新
%% 调用业务模块的code_change函数
code_change(OldVsn, #dclient{userdata = #connect_state{mod = Mod, child = ChildState}} = State, Extra) ->
    {ok, NewChildState} = Mod:code_change(OldVsn, ChildState, Extra),
    {ok, State#dclient{userdata = State#dclient.userdata#connect_state{child = NewChildState}}}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

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

%% @doc 启动连接过程
%% Sleep: 是否需要先休眠
%% State: 客户端状态
do_connect(Sleep, #dclient{userdata = Connect_state} = State) ->
    Client = self(),
    spawn(
        fun() ->
            % 如果需要休眠，先等待指定时间
            Sleep andalso timer:sleep(Connect_state#connect_state.reconnect_sleep * 1000),
            connect(Client, State)
        end),
    State.

%% @doc 建立UDP连接
%% Client: 客户端进程ID
%% State: 客户端状态
connect(Client, #dclient{userdata = #connect_state{port = Port, reconnect_times = Times, reconnect_sleep = Sleep} = Connect_state} = State) ->
    case is_process_alive(Client) of
        true ->
            % 配置UDP套接字选项
            SocketConfig = [binary, {active, true}, {broadcast, true}],
            case gen_udp:open(Port, SocketConfig) of
                {ok, Socket} ->
                    % 通知客户端连接就绪
                    case catch gen_server:call(Client, {connection_ready, Socket}, 5000) of
                        ok ->
                            % 将套接字控制权转移给客户端进程
                            gen_udp:controlling_process(Socket, Client);
                        _ ->
                            ok
                    end;
                {error, Reason} ->
                    % 处理连接错误，尝试重连
                    case is_integer(Times) of
                        true when Times - 1 > 0 ->
                            % 还有重连次数，减少次数并重试
                            Client ! {connection_error, Reason},
                            timer:sleep(Sleep * 1000),
                            connect(Client, State#dclient{userdata = Connect_state#connect_state{reconnect_times = Times - 1}});
                        false when is_atom(Times) ->
                            % 无限重试模式
                            Client ! {connection_error, Reason},
                            timer:sleep(Sleep * 1000),
                            connect(Client, State);
                        _ ->
                            % 重连次数用完，停止连接
                            Client ! connect_stop
                    end
            end
    end.

%% @doc 获取所有网络接口的IP地址
get_ipaddrs() ->
    {ok, NetConfigs} = inet:getifaddrs(),
    lists:foldl(fun get_broadcast_addresses/2, [], NetConfigs).

%% @doc 获取网络接口的广播地址
%% NetConfig: 网络配置
%% AlreadyFoundAddresses: 已找到的地址列表
get_broadcast_addresses(NetConfig, AlreadyFoundAddresses) ->
    case get_broadcast_address(NetConfig) of
        none -> AlreadyFoundAddresses;
        Address -> [Address | AlreadyFoundAddresses]
    end.

%% @doc 从网络配置中提取广播地址
%% {NetName, Opts}: 网络名称和选项列表
get_broadcast_address({_NetName, Opts}) ->
    proplists:get_value(broadaddr, Opts, none).