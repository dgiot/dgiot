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

-module(dgiot_tcp_client).
-author("johnliu").
-include("dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").
-behaviour(gen_server).

%%%===================================================================
%%% API 说明
%%%===================================================================
%% 启动TCP客户端进程
-export([start_link/1, send/1, send/2, send/3]).
%% gen_server回调函数
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% 连接状态记录定义
%%% host:     TCP服务器主机名/IP
%%% port:     TCP服务器端口
%%% mod:      业务逻辑处理模块
%%% socket:   TCP连接套接字
%%% freq:     重连频率（秒）
%%% count:    最大重连次数
%%% reconnect_attempts: 当前重连尝试次数（用于指数退避）
%%% last_error: 最后错误信息
%%%===================================================================
-record(connect_state, {
    host, 
    port, 
    mod, 
    socket = undefined, 
    freq = 30, 
    count = 1000,
    reconnect_attempts = 0,  % 当前重连尝试次数
    last_error = undefined   % 最后错误信息
}).

%%%===================================================================
%%% 常量定义
%%% TIMEOUT:    TCP操作超时时间（毫秒）
%%% TCP_OPTIONS: TCP连接选项
%%%   binary     - 接收二进制数据
%%%   {active, once} - 单次主动接收模式
%%%   {packet, raw} - 原始数据包模式
%%%   {reuseaddr, false} - 禁用地址复用
%%%   {send_timeout, TIMEOUT} - 发送超时时间
%%%   {show_econnreset, true} - 区分正常关闭和连接中止（OTP-12843）
%%%   {keepalive, true} - 启用TCP保活机制
%%%   {nodelay, true} - 禁用Nagle算法，减少延迟
%%%===================================================================
-define(TIMEOUT, 10000).
-define(TCP_OPTIONS, [
    binary,
    {active, once},
    {packet, raw},
    {reuseaddr, false},
    {send_timeout, ?TIMEOUT},
    {show_econnreset, true},  % OTP-12843: 区分正常关闭和连接中止
    {keepalive, true},        % TCP保活机制
    {nodelay, true}           % 禁用Nagle算法
]).

%%%===================================================================
%%% API 实现
%%%===================================================================

%% 启动TCP客户端进程
%% Args: 包含客户端配置的map
%%   - channel: 通道ID
%%   - client:  客户端ID
%%   - ip:      服务器IP
%%   - port:    服务器端口
%%   - mod:     业务逻辑模块
start_link(Args) ->
    dgiot_client:start_link(?MODULE, Args).

%% 向自身进程管理的TCP连接发送数据
send(Payload) ->
    self() ! {send, Payload}.

%% 通过指定Socket发送数据（直接调用）
send(Socket, Payload) ->
    gen_tcp:send(Socket, Payload).

%% 向指定通道和客户端的TCP连接发送数据
send(ChannelId, ClientId, Payload) ->
    case dgiot_client:get(ChannelId, ClientId) of
        {ok, Pid} -> Pid ! {send, Payload};  %% 异步发送
        _ -> ok  %% 客户端不存在时忽略
    end.

%%%===================================================================
%%% gen_server 回调函数实现
%%%===================================================================

%% 初始化TCP客户端
%% 参数解析：
%%   - 从Args中提取连接参数和定时参数
%%   - 创建连接状态记录#connect_state
%%   - 创建时钟结构#dclock控制定时任务
%%   - 调用业务模块的init/1进行自定义初始化
init([#{<<"channel">> := ChannelId, <<"client">> := ClientId, <<"ip">> := Host, <<"port">> := Port, <<"mod">> := Mod} = Args]) ->
    %% 参数转换处理
    Ip = dgiot_utils:to_list(Host),
    Port1 = dgiot_utils:to_int(Port),
    UserData = #connect_state{mod = Mod, host = Ip, port = Port1, freq = 30, count = 300},
    
    %% 定时参数计算
    ChildState = maps:get(<<"child">>, Args, #{}),
    StartTime = dgiot_client:get_time(maps:get(<<"starttime">>, Args, dgiot_datetime:now_secs())),
    EndTime = dgiot_client:get_time(maps:get(<<"endtime">>, Args, dgiot_datetime:now_secs() + 1000000000)),
    Freq = maps:get(<<"freq">>, Args, 30),
    NextTime = dgiot_client:get_nexttime(StartTime, Freq),
    Count = dgiot_client:get_count(StartTime, EndTime, Freq),
    
    %% 随机延迟（避免所有客户端同时重连）
    Rand =
        case maps:get(<<"rand">>, Args, true) of
            true -> 0;
            _ -> dgiot_client:get_rand(Freq)
        end,
    
    %% 构建客户端状态记录
    Clock = #dclock{freq = Freq, nexttime = NextTime + Rand, count = Count, round = 0},
    Dclient = #dclient{channel = ChannelId, client = ClientId, status = ?DCLIENT_INTIALIZED, 
                      clock = Clock, userdata = UserData, child = ChildState},
    
    %% 注册客户端并初始化业务模块
    dgiot_client:add(ChannelId, ClientId),
    case Mod:init(Dclient) of
        {ok, NewDclient} ->
            do_connect(NewDclient),  %% 发起TCP连接
            {ok, NewDclient, hibernate};  %% 进程休眠节省资源
        {stop, Reason} ->
            {stop, Reason}  %% 初始化失败停止进程
    end.

%% 通用call请求处理（委托给业务模块）
handle_call(Request, From, #dclient{channel = ChannelId, client = ClientId, 
        userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_call(Request, From, Dclient) of
        {reply, Reply, NewDclient} ->
            {reply, Reply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),
            {reply, Reason, NewDclient}
    end.

%% 处理connection_ready cast消息
handle_cast({connection_ready, Socket}, #dclient{channel = ChannelId, client = ClientId,
    userdata = #connect_state{mod = Mod} = UserData} = Dclient) ->
    ?LOG(debug, "~ts: ~p", [<<"处理connection_ready cast消息"/utf8>>, Socket]),
    
    %% 更新连接状态
    NewUserData = UserData#connect_state{socket = Socket},
    
    %% 设置套接字选项
    try
        %% 设置套接字控制权
        gen_tcp:controlling_process(Socket, self()),
        
        %% 设置接收选项
        inet:setopts(Socket, [{active, once}]),
        
        %% 通知业务模块连接就绪
        case Mod:handle_info(connection_ready, Dclient#dclient{userdata = NewUserData}) of
            {noreply, NewDclient} ->
                {noreply, NewDclient, hibernate};
            {stop, Reason, NewDclient} ->
                dgiot_client:stop(ChannelId, ClientId),
                {stop, Reason, NewDclient}
        end
    catch
        _:Error ->
            ?LOG(debug, "~ts: ~p, ~p", [<<"设置套接字选项失败"/utf8>>, Socket, Error]),
            gen_tcp:close(Socket),
            self() ! do_connect,  %% 触发重连
            {noreply, Dclient, hibernate}
    end;

%% 通用cast消息处理（委托给业务模块）
handle_cast(Msg, #dclient{channel = ChannelId, client = ClientId,
    userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_cast(Msg, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),
            {stop, Reason, NewDclient}
    end.

%%%===================================================================
%%% 异步消息处理
%%%===================================================================

%% 重连次数耗尽处理
handle_info(do_connect, #dclient{channel = ChannelId, client = ClientId, 
        userdata = #connect_state{count = Count}} = Dclient) when Count =< 0 ->
    dgiot_client:stop(ChannelId, ClientId),  %% 停止客户端
    {noreply, Dclient, hibernate};

%% 定时重连处理（使用指数退避算法）
handle_info(do_connect, #dclient{userdata = #connect_state{count = Count, freq = Freq, reconnect_attempts = Attempts} = UserData} = Dclient) ->
    %% 指数退避算法：基础延迟 * 2^尝试次数，最大不超过300秒
    BaseDelay = Freq * 1000,
    MaxDelay = 300 * 1000,
    Delay = min(BaseDelay * trunc(math:pow(2, min(Attempts, 8))), MaxDelay),
    
    ?LOG(debug, "~ts: ~p秒后重连（尝试次数: ~p）", [<<"TCP重连延迟"/utf8>>, Delay div 1000, Attempts]),
    timer:sleep(Delay),
    
    NewUserData = UserData#connect_state{
        count = Count - 1,
        reconnect_attempts = Attempts + 1
    },
    NewDclient = Dclient#dclient{userdata = NewUserData},
    do_connect(NewDclient),  %% 发起新连接
    {noreply, NewDclient, hibernate};


%% 发送数据到TCP服务器（无连接时忽略）
handle_info({send, _PayLoad}, #dclient{userdata = #connect_state{socket = undefined}} = Dclient) ->
      ?LOG(debug, "tcp send ~p ~p ", [self(), _PayLoad]),
    {noreply, Dclient, hibernate};

%% 发送数据到TCP服务器
handle_info({send, PayLoad}, #dclient{userdata = #connect_state{socket = Socket}} = Dclient) ->
    ?LOG(info, "[TCP_SEND] Socket=~p | 报文=~w | 长度=~p", [Socket, PayLoad, byte_size(PayLoad)]),
    gen_tcp:send(Socket, PayLoad),  %% 同步发送
    dgiot_metrics:inc(dgiot, <<"tcpc_send">>, 1),  %% 更新发送统计
    {noreply, Dclient, hibernate};

%% 接收TCP服务器数据
handle_info({tcp, Socket, Binary}, #dclient{userdata = #connect_state{mod = Mod}} = Dclient) ->
    dgiot_metrics:inc(dgiot, <<"tcpc_recv">>, 1),  %% 更新接收统计
    ?LOG(info, "[TCP_RECV] Socket=~p | 报文=~w | 长度=~p", [Socket, Binary, byte_size(Binary)]),
    
    %% 二进制数据优化处理（防止内存碎片）
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);  %% 创建副本减少内存引用
            _ ->
                Binary
        end,
    
    %% 委托业务模块处理数据
    case Mod:handle_info({tcp, NewBin}, Dclient) of
        {noreply, NewDclient} ->
            inet:setopts(Socket, [{active, once}]),  %% 重新启用接收
            {noreply, NewDclient, hibernate};
        {stop, Reason, #dclient{channel = ChannelId, client = ClientId} = NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),  %% 业务模块要求停止
            {stop, Reason, NewDclient}
    end;

%% TCP错误处理（记录错误并继续运行）
handle_info({tcp_error, Socket, Reason}, #dclient{userdata = UserData} = Dclient) ->
    ?LOG(debug, "~ts: ~p, ~p", [<<"TCP错误"/utf8>>, Socket, Reason]),
    %% 更新最后错误信息
    NewUserData = UserData#connect_state{last_error = {tcp_error, Reason}},
    {noreply, Dclient#dclient{userdata = NewUserData}, hibernate};

%% TCP连接关闭处理（区分正常关闭和连接中止）
handle_info({tcp_closed, Socket}, #dclient{channel = ChannelId, client = ClientId, 
        userdata = #connect_state{mod = Mod} = UserData} = Dclient) ->
    dgiot_metrics:dec(dgiot, <<"tcpc_online">>, 1),  %% 更新在线统计
    
    %% 检查是否为连接中止（ECONNRESET）
    IsAborted = case UserData#connect_state.last_error of
        {tcp_error, econnreset} -> true;
        _ -> false
    end,
    
    ?LOG(debug, "~ts: ~p, ~ts", [<<"TCP连接关闭"/utf8>>, Socket, 
        case IsAborted of true -> <<"连接中止"/utf8>>; false -> <<"正常关闭"/utf8>> end]),
    
    gen_tcp:close(Socket),  %% 关闭套接字
    
    %% 重置重连尝试次数（连接已关闭，重新开始计数）
    NewUserData = UserData#connect_state{
        socket = undefined,
        reconnect_attempts = 0,
        last_error = undefined
    },
    
    case Mod:handle_info(tcp_closed, Dclient#dclient{userdata = NewUserData}) of
        {noreply, NewDclient} ->
            self() ! do_connect,  %% 触发重连
            {noreply, NewDclient, hibernate};
        {stop, _Reason, NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),
            {noreply, NewDclient, hibernate}
    end;

%% 其他异步消息处理（委托给业务模块）
handle_info(Info, #dclient{channel = ChannelId, client = ClientId, 
        userdata = #connect_state{mod = Mod, socket = Socket}} = Dclient) ->
    case Mod:handle_info(Info, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            gen_tcp:close(Socket),  %% 关闭TCP连接
            timer:sleep(10),
            dgiot_client:stop(ChannelId, ClientId),  %% 停止客户端进程
            {stop, Reason, NewDclient}
    end.

%% 进程终止处理
%%  - 更新在线统计
%%  - 调用业务模块的terminate/2
terminate(Reason, #dclient{userdata = #connect_state{mod = Mod}} = Dclient) ->
    dgiot_metrics:dec(dgiot, <<"tcpc_online">>, 1),
    Mod:terminate(Reason, Dclient).

%% 代码热升级处理（委托给业务模块）
code_change(OldVsn, #dclient{userdata = #connect_state{mod = Mod}} = Dclient, Extra) ->
    Mod:code_change(OldVsn, Dclient, Extra).

%%%===================================================================
%%% 内部辅助函数
%%%===================================================================

%% 发起TCP连接
%% 成功时：
%%   - 调用connection_ready通知业务模块
%%   - 设置套接字控制权
%%   - 重置重连尝试次数
%% 失败时：
%%   - 记录错误信息
%%   - 触发重连机制（使用指数退避）
do_connect(#dclient{userdata = #connect_state{host = Host, port = Port, mod = Mod} = UserData}) ->
    ?LOG(debug, "~ts: ~p:~p, ~p", [<<"尝试TCP连接"/utf8>>, Host, Port, Mod]),
    case gen_tcp:connect(Host, Port, ?TCP_OPTIONS, ?TIMEOUT) of
        {ok, Socket} ->
            ?LOG(debug, "~ts: ~p:~p, ~p", [<<"TCP连接成功"/utf8>>, Host, Port, Socket]),
            
            %% 连接成功，重置重连尝试次数
            _NewUserData = UserData#connect_state{
                socket = Socket,
                reconnect_attempts = 0,
                last_error = undefined
            },
            
            %% 使用cast而不是call，避免死锁和超时
            gen_server:cast(self(), {connection_ready, Socket}),
            ?LOG(debug, "~ts: ~p:~p", [<<"已发送connection_ready cast消息"/utf8>>, Host, Port]),
            
            %% 更新在线统计
            dgiot_metrics:inc(dgiot, <<"tcpc_online">>, 1);
            
        {error, Reason} ->
            ?LOG(debug, "~ts: ~p:~p, ~p", [<<"TCP连接失败"/utf8>>, Host, Port, Reason]),
            
            %% 记录错误信息
            _NewUserData = UserData#connect_state{last_error = {connect_error, Reason}},
            
            %% 连接失败触发重连
            self() ! do_connect
    end;

%% 非标准状态处理（触发重连）
do_connect(_) ->
    self() ! do_connect.