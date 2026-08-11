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

<<<<<<< HEAD
%% @doc 增强版UDP服务器模块
%% 修复多播组加入问题
-module(dgiot_udp_server).
-author("johnliu").
-include("logger.hrl").
=======
%% @doc 精简UDP服务器模块
%% 应用层入口，专注于服务器启动和配置
-module(dgiot_udp_server).
-author("johnliu").
-include("../../include/logger.hrl").
>>>>>>> origin/dgaiot-plugins

%% API导出
-export([start_link/1, start_link/3, child_spec/3, child_spec/4, get_status/1]).
-export([join_multicast_group/2, leave_multicast_group/2, send/2, send/3, send/4, send_multicast/4, get_available_multicast_groups/0]).
-export([start_multicast_server/2, start_multicast_server/1, get_multicast_status/1, broadcast_to_groups/3, stop/1]).
<<<<<<< HEAD
-export([ensure_multicast_joined/2, debug_socket/1]).  % 新增导出
=======
>>>>>>> origin/dgaiot-plugins

%%%===================================================================
%%% API函数
%%%===================================================================

%% @doc 启动UDP服务器
start_link(Args) ->
<<<<<<< HEAD
    ?LOG(error, "[UDP_SERVER] 启动参数: ~p", [Args]),
=======
>>>>>>> origin/dgaiot-plugins
    dgiot_udp_session:start_link([{mode, server} | Args]).

%% @doc 启动UDP服务器（兼容旧接口）
start_link(Mod, Opts, State) ->
<<<<<<< HEAD
    ?LOG(error, "[UDP_SERVER] 启动: 模块=~p, 选项=~p", [Mod, Opts]),
    
    BaseArgs = [
        {mode, server},
        {mod, Mod},
        {state, State}
    ],
    
    Args = case Opts of
        OptsMap when is_map(OptsMap) ->
            BaseArgs ++ [OptsMap];
        OptsList when is_list(OptsList) ->
            BaseArgs ++ [{options, OptsList}];
        _ ->
            BaseArgs ++ [{options, Opts}]
    end,
    
    Result = dgiot_udp_session:start_link(Args),
    ?LOG(error, "[UDP_SERVER] 启动结果: ~p", [Result]),
    Result.
=======
    Args = [
        {mode, server},
        {mod, Mod},
        {options, Opts},
        {state, State}
    ],
    dgiot_udp_session:start_link(Args).
>>>>>>> origin/dgaiot-plugins

%% @doc 创建子进程规格
child_spec(Mod, Port, State) ->
    child_spec(Mod, Port, State, []).

child_spec(Mod, Port, State, Opts) ->
    Name = Mod,
    ok = esockd:start(),
<<<<<<< HEAD
    
    ?LOG(error, "[UDP_SERVER_CHILD_SPEC] 开始创建UDP子进程规格"),
    ?LOG(error, "[UDP_SERVER_CHILD_SPEC] 模块: ~p, 端口: ~p", [Mod, Port]),
    ?LOG(error, "[UDP_SERVER_CHILD_SPEC] 选项: ~p", [Opts]),
    
    % 详细检查多播选项
    MulticastGroups = proplists:get_value(multicast_groups, Opts, []),
    ?LOG(error, "[UDP_SERVER_CHILD_SPEC] 多播组列表: ~p", [MulticastGroups]),
    
    case dgiot_transport:get_opts(udp, Port) of
        {ok, DefActiveN, DefRateLimit, UDPOpts} ->
            ?LOG(error, "[UDP_SERVER_CHILD_SPEC] 获取UDP选项成功"),
            
=======
    io:format("~s ~p Creating UDP child spec for port ~p.~n", [?FILE, ?LINE, Port]),
    
    case dgiot_transport:get_opts(udp, Port) of
        {ok, DefActiveN, DefRateLimit, UDPOpts} ->
>>>>>>> origin/dgaiot-plugins
            ActiveN = proplists:get_value(active_n, Opts, DefActiveN),
            RateLimit = proplists:get_value(rate_limit, Opts, DefRateLimit),
            Opts1 = lists:foldl(fun(Key, Acc) -> proplists:delete(Key, Acc) end, Opts, [active_n, rate_limit]),
            NewOpts = [{active_n, ActiveN}, {rate_limit, RateLimit}] ++ Opts1,
            
<<<<<<< HEAD
            % 提取并处理多播选项
            {MulticastGroupsFinal, FinalUDPOpts} = extract_multicast_options(NewOpts, UDPOpts),
            
            ?LOG(error, "[UDP_SERVER_CHILD_SPEC] 最终多播组: ~p", [MulticastGroupsFinal]),
            ?LOG(error, "[UDP_SERVER_CHILD_SPEC] 最终UDP选项: ~p", [FinalUDPOpts]),
            
            % 检查是否有多播组配置
            case MulticastGroupsFinal of
                [] ->
                    ?LOG(warning, "[UDP_SERVER_CHILD_SPEC] 警告: 没有配置多播组!");
                _ ->
                    ?LOG(error, "[UDP_SERVER_CHILD_SPEC] 信息: 配置了多播组: ~p", [MulticastGroupsFinal])
            end,
            
            % 构建参数 - 修复：确保多播组配置正确传递
            OptionsMap = maps:from_list(NewOpts),
            OptionsMapWithGroups = OptionsMap#{multicast_groups => MulticastGroupsFinal},
            
            ?LOG(error, "[UDP_SERVER_CHILD_SPEC] 传递给start_link的选项: ~p", [OptionsMapWithGroups]),
            
            % 关键修复：确保多播组配置被正确传递
            MFArgs = {?MODULE, start_link, [Mod, OptionsMapWithGroups, State]},
            
            ChildSpec = esockd:udp_child_spec(Name, Port, FinalUDPOpts, MFArgs),
            
            ?LOG(error, "[UDP_SERVER_CHILD_SPEC] 最终子进程规格: ~p", [ChildSpec]),
            
            ?LOG(error, "[UDP_SERVER_CHILD_SPEC] ✅ 子进程规格创建完成"),
            ChildSpec;
        Error ->
            ?LOG(error, "[UDP_SERVER_CHILD_SPEC] ❌ 获取UDP选项失败: ~p", [Error]),
            []

    
    end.

%% @doc 确保加入多播组（新增函数）
ensure_multicast_joined(ServerPid, MulticastGroup) ->
    ?LOG(error, "[ENSURE_MULTICAST] 确保加入多播组: ~p", [MulticastGroup]),
    
    % 首先尝试通过标准API加入
    case join_multicast_group(ServerPid, MulticastGroup) of
        ok ->
            ?LOG(error, "[ENSURE_MULTICAST] ✅ 标准API加入成功");
        {error, Reason} ->
            ?LOG(error, "[ENSURE_MULTICAST] ❌ 标准API失败: ~p", [Reason]),
            
            % 尝试直接获取socket并操作
            case get_socket_from_pid(ServerPid) of
                {ok, Socket} ->
                    ?LOG(error, "[ENSURE_MULTICAST] 获取到socket: ~p", [Socket]),
                    
                    % 解析多播地址
                    case inet:parse_address(MulticastGroup) of
                        {ok, MulticastIP} ->
                            ?LOG(error, "[ENSURE_MULTICAST] 解析多播IP: ~p", [MulticastIP]),
                            
                            % 使用多种方法尝试加入
                            join_with_multiple_methods(Socket, MulticastIP);
                        {error, ParseError} ->
                            ?LOG(error, "[ENSURE_MULTICAST] 解析地址失败: ~p", [ParseError]),
                            {error, ParseError}
                    end;
                {error, SocketError} ->
                    ?LOG(error, "[ENSURE_MULTICAST] 获取socket失败: ~p", [SocketError]),
                    {error, SocketError}
            end
    end.

%% @doc 调试socket信息
debug_socket(ServerPid) ->
    case get_status(ServerPid) of
        {ok, Status} ->
            ?LOG(error, "[DEBUG_SOCKET] 服务器状态: ~p", [Status]),
            
            % 获取socket选项
            case get_socket_from_pid(ServerPid) of
                {ok, Socket} ->
                    ?LOG(error, "[DEBUG_SOCKET] Socket: ~p", [Socket]),
                    
                    % 检查socket选项
                    OptsToCheck = [
                        active, reuseaddr, multicast_loop, multicast_ttl,
                        recbuf, sndbuf, ip, ifaddr
                    ],
                    
                    case inet:getopts(Socket, OptsToCheck) of
                        {ok, SocketOpts} ->
                            ?LOG(error, "[DEBUG_SOCKET] Socket选项: ~p", [SocketOpts]);
                        {error, OptsError} ->
                            ?LOG(error, "[DEBUG_SOCKET] 获取选项失败: ~p", [OptsError])
                    end;
                {error, Error} ->
                    ?LOG(error, "[DEBUG_SOCKET] 获取socket失败: ~p", [Error])
            end;
        Error ->
            ?LOG(error, "[DEBUG_SOCKET] 获取状态失败: ~p", [Error])
    end,
    ok.

=======
            MFArgs = {?MODULE, start_link, [Mod, NewOpts, State]},
            esockd:udp_child_spec(Name, Port, UDPOpts, MFArgs);
        _ ->
            []
    end.

>>>>>>> origin/dgaiot-plugins
%% @doc 获取服务器状态
get_status(ServerPid) ->
    dgiot_udp_session:get_status(ServerPid).

%% @doc 加入多播组
join_multicast_group(ServerPid, MulticastGroup) ->
<<<<<<< HEAD
    ?LOG(error, "[JOIN_MULTICAST_GROUP] 加入多播组: ~p", [MulticastGroup]),
    Result = dgiot_udp_session:join_multicast_group(ServerPid, MulticastGroup),
    ?LOG(error, "[JOIN_MULTICAST_GROUP] 结果: ~p", [Result]),
    Result.
=======
    dgiot_udp_session:join_multicast_group(ServerPid, MulticastGroup).
>>>>>>> origin/dgaiot-plugins

%% @doc 离开多播组
leave_multicast_group(ServerPid, MulticastGroup) ->
    dgiot_udp_session:leave_multicast_group(ServerPid, MulticastGroup).

%% @doc 发送数据（使用已连接的套接字）
send(ServerPid, Data) ->
    dgiot_udp_session:send(ServerPid, Data).

%% @doc 发送数据到指定地址（兼容旧接口）
send(ServerPid, Addr, Data) ->
    dgiot_udp_session:send(ServerPid, Addr, Data).

%% @doc 发送数据到指定地址和端口
send(ServerPid, Addr, Port, Data) ->
    dgiot_udp_session:send(ServerPid, Addr, Port, Data).

%% @doc 发送多播消息
send_multicast(ServerPid, MulticastGroup, Port, Message) ->
    dgiot_udp_session:send_multicast(ServerPid, MulticastGroup, Port, Message).

%% @doc 获取可用的多播组列表
get_available_multicast_groups() ->
    dgiot_udp_multicast:get_available_multicast_groups().

%% @doc 启动带多播组的服务器
start_multicast_server(Port, MulticastGroups) when is_list(MulticastGroups) ->
<<<<<<< HEAD
    ?LOG(error, "[START_MULTICAST_SERVER] 启动多播服务器: 端口=~p, 组=~p", 
         [Port, MulticastGroups]),
=======
    io:format("~s ~p Event = Starting multicast server on port ~p with groups: ~p~n", 
              [?FILE, ?LINE, Port, MulticastGroups]),
>>>>>>> origin/dgaiot-plugins
    
    Args = [
        {port, Port},
        {multicast_groups, MulticastGroups}
    ],
    
    case start_link(Args) of
        {ok, Pid} ->
            % 加入指定的多播组
            lists:foreach(fun(Group) ->
<<<<<<< HEAD
                case ensure_multicast_joined(Pid, Group) of
                    ok ->
                        ?LOG(error, "[START_MULTICAST_SERVER] ✅ 加入多播组: ~p", [Group]);
                    Error ->
                        ?LOG(error, "[START_MULTICAST_SERVER] ❌ 加入失败 ~p: ~p", [Group, Error])
=======
                case join_multicast_group(Pid, Group) of
                    ok ->
                        io:format("~s ~p Event = SUCCESS: Joined multicast group ~p~n", 
                                 [?FILE, ?LINE, Group]);
                    Error ->
                        io:format("~s ~p Event = WARNING: Failed to join multicast group ~p: ~p~n", 
                                 [?FILE, ?LINE, Group, Error])
>>>>>>> origin/dgaiot-plugins
                end
            end, MulticastGroups),
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc 启动多播服务器并加入所有可用多播组
start_multicast_server(Port) ->
    MulticastGroups = get_available_multicast_groups(),
    start_multicast_server(Port, MulticastGroups).

%% @doc 获取服务器多播状态
get_multicast_status(ServerPid) ->
    case get_status(ServerPid) of
        {ok, Status} ->
            MulticastGroups = proplists:get_value(multicast_groups, Status, []),
            {ok, [
                {server_pid, ServerPid},
                {multicast_groups_joined, MulticastGroups},
                {total_groups, length(MulticastGroups)},
                {status, running}
            ]};
        Error ->
            Error
    end.

%% @doc 发送多播消息到所有已加入的组
broadcast_to_groups(ServerPid, Port, Message) ->
    case get_status(ServerPid) of
        {ok, Status} ->
            MulticastGroups = proplists:get_value(multicast_groups, Status, []),
<<<<<<< HEAD
            ?LOG(error, "[BROADCAST] 广播到 ~p 个组", [length(MulticastGroups)]),
            
            Results = lists:map(fun(Group) ->
                case send_multicast(ServerPid, Group, Port, Message) of
                    ok -> 
                        ?LOG(error, "[BROADCAST] ✅ 发送到组: ~p", [Group]),
                        {Group, success};
                    Error -> 
                        ?LOG(error, "[BROADCAST] ❌ 发送失败 ~p: ~p", [Group, Error]),
=======
            io:format("~s ~p Event = Broadcasting message to ~p groups on port ~p~n", 
                     [?FILE, ?LINE, length(MulticastGroups), Port]),
            Results = lists:map(fun(Group) ->
                case send_multicast(ServerPid, Group, Port, Message) of
                    ok -> 
                        io:format("~s ~p Event = ✓ Broadcast to group ~p successful~n", 
                                 [?FILE, ?LINE, Group]),
                        {Group, success};
                    Error -> 
                        io:format("~s ~p Event = ✗ Broadcast to group ~p failed: ~p~n", 
                                 [?FILE, ?LINE, Group, Error]),
>>>>>>> origin/dgaiot-plugins
                        {Group, Error}
                end
            end, MulticastGroups),
            {ok, Results};
        Error ->
            Error
    end.

%% @doc 停止服务器
stop(ServerPid) ->
    gen_server:stop(ServerPid).
<<<<<<< HEAD

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 提取多播选项并转换为UDP选项 - 修复版
extract_multicast_options(NewOpts, UDPOpts) ->
    MulticastGroups = proplists:get_value(multicast_groups, NewOpts, []),
    
    ?LOG(error, "[EXTRACT_MULTICAST] 原始多播组: ~p", [MulticastGroups]),
    ?LOG(error, "[EXTRACT_MULTICAST] 原始NewOpts: ~p", [NewOpts]),
    
    case MulticastGroups of
        [] ->
            % 没有多播组
            ?LOG(warning, "[EXTRACT_MULTICAST] 警告：多播组列表为空！"),
            {[], UDPOpts};
        _ ->
            ?LOG(error, "[EXTRACT_MULTICAST] ✅ 找到多播组: ~p", [MulticastGroups]),
            
            % 获取默认网络接口IP（动态检测）
            InterfaceIP = case dgiot_udp_multicast:get_default_interface() of
                {ok, IP} -> 
                    ?LOG(error, "[EXTRACT_MULTICAST] 使用接口IP: ~p", [IP]),
                    IP;
                {error, Reason} -> 
                    ?LOG(error, "[EXTRACT_MULTICAST] 获取接口IP失败: ~p，使用0.0.0.0", [Reason]),
                    {0,0,0,0}
            end,
            
            % 构建多播相关的UDP选项 - 修复：确保所有多播选项都正确传递
            MulticastUDPOpts = [
                {multicast_ttl, proplists:get_value(multicast_ttl, NewOpts, 32)},
                {multicast_loop, proplists:get_value(multicast_loop, NewOpts, true)},
                {reuseaddr, proplists:get_value(reuseaddr, NewOpts, true)},
                {broadcast, proplists:get_value(broadcast, NewOpts, false)},
                {ip, InterfaceIP},  % 修复：使用动态检测的接口IP
                
                % 添加更多多播选项以确保正确加入多播组
                {multicast_if, InterfaceIP},
                {add_membership, {hd(MulticastGroups), InterfaceIP}}  % 添加多播组成员关系
            ],
            
            ?LOG(error, "[EXTRACT_MULTICAST] 多播UDP选项: ~p", [MulticastUDPOpts]),
            
            % 合并UDP选项 - 修复：确保不覆盖重要的UDP选项
            FinalUDPOpts = lists:foldl(fun({Key, Value}, Acc) ->
                case lists:keymember(Key, 1, Acc) of
                    true -> 
                        ?LOG(debug, "[EXTRACT_MULTICAST] 选项已存在: ~p", [Key]),
                        Acc;
                    false -> 
                        ?LOG(debug, "[EXTRACT_MULTICAST] 添加选项: ~p = ~p", [Key, Value]),
                        [{Key, Value} | Acc]
                end
            end, UDPOpts, MulticastUDPOpts),  % 注意：这里顺序改为UDPOpts在前
            
            ?LOG(error, "[EXTRACT_MULTICAST] 最终UDP选项: ~p", [FinalUDPOpts]),
            ?LOG(error, "[EXTRACT_MULTICAST] 返回多播组: ~p", [MulticastGroups]),
            
            {MulticastGroups, FinalUDPOpts}
    end.

%% @doc 从进程获取socket
get_socket_from_pid(Pid) ->
    try
        % 尝试获取进程信息
        case process_info(Pid, dictionary) of
            {dictionary, Dict} ->
                case lists:keyfind('$socket', 1, Dict) of
                    {'$socket', Socket} ->
                        {ok, Socket};
                    false ->
                        {error, no_socket_in_dict}
                end;
            undefined ->
                {error, no_process_dict}
        end
    catch
        _:Error ->
            {error, Error}
    end.

%% @doc 使用多种方法尝试加入多播组
join_with_multiple_methods(Socket, MulticastIP) ->
    ?LOG(error, "[JOIN_MULTIPLE_METHODS] 尝试多种方法加入多播组: ~p", [MulticastIP]),
    
    Methods = [
        % 方法1: 传统方式
        {method1, fun() -> 
            inet:setopts(Socket, [{add_membership, {MulticastIP, {0,0,0,0}}}])
        end},
        % 方法2: 使用enp3s0接口
        {method2, fun() -> 
            InterfaceIP = get_interface_ip("enp3s0"),
            inet:setopts(Socket, [{add_membership, {MulticastIP, InterfaceIP}}])
        end},
        % 方法3: 使用回环接口
        {method3, fun() -> 
            inet:setopts(Socket, [{add_membership, {MulticastIP, {127,0,0,1}}}])
        end},
        % 方法4: 新API格式
        {method4, fun() -> 
            IpMreq = #{multiaddr => MulticastIP, interface => {0,0,0,0}},
            inet:setopts(Socket, [{add_membership, IpMreq}])
        end}
    ],
    
    lists:foldl(fun({MethodName, MethodFun}, Acc) ->
        case Acc of
            ok -> ok;  % 已经成功
            _ ->
                ?LOG(error, "[JOIN_MULTIPLE_METHODS] 尝试方法: ~p", [MethodName]),
                case MethodFun() of
                    ok ->
                        ?LOG(error, "[JOIN_MULTIPLE_METHODS] ✅ 方法 ~p 成功", [MethodName]),
                        
                        % 设置其他多播选项
                        inet:setopts(Socket, [
                            {multicast_loop, true},
                            {multicast_ttl, 32}
                        ]),
                        
                        ok;
                    {error, ealready} ->
                        ?LOG(error, "[JOIN_MULTIPLE_METHODS] ℹ️ 已经加入多播组"),
                        ok;
                    {error, Reason} ->
                        ?LOG(error, "[JOIN_MULTIPLE_METHODS] ❌ 方法 ~p 失败: ~p", 
                             [MethodName, Reason]),
                        {error, Reason}
                end
        end
    end, {error, not_tried}, Methods).

%% @doc 获取接口IP
get_interface_ip(IfName) ->
    case inet:getifaddrs() of
        {ok, Interfaces} ->
            case lists:keyfind(IfName, 1, Interfaces) of
                {IfName, Props} ->
                    case proplists:get_value(addr, Props) of
                        {A,_B,_C,_D} = Addr when A =/= 127 ->
                            Addr;
                        _ -> {0,0,0,0}
                    end;
                false ->
                    {0,0,0,0}
            end;
        _ ->
            {0,0,0,0}
    end.
=======
>>>>>>> origin/dgaiot-plugins
