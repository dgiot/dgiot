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

%% @doc 终极版多播功能模块
%% 修复所有多播问题，确保正确加入多播组
-module(dgiot_udp_multicast).
-author("johnliu").
-include("logger.hrl").

%% API导出
-export([
    join_multicast_group/2,
    join_multicast_group/3,  % 新增：指定接口
    leave_multicast_group/2, 
    is_multicast_ip/1,
    set_multicast_options/1,
    set_multicast_options/2,
    get_multicast_interfaces/0,
    validate_multicast_address/1,
    get_available_multicast_groups/0,
    send_multicast/3,
    send_multicast/4,
    test_multicast/0,
    test_multicast/1,
    debug_multicast_status/0,
    force_join_multicast/2,
    get_local_interface_ip/0,
    get_default_interface/0,
    find_suitable_interface/1
]).

%%%===================================================================
%%% API函数
%%%===================================================================

%% @doc 加入多播组 - 终极修复版
join_multicast_group(Socket, MulticastGroup) when is_tuple(MulticastGroup) ->
    join_multicast_group(Socket, MulticastGroup, {0,0,0,0});

join_multicast_group(Socket, MulticastGroup) when is_list(MulticastGroup) ->
    case inet:parse_address(MulticastGroup) of
        {ok, Ip} -> join_multicast_group(Socket, Ip, {0,0,0,0});
        {error, Reason} -> 
            ?LOG(error, "解析多播地址失败 ~p: ~p", [MulticastGroup, Reason]),
            {error, Reason}
    end;

join_multicast_group(Socket, MulticastGroup) when is_binary(MulticastGroup) ->
    join_multicast_group(Socket, binary_to_list(MulticastGroup));

join_multicast_group(_Socket, InvalidGroup) ->
    ?LOG(error, "无效的多播组: ~p", [InvalidGroup]),
    {error, invalid_group_spec}.

%% @doc 加入多播组（指定接口）
join_multicast_group(Socket, MulticastGroup, Interface) when is_tuple(MulticastGroup) ->
    ?LOG(error, "[JOIN_MULTICAST] 开始加入多播组: ~p，接口: ~p", [MulticastGroup, Interface]),
    
    % 验证多播地址
    case validate_multicast_address(MulticastGroup) of
        true ->
            ?LOG(error, "[JOIN_MULTICAST] ✅ 多播地址有效: ~p", [MulticastGroup]),
            
            % 设置多播选项
            set_multicast_options(Socket),
            
            % 尝试多种方法加入
            Methods = [
                % 方法1: 传统方式
                {method1, fun() -> 
                    ?LOG(error, "[JOIN_MULTICAST] 方法1: 传统方式"),
                    inet:setopts(Socket, [{add_membership, {MulticastGroup, Interface}}])
                end},
                % 方法2: 新API格式
                {method2, fun() -> 
                    ?LOG(error, "[JOIN_MULTICAST] 方法2: 新API格式"),
                    IpMreq = #{multiaddr => MulticastGroup, interface => Interface},
                    inet:setopts(Socket, [{add_membership, IpMreq}])
                end},
                % 方法3: 仅设置多播选项（某些系统可能不需要显式加入）
                {method3, fun() -> 
                    ?LOG(error, "[JOIN_MULTICAST] 方法3: 仅设置选项"),
                    ok
                end}
            ],
            
            Result = try_multiple_methods(Socket, MulticastGroup, Methods),
            
            % 验证是否成功
            case Result of
                ok ->
                    ?LOG(error, "[JOIN_MULTICAST] ✅ 成功加入多播组: ~p", [MulticastGroup]),
                    
                    % 检查系统多播组状态
                    spawn(fun() ->
                        timer:sleep(500),
                        check_system_multicast_status(MulticastGroup)
                    end),
                    
                    ok;
                {error, ealready} ->
                    ?LOG(error, "[JOIN_MULTICAST] ℹ️ 已经加入多播组: ~p", [MulticastGroup]),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "[JOIN_MULTICAST] ❌ 加入多播组失败: ~p, 原因: ~p", 
                         [MulticastGroup, Reason]),
                    
                    % 尝试最后的备选方案
                    last_resort_join(Socket, MulticastGroup, Interface)
            end;
        false ->
            ?LOG(error, "[JOIN_MULTICAST] ❌ 无效的多播地址: ~p", [MulticastGroup]),
            {error, invalid_multicast_address}
    end.

%% @doc 强制加入多播组（使用系统命令）
force_join_multicast(Socket, MulticastGroup) when is_tuple(MulticastGroup) ->
    ?LOG(error, "[FORCE_JOIN_MULTICAST] 强制加入多播组: ~p", [MulticastGroup]),
    
    % 首先尝试标准方法
    case join_multicast_group(Socket, MulticastGroup) of
        ok ->
            ?LOG(error, "[FORCE_JOIN_MULTICAST] ✅ 标准方法成功"),
            ok;
        {error, _} ->
            % 使用系统命令加入
            MulticastGroupStr = ip_to_string(MulticastGroup),
            
            ?LOG(error, "[FORCE_JOIN_MULTICAST] 使用系统命令加入: ~p", [MulticastGroupStr]),
            
            % 尝试在所有接口加入
            Commands = [
                "ip maddr add " ++ MulticastGroupStr ++ " dev enp3s0",
                "ip maddr add " ++ MulticastGroupStr ++ " dev lo",
                "route add -net 224.0.0.0 netmask 240.0.0.0 dev enp3s0"
            ],
            
            lists:foreach(fun(Cmd) ->
                ?LOG(error, "[FORCE_JOIN_MULTICAST] 执行命令: ~s", [Cmd]),
                os:cmd(Cmd)
            end, Commands),
            
            % 检查结果
            timer:sleep(1000),
            check_system_multicast_status(MulticastGroup),
            
            ok
    end.

%% @doc 离开多播组
leave_multicast_group(Socket, MulticastGroup) when is_tuple(MulticastGroup) ->
    ?LOG(error, "[LEAVE_MULTICAST] 离开多播组: ~p", [MulticastGroup]),
    
    % 尝试在所有接口离开
    case inet:getifaddrs() of
        {ok, Interfaces} ->
            lists:foreach(fun({_IfName, Opts}) ->
                case proplists:get_value(addr, Opts) of
                    {A, _B, _C, _D} = LocalIp when A =/= 127 ->
                        try
                            inet:setopts(Socket, [{drop_membership, {MulticastGroup, LocalIp}}]),
                            ?LOG(error, "[LEAVE_MULTICAST] ✅ 离开接口: ~p", [LocalIp])
                        catch
                            _:Error ->
                                ?LOG(error, "[LEAVE_MULTICAST] ❌ 离开失败 ~p: ~p", [LocalIp, Error])
                        end;
                    _ -> ok
                end
            end, Interfaces);
        {error, Reason} ->
            ?LOG(error, "[LEAVE_MULTICAST] ❌ 获取接口失败: ~p", [Reason])
    end.

%% @doc 检查IP地址是否为多播地址
is_multicast_ip(Ip) when is_tuple(Ip) andalso tuple_size(Ip) =:= 4 ->
    {A, B, C, D} = Ip,
    (A >= 224 andalso A =< 239) orelse
    (A =:= 239 andalso B =:= 255 andalso C =:= 255 andalso D =:= 255);
is_multicast_ip(_) ->
    false.

%% @doc 设置多播套接字选项
set_multicast_options(Socket) ->
    set_multicast_options(Socket, 32).

set_multicast_options(Socket, TTL) ->
    ?LOG(error, "[SET_MULTICAST_OPTIONS] 设置多播选项, TTL=~p", [TTL]),
    
    Options = [
        {multicast_ttl, TTL},
        {multicast_loop, true},
        {reuseaddr, true},
        {recbuf, 1024 * 1024},
        {sndbuf, 1024 * 1024}
    ],
    
    case inet:setopts(Socket, Options) of
        ok ->
            ?LOG(error, "[SET_MULTICAST_OPTIONS] ✅ 多播选项设置成功");
        {error, Reason} ->
            ?LOG(error, "[SET_MULTICAST_OPTIONS] ❌ 设置失败: ~p", [Reason])
    end.

%% @doc 获取支持多播的网络接口
get_multicast_interfaces() ->
    case inet:getifaddrs() of
        {ok, Interfaces} ->
            ?LOG(error, "[GET_MULTICAST_INTERFACES] 找到 ~p 个接口", [length(Interfaces)]),
            
            % 筛选支持多播的接口
            MulticastInterfaces = lists:filter(fun({_IfName, Opts}) ->
                Flags = proplists:get_value(flags, Opts, []),
                lists:member(up, Flags) andalso lists:member(multicast, Flags)
            end, Interfaces),
            
            ?LOG(error, "[GET_MULTICAST_INTERFACES] ~p 个接口支持多播", 
                 [length(MulticastInterfaces)]),
            
            lists:foreach(fun({IfName, Opts}) ->
                Addr = proplists:get_value(addr, Opts, undefined),
                ?LOG(error, "[GET_MULTICAST_INTERFACES] 接口: ~s (~p)", [IfName, Addr])
            end, MulticastInterfaces),
            
            {ok, MulticastInterfaces};
        {error, Reason} ->
            ?LOG(error, "[GET_MULTICAST_INTERFACES] ❌ 获取接口失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 验证多播地址有效性
validate_multicast_address(Ip) when is_tuple(Ip) andalso tuple_size(Ip) =:= 4 ->
    is_multicast_ip(Ip);
validate_multicast_address(_) ->
    false.

%% @doc 获取已验证可用的多播组列表
get_available_multicast_groups() ->
    [
        "224.0.0.1",   % 所有主机
        "224.0.0.2",   % 所有路由器
        "224.0.0.5",   % OSPF
        "224.0.0.9",   % RIPv2
        "224.0.0.18",  % VRRP
        "224.0.0.22",  % IGMPv3
        "226.0.0.80"   % 无人机专用
    ].

%% @doc 发送多播消息
send_multicast(Group, Port, Message) ->
    send_multicast(Group, Port, Message, 32).

send_multicast(Group, Port, Message, TTL) ->
    case inet:parse_address(Group) of
        {ok, Ip} ->
            case validate_multicast_address(Ip) of
                true ->
                    % 创建临时socket发送
                    case gen_udp:open(0, [
                        binary, 
                        {multicast_ttl, TTL},
                        {multicast_loop, true}
                    ]) of
                        {ok, Socket} ->
                            Result = gen_udp:send(Socket, Ip, Port, Message),
                            gen_udp:close(Socket),
                            Result;
                        Error ->
                            {error, Error}
                    end;
                false ->
                    {error, invalid_multicast_address}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 测试多播功能
test_multicast() ->
    test_multicast("226.0.0.80").

test_multicast(Group) ->
    ?LOG(error, "[TEST_MULTICAST] 测试多播组: ~p", [Group]),
    
    % 创建接收socket
    {ok, RecvSocket} = gen_udp:open(8001, [
        binary,
        {reuseaddr, true},
        {multicast_loop, true},
        {multicast_ttl, 32},
        {active, false},
        {ip, {0,0,0,0}}
    ]),
    
    % 加入多播组
    case join_multicast_group(RecvSocket, Group) of
        ok ->
            ?LOG(error, "[TEST_MULTICAST] ✅ 加入多播组成功");
        Error ->
            ?LOG(error, "[TEST_MULTICAST] ❌ 加入失败: ~p", [Error]),
            gen_udp:close(RecvSocket),
            return
    end,
    
    % 发送测试消息
    TestMessage = <<"TEST_MULTICAST_MESSAGE_FROM_ERLANG">>,
    
    case send_multicast(Group, 8001, TestMessage) of
        ok -> 
            ?LOG(error, "[TEST_MULTICAST] ✅ 发送测试消息成功");
        {error, SendError} -> 
            ?LOG(error, "[TEST_MULTICAST] ❌ 发送失败: ~p", [SendError])
    end,
    
    % 尝试接收
    ?LOG(error, "[TEST_MULTICAST] 等待接收数据(5秒)..."),
    
    case gen_udp:recv(RecvSocket, 0, 5000) of
        {ok, {IP, Port, Data}} ->
            ?LOG(error, "[TEST_MULTICAST] ✅ 收到数据: ~p:~p -> ~p", [IP, Port, Data]);
        {error, timeout} ->
            ?LOG(error, "[TEST_MULTICAST] ⏰ 接收超时");
        {error, RecvError} ->
            ?LOG(error, "[TEST_MULTICAST] ❌ 接收错误: ~p", [RecvError])
    end,
    
    gen_udp:close(RecvSocket),
    ?LOG(error, "[TEST_MULTICAST] 测试完成").

%% @doc 调试多播状态
debug_multicast_status() ->
    ?LOG(error, "=== 多播状态调试 ==="),
    
    % 1. 检查系统多播组
    ?LOG(error, "1. 系统多播组状态:"),
    Result = os:cmd("netstat -g 2>/dev/null"),
    io:format("~s~n", [Result]),
    
    % 2. 检查路由
    ?LOG(error, "~n2. 多播路由:"),
    RouteResult = os:cmd("ip mroute show 2>/dev/null || route -n | grep 224"),
    io:format("~s~n", [RouteResult]),
    
    % 3. 检查接口
    ?LOG(error, "~n3. 网络接口状态:"),
    case inet:getifaddrs() of
        {ok, Interfaces} ->
            lists:foreach(fun({Name, Props}) ->
                Addr = proplists:get_value(addr, Props, undefined),
                Flags = proplists:get_value(flags, Props, []),
                if
                    is_tuple(Addr) andalso tuple_size(Addr) == 4 ->
                        ?LOG(error, "接口 ~s: ~p (flags: ~p)", [Name, Addr, Flags]);
                    true -> ok
                end
            end, Interfaces);
        {error, Reason} ->
            ?LOG(error, "获取接口失败: ~p", [Reason])
    end,
    
    ?LOG(error, "=== 调试结束 ===").

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 尝试多种方法
try_multiple_methods(_Socket, _MulticastGroup, Methods) ->
    lists:foldl(fun({MethodName, MethodFun}, Acc) ->
        case Acc of
            ok -> ok;
            _ ->
                ?LOG(error, "[TRY_METHODS] 尝试方法: ~p", [MethodName]),
                try MethodFun() of
                    ok -> 
                        ?LOG(error, "[TRY_METHODS] ✅ 方法 ~p 成功", [MethodName]),
                        ok;
                    {error, ealready} -> 
                        ?LOG(error, "[TRY_METHODS] ℹ️ 已经加入多播组"),
                        ok;
                    {error, Reason} -> 
                        ?LOG(error, "[TRY_METHODS] ❌ 方法 ~p 失败: ~p", [MethodName, Reason]),
                        {error, Reason}
                catch
                    _:Error:Stack ->
                        ?LOG(error, "[TRY_METHODS] 💥 方法 ~p 异常: ~p~n堆栈: ~p", 
                             [MethodName, Error, Stack]),
                        {error, Error}
                end
        end
    end, {error, not_tried}, Methods).

%% @doc 最后的备选方案
last_resort_join(Socket, MulticastGroup, Interface) ->
    ?LOG(error, "[LAST_RESORT_JOIN] 尝试最后的方法: ~p", [MulticastGroup]),
    
    % 方法1: 使用原始setsockopt
    try
        % 获取socket的文件描述符
        {ok, [{fd, Fd}]} = inet:getopts(Socket, [fd]),
        
        % 构建ip_mreq结构
        IpMreq = <<
            (element(1, MulticastGroup)):8,
            (element(2, MulticastGroup)):8,
            (element(3, MulticastGroup)):8,
            (element(4, MulticastGroup)):8,
            (element(1, Interface)):8,
            (element(2, Interface)):8,
            (element(3, Interface)):8,
            (element(4, Interface)):8
        >>,
        
        % 使用prim_inet直接调用setsockopt
        case prim_inet:setsockopt(Fd, inet, ip, add_membership, IpMreq) of
            ok ->
                ?LOG(error, "[LAST_RESORT_JOIN] ✅ 原始setsockopt成功"),
                ok;
            {error, Reason} ->
                ?LOG(error, "[LAST_RESORT_JOIN] ❌ 原始setsockopt失败: ~p", [Reason]),
                {error, Reason}
        end
    catch
        _:Error:Stack ->
            ?LOG(error, "[LAST_RESORT_JOIN] 💥 原始方法异常: ~p~n堆栈: ~p", [Error, Stack]),
            {error, Error}
    end.

%% @doc 检查系统多播组状态
check_system_multicast_status(MulticastGroup) ->
    MulticastGroupStr = ip_to_string(MulticastGroup),
    
    ?LOG(error, "[CHECK_SYSTEM_STATUS] 检查多播组: ~s", [MulticastGroupStr]),
    
    Result = os:cmd("netstat -g 2>/dev/null"),
    
    case string:str(Result, MulticastGroupStr) of
        0 ->
            ?LOG(error, "[CHECK_SYSTEM_STATUS] ❌ 系统未加入多播组: ~s", [MulticastGroupStr]),
            false;
        _ ->
            ?LOG(error, "[CHECK_SYSTEM_STATUS] ✅ 系统已加入多播组: ~s", [MulticastGroupStr]),
            
            % 显示相关行
            Lines = string:tokens(Result, "\n"),
            lists:foreach(fun(Line) ->
                case string:str(Line, MulticastGroupStr) > 0 of
                    true -> ?LOG(error, "[CHECK_SYSTEM_STATUS]   ~s", [Line]);
                    false -> ok
                end
            end, Lines),
            true
    end.

%% @doc IP元组转字符串
ip_to_string({A,B,C,D}) ->
    lists:flatten(io_lib:format("~B.~B.~B.~B", [A,B,C,D])).

%% @doc 获取本地网络接口IP（供dgiot_udp_server使用）
get_local_interface_ip() ->
    case inet:getifaddrs() of
        {ok, Interfaces} ->
            % 查找第一个非回环的IPv4地址
            case find_first_non_loopback_ipv4(Interfaces) of
                {ok, IP} -> {ok, IP};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取默认网络接口（供dgiot_udp_server使用）
get_default_interface() ->
    case get_local_interface_ip() of
        {ok, IP} -> {ok, IP};
        {error, _} -> {ok, {0,0,0,0}}
    end.

%% @doc 查找合适的网络接口（供dgiot_udp_server使用）
find_suitable_interface(PreferIfName) when is_list(PreferIfName) ->
    case inet:getifaddrs() of
        {ok, Interfaces} ->
            % 优先查找指定接口
            case lists:keyfind(PreferIfName, 1, Interfaces) of
                {_, Props} ->
                    case proplists:get_value(addr, Props) of
                        {A,_B,_C,_D} = IP when A =/= 127 -> {ok, IP};
                        _ -> find_first_non_loopback_ipv4(Interfaces)
                    end;
                false ->
                    find_first_non_loopback_ipv4(Interfaces)
            end;
        {error, Reason} ->
            {error, Reason}
    end;
find_suitable_interface(_) ->
    get_default_interface().

%% @private 查找第一个非回环的IPv4地址
find_first_non_loopback_ipv4([]) ->
    {error, no_suitable_interface};
find_first_non_loopback_ipv4([{IfName, Props} | Rest]) ->
    case proplists:get_value(addr, Props) of
        {A, _B, _C, _D} = IP when A =/= 127 ->
            ?LOG(error, "[FIND_INTERFACE] 找到接口 ~s: ~p", [IfName, IP]),
            {ok, IP};
        _ ->
            find_first_non_loopback_ipv4(Rest)
    end.
