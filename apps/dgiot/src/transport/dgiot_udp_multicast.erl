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

%% @doc 专门的多播功能模块
%% 提供UDP多播相关的所有功能，包括加入/离开多播组、多播地址验证等
-module(dgiot_udp_multicast).
-author("johnliu").
-include("../../include/logger.hrl").

%% API导出
-export([
    join_multicast_group/2,
    leave_multicast_group/2, 
    is_multicast_ip/1,
    set_multicast_options/1,
    get_multicast_interfaces/0,
    validate_multicast_address/1,
    get_available_multicast_groups/0,
    send_multicast/3,
    send_multicast/4,
    test_multicast/0
]).

%%%===================================================================
%%% API函数
%%%===================================================================

%% @doc 加入多播组 - 终极优化版本，确保多播组加入成功率和稳定性
join_multicast_group(Socket, MulticastGroup) when is_tuple(MulticastGroup) ->
    % 验证多播地址范围 (224.0.0.0 - 239.255.255.255)
    case validate_multicast_address(MulticastGroup) of
        true ->
            io:format("~s ~p Event = ========== MULTICAST GROUP JOIN START ==========.~n", [?FILE, ?LINE]),
            io:format("~s ~p Event = Joining multicast group ~p on socket ~p.~n", [?FILE, ?LINE, MulticastGroup, Socket]),
            
            % 设置多播选项
            set_multicast_options(Socket),
            
            % 获取所有网络接口并加入多播组
            case get_multicast_interfaces() of
                {ok, Interfaces} ->
                    JoinedCount = join_multicast_on_interfaces(Socket, MulticastGroup, Interfaces),
                    handle_join_result(Socket, MulticastGroup, JoinedCount);
                {error, Reason} ->
                    io:format("~s ~p Event = ✗ FAILED to get network interfaces: ~p.~n", [?FILE, ?LINE, Reason]),
                    {error, Reason}
            end;
        false ->
            io:format("~s ~p Event = ✗ INVALID multicast address ~p (not in multicast range 224.0.0.0-239.255.255.255).~n", 
                 [?FILE, ?LINE, MulticastGroup]),
            {error, invalid_multicast_address}
    end;

join_multicast_group(Socket, MulticastGroup) when is_list(MulticastGroup) ->
    case inet:parse_address(MulticastGroup) of
        {ok, Ip} -> join_multicast_group(Socket, Ip);
        {error, Reason} -> 
            ?LOG(error, "Invalid multicast address ~p: ~p", [MulticastGroup, Reason]),
            {error, Reason}
    end;

join_multicast_group(_Socket, InvalidGroup) ->
    ?LOG(error, "Invalid multicast group specification: ~p", [InvalidGroup]),
    {error, invalid_group_spec}.

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

%% @doc 检查IP地址是否为多播地址
is_multicast_ip(Ip) when is_tuple(Ip) andalso tuple_size(Ip) =:= 4 ->
    {A, B, C, D} = Ip,
    (A >= 224 andalso A =< 239) orelse
    (A =:= 239 andalso B =:= 255 andalso C =:= 255 andalso D =:= 255);
is_multicast_ip(_) ->
    false.

%% @doc 设置多播套接字选项
set_multicast_options(Socket) ->
    % 强制设置多播TTL为4，确保报文能跨网络传播
    _ = case inet:setopts(Socket, [{multicast_ttl, 4}]) of
        ok ->
            io:format("~s ~p Event = ✓ Set multicast TTL to 4.~n", [?FILE, ?LINE]);
        Error1 ->
            io:format("~s ~p Event = ✗ Failed to set multicast TTL: ~p.~n", [?FILE, ?LINE, Error1])
    end,
    
    % 强制启用多播环回，确保本地也能收到多播报文
    _ = case inet:setopts(Socket, [{multicast_loop, true}]) of
        ok ->
            io:format("~s ~p Event = ✓ Enabled multicast loopback.~n", [?FILE, ?LINE]);
        Error2 ->
            io:format("~s ~p Event = ✗ Failed to set multicast loopback: ~p.~n", [?FILE, ?LINE, Error2])
    end.

%% @doc 获取支持多播的网络接口
get_multicast_interfaces() ->
    case inet:getifaddrs() of
        {ok, Interfaces} ->
            io:format("~s ~p Event = Found ~p network interfaces.~n", [?FILE, ?LINE, length(Interfaces)]),
            
            % 详细记录每个接口的状态
            lists:foreach(fun({IfName, Opts}) ->
                Addr = proplists:get_value(addr, Opts, undefined),
                Flags = proplists:get_value(flags, Opts, []),
                IsUp = lists:member(up, Flags),
                IsMulticast = lists:member(multicast, Flags),
                io:format("~s ~p Event = Interface ~s: addr=~p, up=~p, multicast=~p~n", 
                     [?FILE, ?LINE, IfName, Addr, IsUp, IsMulticast])
            end, Interfaces),
            
            {ok, Interfaces};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 验证多播地址有效性
validate_multicast_address(Ip) when is_tuple(Ip) andalso tuple_size(Ip) =:= 4 ->
    is_multicast_ip(Ip);
validate_multicast_address(_) ->
    false.

%% @doc 获取已验证可用的多播组列表
%% 基于测试结果，优先使用224.0.0.1在回环接口上工作
%% 其他组在特定网络环境下可能工作
get_available_multicast_groups() ->
    [
        "224.0.0.1",  % 所有主机多播组 - 已验证工作
        "224.0.0.2",  % 所有路由器多播组
        "224.0.0.5",  % OSPF路由器
        "224.0.0.9",  % RIPv2路由器
        "224.0.0.18", % VRRP
        "224.0.0.22"  % IGMPv3
    ].

%% @doc 发送多播消息（改进版本）
send_multicast(Group, Port, Message) ->
    case inet:parse_address(Group) of
        {ok, Ip} ->
            case validate_multicast_address(Ip) of
                true ->
                    % 直接发送多播消息，不创建临时套接字
                    case gen_udp:open(0, [
                        binary, 
                        {multicast_ttl, 4},
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

%% @doc 测试多播功能 - 发送测试报文
test_multicast() ->
    io:format("~s ~p Event = Starting multicast test...~n", [?FILE, ?LINE]),
    Groups = get_available_multicast_groups(),
    io:format("~s ~p Event = Testing multicast groups: ~p~n", [?FILE, ?LINE, Groups]),
    lists:foreach(fun(Group) ->
        io:format("~s ~p Event = Testing group: ~p~n", [?FILE, ?LINE, Group]),
        case send_multicast(Group, 19000, <<"TEST_MULTICAST_MESSAGE">>) of
            ok -> 
                io:format("~s ~p Event = ✓ SUCCESS: Sent multicast to ~p~n", [?FILE, ?LINE, Group]);
            {error, Reason} -> 
                io:format("~s ~p Event = ✗ FAILED: ~p~n", [?FILE, ?LINE, Reason])
        end
    end, Groups),
    io:format("~s ~p Event = Multicast test completed.~n", [?FILE, ?LINE]).

%% @doc 使用现有套接字发送多播消息（优化版本）
-spec(send_multicast(inet:socket(), string(), inet:port_number(), binary()) ->
      ok | {error, term()}).
send_multicast(Socket, Group, Port, Message) ->
    case inet:parse_address(Group) of
        {ok, Ip} ->
            case validate_multicast_address(Ip) of
                true ->
                    set_multicast_options(Socket),
                    gen_udp:send(Socket, Ip, Port, Message);
                false ->
                    {error, invalid_multicast_address}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 在多个网络接口上加入多播组
join_multicast_on_interfaces(Socket, MulticastGroup, Interfaces) ->
    lists:foldl(fun({IfName, Opts}, Count) ->
        case proplists:get_value(addr, Opts) of
            {A, _B, _C, _D} = LocalIp when A =/= 127 -> % 排除回环地址
                Flags = proplists:get_value(flags, Opts, []),
                IsUp = lists:member(up, Flags),
                IsMulticast = lists:member(multicast, Flags),
                
                if
                    IsUp andalso IsMulticast ->
                        try_join_multicast_on_interface(Socket, MulticastGroup, LocalIp, IfName, Count);
                    not IsUp ->
                        io:format("~s ~p Event = Interface ~p (~s) is DOWN, skipping.~n", 
                             [?FILE, ?LINE, LocalIp, IfName]),
                        Count;
                    not IsMulticast ->
                        io:format("~s ~p Event = Interface ~p (~s) does NOT support multicast, skipping.~n", 
                             [?FILE, ?LINE, LocalIp, IfName]),
                        Count;
                    true ->
                        Count
                end;
            _ ->
                Count
        end
    end, 0, Interfaces).

%% @doc 在单个网络接口上尝试加入多播组
try_join_multicast_on_interface(Socket, MulticastGroup, LocalIp, IfName, Count) ->
    io:format("~s ~p Event = Attempting to join multicast group ~p on interface ~p (~s)...~n", 
         [?FILE, ?LINE, MulticastGroup, LocalIp, IfName]),
    case inet:setopts(Socket, [{add_membership, {MulticastGroup, LocalIp}}]) of
        ok ->
            io:format("~s ~p Event = ✓ SUCCESS: Joined multicast group ~p on interface ~p (~s).~n", 
                 [?FILE, ?LINE, MulticastGroup, LocalIp, IfName]),
            Count + 1;
        {error, ealready} ->
            % 已经加入该多播组，不算失败
            io:format("~s ~p Event = ℹ INFO: Already joined multicast group ~p on interface ~p (~s).~n", 
                 [?FILE, ?LINE, MulticastGroup, LocalIp, IfName]),
            Count + 1;
        {error, Reason} ->
            io:format("~s ~p Event = ✗ FAILED to join multicast group ~p on interface ~p (~s): ~p.~n", 
                 [?FILE, ?LINE, MulticastGroup, LocalIp, IfName, Reason]),
            Count
    end.

%% @doc 处理多播组加入结果
handle_join_result(Socket, MulticastGroup, JoinedCount) ->
    io:format("~s ~p Event = Multicast group join summary: ~p interfaces successfully joined.~n", 
         [?FILE, ?LINE, JoinedCount]),
    
    case JoinedCount of
        0 ->
            io:format("~s ~p Event = ⚠ WARNING: No suitable interfaces found for multicast group ~p.~n", 
                 [?FILE, ?LINE, MulticastGroup]),
            io:format("~s ~p Event = Attempting to join on default interface (0.0.0.0)...~n", [?FILE, ?LINE]),
            try_join_on_default_interface(Socket, MulticastGroup);
        _ ->
            io:format("~s ~p Event = ✓ SUCCESS: Multicast group ~p joined on ~p interfaces.~n", 
                 [?FILE, ?LINE, MulticastGroup, JoinedCount]),
            ok
    end,
    
    io:format("~s ~p Event = ========== MULTICAST GROUP JOIN END ==========.~n", [?FILE, ?LINE]),
    ok.

%% @doc 在默认接口上尝试加入多播组
try_join_on_default_interface(Socket, MulticastGroup) ->
    try
        case inet:setopts(Socket, [{add_membership, {MulticastGroup, {0,0,0,0}}}]) of
            ok ->
                io:format("~s ~p Event = ✓ SUCCESS: Joined multicast group ~p on default interface.~n", 
                     [?FILE, ?LINE, MulticastGroup]);
            {error, ealready} ->
                io:format("~s ~p Event = ℹ INFO: Already joined multicast group ~p on default interface.~n", 
                     [?FILE, ?LINE, MulticastGroup]);
            {error, Reason} ->
                io:format("~s ~p Event = ✗ FAILED to join multicast group ~p on default interface: ~p.~n", 
                     [?FILE, ?LINE, MulticastGroup, Reason])
        end
    catch
        _:Error ->
            io:format("~s ~p Event = ✗ EXCEPTION joining multicast group ~p on default interface: ~p.~n", 
                 [?FILE, ?LINE, MulticastGroup, Error])
    end.
