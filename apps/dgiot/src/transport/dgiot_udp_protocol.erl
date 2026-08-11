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

%% @doc UDP协议层模块
%% 统一处理单播、广播、多播协议
-module(dgiot_udp_protocol).
-author("johnliu").
-include("logger.hrl").

%% API导出
-export([
    create_socket/1,
    send/4,
    join_multicast_group/2,
    leave_multicast_group/2,
    get_broadcast_addrs/0,
    is_multicast_ip/1
]).

%%%===================================================================
%%% API函数
%%%===================================================================

%% @doc 根据模式创建合适的套接字
create_socket(Options) ->
    % 检查是否是多播模式（通过mode参数或multicast选项）
    IsMulticast = proplists:get_value(mode, Options, unicast) =:= multicast orelse
                  proplists:get_value(multicast, Options, false),
    % 检查是否是广播模式
    IsBroadcast = proplists:get_value(mode, Options, unicast) =:= broadcast orelse
                  proplists:get_value(broadcast, Options, false),
    Port = proplists:get_value(port, Options, 0),
    
    SocketOptions = case {IsMulticast, IsBroadcast} of
        {true, _} -> [binary, {active, once}, {reuseaddr, true}, {multicast_ttl, 4}];
        {_, true} -> [binary, {active, once}, {reuseaddr, true}, {broadcast, true}];
        _ -> [binary, {active, once}, {reuseaddr, true}]
    end,
    
    io:format("~s ~p Creating socket with options: ~p~n", [?FILE, ?LINE, SocketOptions]),
    
    case dgiot_udp_transport:open(Port, SocketOptions) of
        {ok, Socket} ->
            % 如果是多播模式，加入多播组
            case IsMulticast of
                true ->
                    MulticastGroups = proplists:get_value(multicast_groups, Options, []),
                    lists:foreach(fun(Group) ->
                        join_multicast_group(Socket, Group)
                    end, MulticastGroups);
                false ->
                    ok
            end,
            {ok, Socket};
        Error ->
            Error
    end.

%% @doc 发送数据（根据模式自动选择发送方式）
send(Socket, Mode, Target, Data) ->
    case Mode of
        unicast ->
            {Addr, Port} = Target,
            dgiot_udp_transport:send(Socket, Addr, Port, Data);
        broadcast ->
            {_Addr, Port} = Target,
            Addrs = get_broadcast_addrs(),
            lists:foreach(fun(Addr) ->
                dgiot_udp_transport:send(Socket, Addr, Port, Data)
            end, Addrs);
        multicast ->
            {Addr, Port} = Target,
            dgiot_udp_transport:send(Socket, Addr, Port, Data)
    end.

%% @doc 加入多播组
join_multicast_group(Socket, MulticastGroup) ->
    dgiot_udp_multicast:join_multicast_group(Socket, MulticastGroup).

%% @doc 离开多播组
leave_multicast_group(Socket, MulticastGroup) ->
    dgiot_udp_multicast:leave_multicast_group(Socket, MulticastGroup).

%% @doc 获取广播地址列表
get_broadcast_addrs() ->
    dgiot_udp_broadcast:get_broadcast_addrs().

%% @doc 检查是否为多播IP地址
is_multicast_ip(Ip) ->
    dgiot_udp_multicast:is_multicast_ip(Ip).
