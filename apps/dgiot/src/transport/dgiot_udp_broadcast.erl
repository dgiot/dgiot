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

%% @doc 专门的广播功能模块
%% 提供UDP广播相关的所有功能，包括获取广播地址、广播地址验证等
-module(dgiot_udp_broadcast).
-author("johnliu").
-include("../../include/logger.hrl").

%% API导出
-export([
    get_broadcast_addrs/0,
    get_broadcast_addresses/2,
    get_broadcast_address/1,
    validate_broadcast_address/1,
    get_broadcast_interfaces/0,
    send/3,
    get_ipaddrs/0
]).

%%%===================================================================
%%% API函数
%%%===================================================================

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

%% @doc 验证广播地址有效性
validate_broadcast_address(Ip) when is_tuple(Ip) andalso tuple_size(Ip) =:= 4 ->
    {A, _, _, D} = Ip,
    % 广播地址通常是网络地址的最后一位为255
    (D =:= 255) orelse 
    % 或者是一些特殊的广播地址，如255.255.255.255
    (A =:= 255 andalso D =:= 255);
validate_broadcast_address(_) ->
    false.

%% @doc 获取支持广播的网络接口
get_broadcast_interfaces() ->
    case inet:getifaddrs() of
        {ok, Interfaces} ->
            io:format("~s ~p Event = Found ~p network interfaces.~n", [?FILE, ?LINE, length(Interfaces)]),
            
            % 详细记录每个接口的状态
            lists:foreach(fun({IfName, Opts}) ->
                Addr = proplists:get_value(addr, Opts, undefined),
                BroadcastAddr = proplists:get_value(broadaddr, Opts, undefined),
                Flags = proplists:get_value(flags, Opts, []),
                IsUp = lists:member(up, Flags),
                IsBroadcast = lists:member(broadcast, Flags),
                io:format("~s ~p Event = Interface ~s: addr=~p, broadcast=~p, up=~p, broadcast_flag=~p~n", 
                     [?FILE, ?LINE, IfName, Addr, BroadcastAddr, IsUp, IsBroadcast])
            end, Interfaces),
            
            {ok, Interfaces};
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% 新增函数以满足其他模块的调用
%%%===================================================================

%% @doc 发送广播消息
send(ChannelId, ClientId, Message) ->
    io:format("~s ~p Event = Sending broadcast message via dgiot_udp_server: ChannelId=~p, ClientId=~p, Message=~p.~n", 
             [?FILE, ?LINE, ChannelId, ClientId, Message]),
    
    % 通过UDP服务器发送消息
    case dgiot_udp_server:send(ChannelId, ClientId, Message) of
        ok ->
            io:format("~s ~p Event = Broadcast message sent successfully.~n", [?FILE, ?LINE]),
            ok;
        Error ->
            io:format("~s ~p Event = Failed to send broadcast message: ~p.~n", [?FILE, ?LINE, Error]),
            Error
    end.

%% @doc 获取所有IP地址
get_ipaddrs() ->
    io:format("~s ~p Event = Getting all IP addresses.~n", [?FILE, ?LINE]),
    
    case inet:getifaddrs() of
        {ok, Interfaces} ->
            % 提取所有IPv4地址
            Addrs = lists:foldl(fun({_IfName, Opts}, Acc) ->
                case proplists:get_value(addr, Opts) of
                    {A, _, _, _} = Addr when A /= 127 -> % 排除回环地址
                        [Addr | Acc];
                    _ ->
                        Acc
                end
            end, [], Interfaces),
            
            io:format("~s ~p Event = Found ~p IP addresses: ~p.~n", [?FILE, ?LINE, length(Addrs), Addrs]),
            Addrs;
        {error, Reason} ->
            io:format("~s ~p Event = Failed to get IP addresses: ~p.~n", [?FILE, ?LINE, Reason]),
            []
    end.
