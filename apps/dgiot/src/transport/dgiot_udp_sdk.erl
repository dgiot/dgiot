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

%% @doc UDP SDK模块
%% 封装成熟的UDP库功能，提供统一接口
-module(dgiot_udp_sdk).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include_lib("kernel/include/inet.hrl").

%% API导出
-export([
    start_server/1,          % 启动UDP服务器
    stop_server/1,           % 停止UDP服务器
    send/2,                  % 发送数据
    test_available/0,        % 测试SDK是否可用
    join_multicast_group/2,   % 加入多播组
    leave_multicast_group/2  % 离开多播组
]).

%% @doc 启动UDP服务器
%% Port: 监听端口
start_server(Port) ->
    try
        case gen_udp:open(Port, [binary, {active, false}, {reuseaddr, true}, inet]) of
            {ok, Socket} -> 
                ?LOG(info, "UDP server started on port ~p", [Port]),
                {ok, Socket};
            {error, eaddrinuse} ->
                ?LOG(warning, "Port ~p already in use", [Port]),
                {error, port_in_use};
            {error, OpenReason} ->
                ?LOG(error, "Failed to start UDP server on port ~p: ~p", [Port, OpenReason]),
                {error, OpenReason}
        end
    catch
        Error:CatchReason ->
            ?LOG(error, "UDP server start exception: ~p:~p", [Error, CatchReason]),
            {error, CatchReason}
    end.

%% @doc 停止UDP服务器
%% ServerRef: 套接字引用
stop_server(Socket) when is_port(Socket) ->
    gen_udp:close(Socket),
    ?LOG(info, "UDP server stopped"),
    ok.

%% @doc 发送UDP数据
%% Socket: 套接字引用
%% Data: 要发送的二进制数据
send(Socket, Data) when is_port(Socket), is_binary(Data) ->
    case gen_udp:send(Socket, "localhost", 0, Data) of
        ok -> 
            ?LOG(debug, "UDP data sent, size: ~p", [byte_size(Data)]),
            ok;
        {error, Reason} ->
            ?LOG(error, "Failed to send UDP data: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 检查SDK是否可用
test_available() ->
    try
        case gen_udp:open(0, [{active, false}, {reuseaddr, true}]) of
            {ok, Socket} -> 
                gen_udp:close(Socket),
                true;
            _ -> 
                ?LOG(error, "UDP port test failed"),
                false
        end
    catch
        Error:Reason ->
            ?LOG(error, "UDP test exception: ~p:~p", [Error, Reason]),
            false
    end.

%% @doc 加入多播组
%% Socket: 套接字引用
%% Group: 多播组地址(字符串或元组格式)
join_multicast_group(Socket, Group) when is_port(Socket) ->
    case inet:parse_address(Group) of
        {ok, Ip} when tuple_size(Ip) == 4 ->
            case inet:setopts(Socket, [{add_membership, {Ip, {0,0,0,0}}}]) of
                ok -> 
                    ?LOG(info, "Joined multicast group ~p", [Ip]),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "Failed to join multicast group ~p: ~p", [Ip, Reason]),
                    {error, Reason}
            end;
        _ ->
            ?LOG(error, "Invalid multicast address: ~p", [Group]),
            {error, invalid_address}
    end.

%% @doc 离开多播组
%% Socket: 套接字引用
%% Group: 多播组地址(字符串或元组格式)
leave_multicast_group(Socket, Group) when is_port(Socket) ->
    case inet:parse_address(Group) of
        {ok, Ip} when tuple_size(Ip) == 4 ->
            case inet:setopts(Socket, [{drop_membership, {Ip, {0,0,0,0}}}]) of
                ok -> 
                    ?LOG(info, "Left multicast group ~p", [Ip]),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "Failed to leave multicast group ~p: ~p", [Ip, Reason]),
                    {error, Reason}
            end;
        _ ->
            ?LOG(error, "Invalid multicast address: ~p", [Group]),
            {error, invalid_address}
    end.