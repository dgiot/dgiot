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

%% @doc 精简UDP服务器模块
%% 应用层入口，专注于服务器启动和配置
-module(dgiot_udp_server).
-author("johnliu").
-include("../../include/logger.hrl").

%% API导出
-export([start_link/1, start_link/3, child_spec/3, child_spec/4, get_status/1]).
-export([join_multicast_group/2, leave_multicast_group/2, send/2, send/3, send/4, send_multicast/4, get_available_multicast_groups/0]).
-export([start_multicast_server/2, start_multicast_server/1, get_multicast_status/1, broadcast_to_groups/3, stop/1]).

%%%===================================================================
%%% API函数
%%%===================================================================

%% @doc 启动UDP服务器
start_link(Args) ->
    dgiot_udp_session:start_link([{mode, server} | Args]).

%% @doc 启动UDP服务器（兼容旧接口）
start_link(Mod, Opts, State) ->
    Args = [
        {mode, server},
        {mod, Mod},
        {options, Opts},
        {state, State}
    ],
    dgiot_udp_session:start_link(Args).

%% @doc 创建子进程规格
child_spec(Mod, Port, State) ->
    child_spec(Mod, Port, State, []).

child_spec(Mod, Port, State, Opts) ->
    Name = Mod,
    ok = esockd:start(),
    io:format("~s ~p Creating UDP child spec for port ~p.~n", [?FILE, ?LINE, Port]),
    
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

%% @doc 获取服务器状态
get_status(ServerPid) ->
    dgiot_udp_session:get_status(ServerPid).

%% @doc 加入多播组
join_multicast_group(ServerPid, MulticastGroup) ->
    dgiot_udp_session:join_multicast_group(ServerPid, MulticastGroup).

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
    io:format("~s ~p Event = Starting multicast server on port ~p with groups: ~p~n", 
              [?FILE, ?LINE, Port, MulticastGroups]),
    
    Args = [
        {port, Port},
        {multicast_groups, MulticastGroups}
    ],
    
    case start_link(Args) of
        {ok, Pid} ->
            % 加入指定的多播组
            lists:foreach(fun(Group) ->
                case join_multicast_group(Pid, Group) of
                    ok ->
                        io:format("~s ~p Event = SUCCESS: Joined multicast group ~p~n", 
                                 [?FILE, ?LINE, Group]);
                    Error ->
                        io:format("~s ~p Event = WARNING: Failed to join multicast group ~p: ~p~n", 
                                 [?FILE, ?LINE, Group, Error])
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
