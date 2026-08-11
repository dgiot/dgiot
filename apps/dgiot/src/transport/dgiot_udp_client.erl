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

%% @doc 精简UDP客户端模块
%% 应用层入口，专注于客户端启动和消息发送
-module(dgiot_udp_client).
-author("johnliu").
-include("../../include/logger.hrl").

%% API导出
-export([
    start_link/1, 
    send/2, send/4,
    close/1,
    get_status/1,
    join_multicast_group/2,
    leave_multicast_group/2,
    send_multicast/4,
    get_available_multicast_groups/0,
    start_multicast_client/1, start_multicast_client/2,
    get_multicast_status/1,
    listen_multicast/2,
    test_multicast_send/3,
    multicast_test_loop/4
]).

%%%===================================================================
%%% API函数
%%%===================================================================

%% @doc 启动UDP客户端
start_link(Args) when is_map(Args) ->
    % 将map转换为列表形式
    ArgsList = maps:to_list(Args),
    dgiot_udp_session:start_link([{mode, client} | ArgsList]);
start_link(Args) when is_list(Args) ->
    dgiot_udp_session:start_link([{mode, client} | Args]).

%% @doc 发送数据（使用已连接的套接字）
send(ClientPid, Data) ->
    gen_server:call(ClientPid, {send, Data}).

%% @doc 发送数据到指定地址和端口
send(ClientPid, Addr, Port, Data) ->
    gen_server:call(ClientPid, {send, Addr, Port, Data}).


%% @doc 关闭客户端
close(ClientPid) ->
    gen_server:call(ClientPid, close).

%% @doc 获取客户端状态
get_status(ClientPid) ->
    gen_server:call(ClientPid, get_status).

%% @doc 加入多播组
join_multicast_group(ClientPid, MulticastGroup) ->
    gen_server:call(ClientPid, {join_multicast_group, MulticastGroup}).

%% @doc 离开多播组
leave_multicast_group(ClientPid, MulticastGroup) ->
    gen_server:call(ClientPid, {leave_multicast_group, MulticastGroup}).

%% @doc 发送多播消息
send_multicast(ClientPid, MulticastGroup, Port, Message) ->
    gen_server:call(ClientPid, {send_multicast, MulticastGroup, Port, Message}).

%% @doc 获取可用的多播组列表
get_available_multicast_groups() ->
    dgiot_udp_multicast:get_available_multicast_groups().

%% @doc 启动多播客户端
start_multicast_client(Port, MulticastGroups) when is_list(MulticastGroups) ->
    io:format("~s ~p Event = Starting multicast client on port ~p with groups: ~p~n", 
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
            io:format("~s ~p Event = ERROR: Failed to start multicast client: ~p~n", 
                     [?FILE, ?LINE, Error]),
            Error
    end.


%% @doc 启动多播客户端（简化版本）
start_multicast_client(Port) ->
    MulticastGroups = get_available_multicast_groups(),
    start_multicast_client(Port, MulticastGroups).

%% @doc 获取客户端多播状态
get_multicast_status(ClientPid) ->
    case get_status(ClientPid) of
        {ok, Status} ->
            MulticastGroups = proplists:get_value(multicast_groups, Status, []),
            {ok, [
                {client_pid, ClientPid},
                {multicast_groups_joined, MulticastGroups},
                {total_groups, length(MulticastGroups)},
                {status, running}
            ]};
        Error ->
            Error
    end.

%% @doc 监听多播消息
listen_multicast(_ClientPid, Timeout) ->
    io:format("~s ~p Event = Listening for multicast messages for ~p ms...~n", 
              [?FILE, ?LINE, Timeout]),
    
    receive
        {udp, _Socket, _Address, _Port, Data} ->
            io:format("~s ~p Event = RECEIVED multicast message: ~p~n", 
                     [?FILE, ?LINE, Data]),
            {ok, Data}
    after Timeout ->
        io:format("~s ~p Event = No multicast messages received within timeout.~n", 
                 [?FILE, ?LINE]),
        {error, timeout}
    end.

%% @doc 发送测试多播消息
test_multicast_send(ClientPid, Port, Message) ->
    io:format("~s ~p Event = Testing multicast send to port ~p: ~p~n", 
              [?FILE, ?LINE, Port, Message]),
    
    MulticastGroups = get_available_multicast_groups(),
    Results = lists:map(fun(Group) ->
        case send_multicast(ClientPid, Group, Port, Message) of
            ok -> {Group, success};
            Error -> {Group, Error}
        end
    end, MulticastGroups),
    {ok, Results}.

%% @doc 多播客户端测试循环
multicast_test_loop(ClientPid, Port, Count, Interval) ->
    if
        Count =< 0 ->
            ok;
        true ->
            Message = list_to_binary("Multicast test message #" ++ integer_to_list(Count)),
            case test_multicast_send(ClientPid, Port, Message) of
                {ok, Results} ->
                    io:format("~s ~p Event = Sent multicast message ~p/~p: ~p~n", 
                             [?FILE, ?LINE, Count, Count, Results]),
                    timer:sleep(Interval),
                    multicast_test_loop(ClientPid, Port, Count - 1, Interval);
                Error ->
                    io:format("~s ~p Event = Failed to send multicast message: ~p~n", 
                             [?FILE, ?LINE, Error]),
                    Error
            end
    end.
