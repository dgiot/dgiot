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

%% @doc UDP多播演示模块
%% 展示四层架构在多播场景下的协同工作
-module(test_multicast_demo).
-author("johnliu").
<<<<<<< HEAD
-include("logger.hrl").
=======
-include("../../include/logger.hrl").
>>>>>>> origin/dgaiot-plugins

%% API导出
-export([run_demo/0, stop_demo/0]).

%% 回调模块
-export([init/1, handle_info/2]).

-record(server_state, {
    received_count = 0,
    clients = []
}).

%%%===================================================================
%%% API函数
%%%===================================================================

%% @doc 运行多播演示
run_demo() ->
    io:format("~n=== UDP多播四层架构演示 ===~n", []),
    
    % 多播配置
    MulticastGroups = ["239.255.255.250"],
    Port = 19000,
    
    % 启动多播服务器
    ServerOpts = [
        {port, Port},
        {multicast, true},
        {multicast_groups, MulticastGroups},
        {mod, ?MODULE},
        {state, #server_state{}}
    ],
    
    case dgiot_udp_server:start_link(ServerOpts) of
        {ok, ServerPid} ->
            io:format("✓ 多播服务器启动成功: ~p~n", [ServerPid]),
            
            % 启动多个多播客户端
            ClientOpts = [
                {port, 0},
                {multicast, true},
                {multicast_groups, MulticastGroups}
            ],
            
            Clients = lists:map(fun(N) ->
                case dgiot_udp_client:start_link(ClientOpts) of
                    {ok, Pid} ->
                        io:format("✓ 客户端 ~p 启动成功: ~p~n", [N, Pid]),
                        {N, Pid};
                    Error ->
                        io:format("✗ 客户端 ~p 启动失败: ~p~n", [N, Error]),
                        {N, error}
                end
            end, lists:seq(1, 3)),
            
            % 发送测试消息
            timer:sleep(1000),
            io:format("~n--- 发送多播测试消息 ---~n", []),
            
            lists:foreach(fun({N, Pid}) ->
                case Pid of
                    error -> ok;
                    _ ->
                        Message = list_to_binary("Hello from client " ++ integer_to_list(N)),
                        case dgiot_udp_client:send_multicast(Pid, "239.255.255.250", Port, Message) of
                            ok ->
                                io:format("✓ 客户端 ~p 发送消息成功: ~s~n", [N, Message]);
                            Error ->
                                io:format("✗ 客户端 ~p 发送消息失败: ~p~n", [N, Error])
                        end
                end
            end, Clients),
            
            % 等待接收消息
            timer:sleep(2000),
            
            % 获取服务器状态
            ServerStatus = dgiot_udp_server:get_status(ServerPid),
            io:format("~n--- 服务器状态 ---~n", []),
            io:format("~p~n", [ServerStatus]),
            
            {ok, #{server => ServerPid, clients => Clients}};
            
        Error ->
            io:format("✗ 多播服务器启动失败: ~p~n", [Error]),
            Error
    end.

%% @doc 停止演示
stop_demo() ->
    io:format("~n=== 停止多播演示 ===~n", []),
    ok.

%%%===================================================================
%%% 服务器回调函数
%%%===================================================================

%% @doc 服务器初始化
init(State) ->
    io:format("~s ~p Event = Multicast server initialized with state: ~p~n", 
             [?FILE, ?LINE, State]),
    {ok, State}.

%% @doc 处理接收到的UDP数据
handle_info({udp, Addr, Port, Data}, #server_state{received_count = Count} = State) ->
    io:format("~s ~p Event = Server received multicast data from ~p:~p: ~s (total: ~p)~n", 
             [?FILE, ?LINE, Addr, Port, Data, Count + 1]),
    
    NewState = State#server_state{
        received_count = Count + 1
    },
    {ok, NewState};

handle_info(_Info, State) ->
    {ok, State}.
