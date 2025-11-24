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

%% @doc UDP广播测试模块
%% 专门处理UDP广播通信测试
-module(dgiot_udp_test_broadcast).
-author("johnliu").
-include("../../../include/dgiot_socket.hrl").
-include("../../../include/logger.hrl").

%% API导出
-export([
    run_tests/0,
    stop_all/0,
    get_status/0
]).

%%%===================================================================
%%% 广播测试函数
%%%===================================================================

%% @doc 运行广播测试
run_tests() ->
    io:format("~s ~p Event = Starting broadcast tests.~n", [?FILE, ?LINE]),
    
    % 广播地址配置
    BroadcastAddress = "255.255.255.255",
    Port = 18889,
    
    io:format("~s ~p Event = Using broadcast address ~p on port ~p.~n", 
              [?FILE, ?LINE, BroadcastAddress, Port]),
    
    % 启动广播服务器
    case dgiot_udp_server:start_link(test_broadcast_module, [{port, Port}], #{}) of
        {ok, ServerPid} ->
            io:format("~s ~p Event = Broadcast server started on port ~p with pid ~p.~n", 
                     [?FILE, ?LINE, Port, ServerPid]),
            
            % 启动广播客户端
            case dgiot_udp_client:start_link([{port, 0}]) of
                {ok, ClientPid} ->
                    io:format("~s ~p Event = Broadcast client started with pid ~p.~n", 
                             [?FILE, ?LINE, ClientPid]),
                    
                    % 发送广播测试消息
                    TestMessage = <<"BROADCAST_TEST_MESSAGE">>,
                    io:format("~s ~p Event = Sending broadcast message: ~p.~n", 
                             [?FILE, ?LINE, TestMessage]),
                    
                    case dgiot_udp_client:send(ClientPid, BroadcastAddress, Port, TestMessage) of
                        ok ->
                            io:format("~s ~p Event = ✓ Broadcast message sent successfully.~n", 
                                     [?FILE, ?LINE]),
                            
                            % 等待消息处理
                            timer:sleep(2000),
                            
                            % 清理资源
                            dgiot_udp_client:close(ClientPid),
                            dgiot_udp_server:stop(ServerPid),
                            
                            {ok, #{
                                server_pid => ServerPid,
                                client_pid => ClientPid,
                                broadcast_address => BroadcastAddress,
                                port => Port,
                                message_sent => TestMessage
                            }};
                        Error ->
                            io:format("~s ~p Event = ✗ Broadcast message send failed: ~p.~n", 
                                     [?FILE, ?LINE, Error]),
                            
                            % 清理资源
                            dgiot_udp_client:close(ClientPid),
                            dgiot_udp_server:stop(ServerPid),
                            
                            Error
                    end;
                Error ->
                    io:format("~s ~p Event = ✗ Broadcast client start failed: ~p.~n", 
                             [?FILE, ?LINE, Error]),
                    dgiot_udp_server:stop(ServerPid),
                    Error
            end;
        Error ->
            io:format("~s ~p Event = ✗ Broadcast server start failed: ~p.~n", 
                     [?FILE, ?LINE, Error]),
            Error
    end.

%% @doc 停止所有广播测试进程
stop_all() ->
    io:format("~s ~p Event = Stopping all broadcast test processes.~n", [?FILE, ?LINE]),
    % 这里可以添加停止广播测试进程的逻辑
    ok.

%% @doc 获取广播测试状态
get_status() ->
    #{
        broadcast_tests => "available",
        broadcast_address => "255.255.255.255",
        default_port => 18889,
        supported_protocols => ["UDP"]
    }.
