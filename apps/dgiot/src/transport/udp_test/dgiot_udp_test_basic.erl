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

%% @doc UDP基础测试模块
%% 提供UDP单播通信的基础测试功能
-module(dgiot_udp_test_basic).
-author("johnliu").
-include("../../../include/dgiot_socket.hrl").
-include("../../../include/logger.hrl").

%% API导出
-export([
    test_full_communication/0,
    stop_all/0
]).

%%%===================================================================
%%% 基础测试函数
%%%===================================================================

%% @doc 测试完整的UDP通信流程
test_full_communication() ->
    io:format("~s ~p Event = Starting full UDP communication test.~n", [?FILE, ?LINE]),
    
    % 使用固定端口进行测试
    Port = 18888,
    
    % 启动UDP服务器
    case dgiot_udp_server:start_link(test_business_module, [{port, Port}], #{}) of
        {ok, ServerPid} ->
            io:format("~s ~p Event = UDP server started on port ~p with pid ~p.~n", 
                     [?FILE, ?LINE, Port, ServerPid]),
            
            % 启动UDP客户端
            case dgiot_udp_client:start_link([{port, 0}]) of
                {ok, ClientPid} ->
                    io:format("~s ~p Event = UDP client started with pid ~p.~n", 
                             [?FILE, ?LINE, ClientPid]),
                    
                    % 发送测试消息
                    TestMessage = <<"Hello UDP Server">>,
                    io:format("~s ~p Event = Sending test message: ~p.~n", 
                             [?FILE, ?LINE, TestMessage]),
                    
                    case dgiot_udp_client:send(ClientPid, "127.0.0.1", Port, TestMessage) of
                        ok ->
                            io:format("~s ~p Event = ✓ Test message sent successfully.~n", 
                                     [?FILE, ?LINE]),
                            
                            % 等待消息处理
                            timer:sleep(1000),
                            
                            % 清理资源
                            dgiot_udp_client:close(ClientPid),
                            dgiot_udp_server:stop(ServerPid),
                            
                            {ok, #{
                                server_pid => ServerPid,
                                client_pid => ClientPid,
                                port => Port,
                                message_sent => TestMessage
                            }};
                        Error ->
                            io:format("~s ~p Event = ✗ Test message send failed: ~p.~n", 
                                     [?FILE, ?LINE, Error]),
                            
                            % 清理资源
                            dgiot_udp_client:close(ClientPid),
                            dgiot_udp_server:stop(ServerPid),
                            
                            Error
                    end;
                Error ->
                    io:format("~s ~p Event = ✗ UDP client start failed: ~p.~n", 
                             [?FILE, ?LINE, Error]),
                    dgiot_udp_server:stop(ServerPid),
                    Error
            end;
        Error ->
            io:format("~s ~p Event = ✗ UDP server start failed: ~p.~n", 
                     [?FILE, ?LINE, Error]),
            Error
    end.

%% @doc 停止所有基础测试进程
stop_all() ->
    io:format("~s ~p Event = Stopping all basic test processes.~n", [?FILE, ?LINE]),
    % 这里可以添加停止基础测试进程的逻辑
    ok.
