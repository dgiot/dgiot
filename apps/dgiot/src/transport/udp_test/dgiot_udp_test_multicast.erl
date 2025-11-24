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

%% @doc UDP多播测试模块
%% 专门处理UDP多播通信测试
-module(dgiot_udp_test_multicast).
-author("johnliu").
-include("../../../include/dgiot_socket.hrl").
-include("../../../include/logger.hrl").

%% API导出
-export([
    run_tests/0,
    stop_all/0,
    get_status/0,
    test_multicast_with_tcpdump/0
]).

%%%===================================================================
%%% 多播测试函数
%%%===================================================================

%% @doc 运行多播测试 - 简化版本
run_tests() ->
    io:format("=== UDP MULTICAST TESTS START ===~n"),
    
    % 运行基础多播测试
    io:format("1. Running basic multicast test...~n"),
    case dgiot_udp_test_utils:test_multicast() of
        {ok, _} ->
            io:format("   ✓ Basic multicast test completed~n");
        Error1 ->
            io:format("   ✗ Basic multicast test failed: ~p~n", [Error1])
    end,
    
    % 运行tcpdump多播测试
    io:format("2. Running multicast test with tcpdump...~n"),
    case dgiot_udp_test_utils:test_multicast_with_tcpdump() of
        {ok, _} ->
            io:format("   ✓ Tcpdump multicast test completed~n");
        Error2 ->
            io:format("   ✗ Tcpdump multicast test failed: ~p~n", [Error2])
    end,
    
    io:format("=== UDP MULTICAST TESTS COMPLETED ===~n"),
    ok.

%% @doc 停止所有多播测试进程
stop_all() ->
    io:format("~s ~p Event = Stopping all multicast test processes.~n", [?FILE, ?LINE]),
    dgiot_udp_test_utils:stop_all_processes(),
    ok.

%% @doc 获取多播测试状态
get_status() ->
    #{
        multicast_tests => "available",
        default_port => 19000,
        supported_protocols => ["UDP Multicast"],
        multicast_groups => dgiot_udp_test_utils:get_multicast_groups()
    }.

%% @doc 使用tcpdump进行多播测试
test_multicast_with_tcpdump() ->
    io:format("~s ~p Event = Starting multicast test with tcpdump.~n", [?FILE, ?LINE]),
    dgiot_udp_test_utils:test_multicast_with_tcpdump().
