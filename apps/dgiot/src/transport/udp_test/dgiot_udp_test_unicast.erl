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

%% @doc UDP单播测试模块
%% 专门处理UDP单播通信测试
-module(dgiot_udp_test_unicast).
-author("johnliu").
-include("../../../include/dgiot_socket.hrl").

%% API导出
-export([
    run_tests/0,
    stop_all/0,
    get_status/0
]).

%%%===================================================================
%%% 单播测试函数
%%%===================================================================

%% @doc 运行单播测试
run_tests() ->
    io:format("~s ~p Event = Starting unicast tests.~n", [?FILE, ?LINE]),
    
    % 使用基本测试模块中的单播测试
    case dgiot_udp_test_basic:test_full_communication() of
        {ok, Result} ->
            io:format("~s ~p Event = Unicast tests completed successfully.~n", [?FILE, ?LINE]),
            {ok, Result};
        Error ->
            io:format("~s ~p Event = Unicast tests failed: ~p.~n", [?FILE, ?LINE, Error]),
            Error
    end.

%% @doc 停止所有单播测试进程
stop_all() ->
    io:format("~s ~p Event = Stopping all unicast test processes.~n", [?FILE, ?LINE]),
    dgiot_udp_test_basic:stop_all(),
    ok.

%% @doc 获取单播测试状态
get_status() ->
    #{
        unicast_tests => "available",
        default_port => 18888,
        supported_protocols => ["UDP"]
    }.
