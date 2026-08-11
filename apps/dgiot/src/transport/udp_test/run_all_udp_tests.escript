#!/usr/bin/env escript
%%! -pa ./_build/emqx/lib/*/ebin ./apps/*/ebin

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

%% @doc UDP测试运行脚本
%% 统一运行所有UDP测试的便捷脚本

main([]) ->
    io:format("~n" ++ string:chars($=, 60) ++ "~n", []),
    io:format("=== DGIOT UDP测试套件启动 ===~n", []),
    io:format("时间: ~s~n", [format_timestamp()]),
    io:format(string:chars($=, 60) ++ "~n~n", []),
    
    % 检查依赖模块是否加载
    case check_dependencies() of
        ok ->
            run_comprehensive_tests();
        {error, Reason} ->
            io:format("依赖检查失败: ~p~n", [Reason]),
            halt(1)
    end;

main(["unicast"]) ->
    io:format("运行单播测试...~n", []),
    run_unicast_tests();

main(["broadcast"]) ->
    io:format("运行广播测试...~n", []),
    run_broadcast_tests();

main(["multicast"]) ->
    io:format("运行多播测试...~n", []),
    run_multicast_tests();

main(["help"]) ->
    show_help();

main([_Unknown]) ->
    io:format("未知参数，使用 'help' 查看可用选项~n", []),
    show_help().

%%%===================================================================
%%% 测试函数
%%%===================================================================

run_comprehensive_tests() ->
    StartTime = erlang:system_time(millisecond),
    
    Results = #{
        unicast => run_unicast_tests(),
        broadcast => run_broadcast_tests(),
        multicast => run_multicast_tests()
    },
    
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    
    io:format("~n" ++ string:chars($=, 60) ++ "~n", []),
    io:format("=== 测试套件完成 ===~n", []),
    io:format("总耗时: ~p 毫秒~n", [Duration]),
    io:format("最终结果: ~n", []),
    print_final_results(Results),
    io:format(string:chars($=, 60) ++ "~n", []),
    
    case all_tests_passed(Results) of
        true ->
            io:format("✓ 所有测试通过！~n", []),
            halt(0);
        false ->
            io:format("✗ 部分测试失败！~n", []),
            halt(1)
    end.

run_unicast_tests() ->
    io:format("~n" ++ string:chars($-, 40) ++ "~n", []),
    io:format("单播测试套件~n", []),
    io:format(string:chars($-, 40) ++ "~n", []),
    
    try
        % 运行基础单播测试
        io:format("1. 基础单播通信测试...~n", []),
        BasicResult = dgiot_udp_test_unicast:run_tests(),
        
        % 运行综合单播测试
        io:format("2. 综合单播测试...~n", []),
        ComprehensiveResult = unicast_comprehensive_test:run_all_tests(),
        
        Summary = #{
            basic => BasicResult,
            comprehensive => ComprehensiveResult
        },
        
        io:format("✓ 单播测试套件完成~n", []),
        {ok, Summary}
        
    catch
        Error:Reason ->
            io:format("✗ 单播测试套件失败: ~p:~p~n", [Error, Reason]),
            {error, {unicast_failed, Error, Reason}}
    end.

run_broadcast_tests() ->
    io:format("~n" ++ string:chars($-, 40) ++ "~n", []),
    io:format("广播测试套件~n", []),
    io:format(string:chars($-, 40) ++ "~n", []),
    
    try
        % 运行基础广播测试
        io:format("1. 基础广播测试...~n", []),
        BasicResult = dgiot_udp_test_broadcast:run_tests(),
        
        % 运行综合广播测试
        io:format("2. 综合广播测试...~n", []),
        ComprehensiveResult = broadcast_comprehensive_test:run_all_tests(),
        
        Summary = #{
            basic => BasicResult,
            comprehensive => ComprehensiveResult
        },
        
        io:format("✓ 广播测试套件完成~n", []),
        {ok, Summary}
        
    catch
        Error:Reason ->
            io:format("✗ 广播测试套件失败: ~p:~p~n", [Error, Reason]),
            {error, {broadcast_failed, Error, Reason}}
    end.

run_multicast_tests() ->
    io:format("~n" ++ string:chars($-, 40) ++ "~n", []),
    io:format("多播测试套件~n", []),
    io:format(string:chars($-, 40) ++ "~n", []),
    
    try
        % 运行基础多播测试
        io:format("1. 基础多播测试...~n", []),
        BasicResult = dgiot_udp_test_multicast:run_tests(),
        
        % 运行综合多播测试
        io:format("2. 综合多播测试...~n", []),
        ComprehensiveResult = dgiot_udp_test_utils_multicast:run_all_tests(),
        
        Summary = #{
            basic => BasicResult,
            comprehensive => ComprehensiveResult
        },
        
        io:format("✓ 多播测试套件完成~n", []),
        {ok, Summary}
        
    catch
        Error:Reason ->
            io:format("✗ 多播测试套件失败: ~p:~p~n", [Error, Reason]),
            {error, {multicast_failed, Error, Reason}}
    end.

%%%===================================================================
%%% 工具函数
%%%===================================================================

check_dependencies() ->
    RequiredModules = [
        dgiot_udp_test_unicast,
        dgiot_udp_test_broadcast,
        dgiot_udp_test_multicast,
        unicast_comprehensive_test,
        broadcast_comprehensive_test,
        dgiot_udp_test_utils_multicast
    ],
    
    case lists:all(fun code:is_loaded/1, RequiredModules) of
        true ->
            ok;
        false ->
            {error, missing_dependencies}
    end.

print_final_results(Results) ->
    maps:foreach(
        fun(TestType, Result) ->
            TypeName = case TestType of
                unicast -> "单播测试";
                broadcast -> "广播测试";
                multicast -> "多播测试"
            end,
            
            case Result of
                {ok, _} ->
                    io:format("  ✓ ~s: 通过~n", [TypeName]);
                {error, Reason} ->
                    io:format("  ✗ ~s: 失败 (~p)~n", [TypeName, Reason]);
                _ ->
                    io:format("  ? ~s: 未知状态~n", [TypeName])
            end
        end,
        Results
    ).

all_tests_passed(Results) ->
    lists:all(
        fun({_Type, Result}) ->
            case Result of
                {ok, _} -> true;
                _ -> false
            end
        end,
        maps:to_list(Results)
    ).

format_timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", 
                 [Year, Month, Day, Hour, Minute, Second]).

show_help() ->
    io:format("~nDGIOT UDP测试套件使用说明~n", []),
    io:format(string:chars($-, 40) ++ "~n", []),
    io:format("用法: ~s [选项]~n", [escript:script_name()]),
    io:format("~n选项:~n", []),
    io:format("  (无参数)   运行所有测试~n", []),
    io:format("  unicast    仅运行单播测试~n", []),
    io:format("  broadcast  仅运行广播测试~n", []),
    io:format("  multicast  仅运行多播测试~n", []),
    io:format("  help       显示此帮助信息~n", []),
    io:format("~n示例:~n", []),
    io:format("  ~s              # 运行所有测试~n", [escript:script_name()]),
    io:format("  ~s unicast      # 仅运行单播测试~n", [escript:script_name()]),
    io:format("  ~s broadcast    # 仅运行广播测试~n", [escript:script_name()]),
    io:format(string:chars($-, 40) ++ "~n", []).
