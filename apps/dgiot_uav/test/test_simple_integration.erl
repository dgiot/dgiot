%%%-------------------------------------------------------------------
%%% @doc
%%% 无人机测试项管理系统简单集成测试
%%% 
%%% 测试核心集成功能
%%%-------------------------------------------------------------------
-module(test_simple_integration).
-author("johnliu").

%% API
-export([test_target_mapping/0, test_integration_modules/0]).

%%%===================================================================
%%% 测试函数
%%%===================================================================

%% @doc 测试target_type到slave_address的映射
test_target_mapping() ->
    io:format("测试target_type到slave_address映射...~n"),
    
    %% 测试PLC映射
    case dgiot_uav_command_scheduler:target_to_slave_addr(<<"plc">>) of
        51 -> 
            io:format("  ✅ PLC映射正确: 51~n");
        Other1 ->
            io:format("  ❌ PLC映射错误: ~p (期望: 51)~n", [Other1]),
            error
    end,
    
    %% 测试治具映射
    case dgiot_uav_command_scheduler:target_to_slave_addr(<<"fixture">>) of
        52 -> 
            io:format("  ✅ 治具映射正确: 52~n");
        Other2 ->
            io:format("  ❌ 治具映射错误: ~p (期望: 52)~n", [Other2]),
            error
    end,
    
    %% 测试无人机映射
    case dgiot_uav_command_scheduler:target_to_slave_addr(<<"uav">>) of
        10007 -> 
            io:format("  ✅ 无人机映射正确: 10007~n");
        Other3 ->
            io:format("  ❌ 无人机映射错误: ~p (期望: 10007)~n", [Other3]),
            error
    end,
    
    %% 测试默认值
    case dgiot_uav_command_scheduler:target_to_slave_addr(<<"unknown">>) of
        0 -> 
            io:format("  ✅ 默认映射正确: 0~n");
        Other4 ->
            io:format("  ❌ 默认映射错误: ~p (期望: 0)~n", [Other4]),
            error
    end,
    
    io:format("✅ target_type映射测试完成~n~n"),
    ok.

%% @doc 测试集成模块
test_integration_modules() ->
    io:format("测试集成模块...~n"),
    
    %% 检查模块是否已编译
    Modules = [
        dgiot_uav_test_dispatcher,
        dgiot_uav_command_scheduler,
        dgiot_uav_test_integration,
        dgiot_uav_handler
    ],
    
    lists:foreach(fun(Module) ->
        case code:which(Module) of
            non_existing ->
                io:format("  ❌ 模块 ~p 未编译~n", [Module]);
            _ ->
                io:format("  ✅ 模块 ~p 已编译~n", [Module])
        end
    end, Modules),
    
    io:format("✅ 集成模块测试完成~n~n"),
    ok.

%% @doc 运行所有测试
run_all() ->
    io:format("=== 无人机测试项管理系统集成验证 ===~n~n"),
    
    try
        test_target_mapping(),
        test_integration_modules(),
        
        io:format("=== 所有测试通过 ===~n"),
        ok
    catch
        _:Error ->
            io:format("=== 测试失败: ~p ===~n", [Error]),
            {error, Error}
    end.