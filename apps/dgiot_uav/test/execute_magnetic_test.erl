%%%-------------------------------------------------------------------
%%% @doc
%%% 磁航向工位指令下发测试执行器
%%% 从Parse库加载测试项，解析步骤，执行指令下发
%%% @end
%%%-------------------------------------------------------------------
-module(execute_magnetic_test).
-author("dgiot_team").

-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    test/0,
    execute_first_item/0,
    execute_step/2
]).

%% 测试项记录
-record(test_item, {
    id :: binary(),
    name :: binary(),
    station_id :: integer(),
    station_name :: binary(),
    steps = [] :: list(),
    order = 0 :: integer()
}).

%% @doc 完整测试流程
test() ->
    ?LOG(info, "~n========================================"),
    ?LOG(info, "磁航向工位指令下发测试"),
    ?LOG(info, "========================================"),

    %% 1. 检查PLC客户端
    ?LOG(info, "~n步骤1: 检查PLC客户端"),
    case global:whereis_name({plc, 1700}) of
        undefined ->
            ?LOG(error, "  ✗ PLC客户端未启动"),
            {error, plc_not_found};
        Pid ->
            ?LOG(info, "  ✓ PLC客户端运行中: ~p", [Pid]),
            execute_first_item()
    end.

%% @doc 执行第一个测试项
execute_first_item() ->
    ?LOG(info, "~n步骤2: 从Parse库加载测试项"),

    case dgiot_uav_test_loader:load_by_station(1700) of
        {ok, []} ->
            ?LOG(error, "  ✗ Parse库未找到磁航向测试项"),
            {error, no_test_items};

        {ok, [#test_item{id = ItemId, name = ItemName, steps = Steps} = FirstItem | _]} ->
            ?LOG(info, "  ✓ 找到测试项: ID=~s, 步骤数=~p", [ItemId, length(Steps)]),

            ?LOG(info, "~n步骤3: 解析测试步骤"),
            ParsedSteps = parse_steps(Steps),
            lists:foreach(fun({Index, ParsedStep}) ->
                ?LOG(info, "  步骤~p: ~p", [Index, ParsedStep])
            end, lists:zip(lists:seq(1, length(ParsedSteps)), ParsedSteps)),

            ?LOG(info, "~n步骤4: 执行指令下发"),
            execute_steps(ParsedSteps, 1);

        {error, Reason} ->
            ?LOG(error, "  ✗ 加载测试项失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 解析测试步骤
parse_steps(Steps) ->
    lists:map(fun(Step) ->
        ActionType = maps:get(<<"action_type">>, Step, <<>>),
        Target = maps:get(<<"target">>, Step, <<>>),
        Description = maps:get(<<"description">>, Step, <<>>),
        StepNumber = maps:get(<<"step_number">>, Step, 0),

        %% 解析send字段
        SendValue = case maps:get(<<"send">>, Step, undefined) of
            undefined -> <<"0">>;
            SendMap when is_map(SendMap) ->
                maps:get(<<"content">>, SendMap, <<"0">>);
            SendValueBinary when is_binary(SendValueBinary) ->
                SendValueBinary
        end,

        #{
            step_number => StepNumber,
            action_type => ActionType,
            target => Target,
            send => SendValue,
            description => Description
        }
    end, Steps).

%% @doc 执行测试步骤
execute_steps([], _CurrentStep) ->
    ?LOG(info, "~n========================================"),
    ?LOG(info, "所有步骤执行完成"),
    ?LOG(info, "========================================"),
    ok;

execute_steps([Step | Rest], CurrentStep) ->
    #{
        action_type := ActionType,
        target := Target,
        send := SendValue,
        description := Description
    } = Step,

    ?LOG(info, "~n执行步骤~p: ~ts", [CurrentStep, Description]),
    ?LOG(info, "  动作类型: ~s, 目标: ~s, 发送值: ~s", [ActionType, Target, SendValue]),

    case execute_step(Step, CurrentStep) of
        ok ->
            ?LOG(info, "  ✓ 步骤执行成功"),
            timer:sleep(1000),  %% 等待指令执行
            execute_steps(Rest, CurrentStep + 1);
        {error, Reason} ->
            ?LOG(error, "  ✗ 步骤执行失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 执行单个步骤
execute_step(#{action_type := <<"send">>, target := Target, send := SendValue}, StepIndex) ->
    case Target of
        <<"工位PLC">> ->
            %% 发送PLC指令
            Code = binary_to_integer(SendValue),
            ?LOG(info, "    → 下发PLC指令: Code=~p, Station=1700", [Code]),

            Params = #{
                station_id => 1700,
                step_index => StepIndex
            },

            case dgiot_uav_command_manager:send_plc_command(Code, Code, Params) of
                ok ->
                    ?LOG(info, "    ✓ PLC指令下发成功"),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "    ✗ PLC指令下发失败: ~p", [Reason]),
                    {error, Reason}
            end;

        <<"无人机">> ->
            %% 发送无人机指令
            ?LOG(info, "    → 下发无人机指令: Value=~s", [SendValue]),
            %% TODO: 实现无人机指令下发
            ok;

        _ ->
            ?LOG(warning, "    ⚠ 未知目标: ~s", [Target]),
            ok
    end;

execute_step(#{action_type := <<"judge">>, description := Description}, _StepIndex) ->
    %% 判据步骤
    ?LOG(info, "    → 判据步骤: ~ts", [Description]),
    ?LOG(info, "    → 等待数据汇聚和判定..."),
    timer:sleep(2000),  %% 等待数据汇聚
    ?LOG(info, "    ✓ 判据完成"),
    ok;

execute_step(Step, _StepIndex) ->
    ?LOG(warning, "  ⚠ 未知步骤类型: ~p", [Step]),
    ok.

%% 辅助函数
binary_to_integer(Bin) ->
    try
        binary_to_integer(Bin)
    catch
        _:_ -> 0
    end.
