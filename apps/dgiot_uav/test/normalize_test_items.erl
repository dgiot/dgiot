%%%-------------------------------------------------------------------
%%% @doc 测试项规范化脚本
%%% 扫描产品 ID 为 <<"343cf21f82">> 的所有设备，检查并修复 content 字段，
%%% 确保其符合测试项数据结构规范。
%%%
%%% 使用方法：
%%%   1. 在 dgiot 节点上编译：c(fix_test_items).
%%%   2. 执行 fix_test_items:run().        （实际更新）
%%%   3. 执行 fix_test_items:run(dry_run). （预览变更，不写入）
%%%-------------------------------------------------------------------
-module(fix_test_items).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot.hrl").

-export([run/0, run/1]).

-define(TEST_ITEM_PRODUCT_ID, <<"343cf21f82">>).

%% 工位简称与 ID 的映射（与前端一致）
-define(STATION_MAP, #{
    <<"桁架"/utf8>>   => 1100,
    <<"拷机1"/utf8>>  => 1200,
    <<"拷机2"/utf8>>  => 1300,
    <<"总测1"/utf8>>  => 1500,
    <<"总测2"/utf8>>  => 1600,
    <<"磁航向"/utf8>> => 1700
}).

%% 目标到数字的映射（可选，用于统一 target 格式）
-define(TARGET_MAP, #{
    <<"工位PLC"/utf8>> => <<"1">>,
    <<"治具"/utf8>>    => <<"2">>,
    <<"无人机"/utf8>>  => <<"3">>
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 执行规范化（实际更新）
run() ->
    run(update).

%% @doc 执行规范化
%% Mode = dry_run | update
run(Mode) when Mode =:= dry_run; Mode =:= update ->
    ?LOG(info, "开始规范化测试项，模式：~p", [Mode]),
    case dgiot_parse:query_object(<<"Device">>, build_query()) of
        {ok, #{<<"results">> := Results}} ->
            Total = length(Results),
            ?LOG(info, "共找到 ~p 个测试项设备", [Total]),
            {Fixed, Errors} = lists:foldl(fun(Device, {FixAcc, ErrAcc}) ->
                case fix_one_device(Device, Mode) of
                    {ok, fixed} -> {FixAcc + 1, ErrAcc};
                    {ok, unchanged} -> {FixAcc, ErrAcc};
                    {error, Reason} -> {FixAcc, [{maps:get(<<"objectId">>, Device), Reason} | ErrAcc]}
                end
            end, {0, []}, Results),
            ?LOG(info, "规范化完成：成功处理 ~p 个，无需修改 ~p 个，失败 ~p 个",
                 [Fixed, Total - Fixed - length(Errors), length(Errors)]),
            lists:foreach(fun({Id, Reason}) ->
                ?LOG(error, "设备 ~s 处理失败：~p", [Id, Reason])
            end, Errors),
            ok;
        {error, Reason} ->
            ?LOG(error, "查询测试项失败：~p", [Reason]),
            error
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% 构建查询条件：产品指针指向指定的产品 ID
build_query() ->
    #{
        <<"where">> => #{
            <<"product">> => #{
                <<"__type">> => <<"Pointer">>,
                <<"className">> => <<"Product">>,
                <<"objectId">> => ?TEST_ITEM_PRODUCT_ID
            }
        },
        <<"limit">> => 1000
    }.

%% 处理单个设备
fix_one_device(Device, Mode) ->
    ObjectId = maps:get(<<"objectId">>, Device),
    DevAddr = maps:get(<<"devaddr">>, Device, <<>>),
    OldContent = maps:get(<<"content">>, Device, #{}),

    case fix_content(OldContent, DevAddr) of
        {ok, NewContent} when NewContent =/= OldContent ->
            case Mode of
                dry_run ->
                    print_diff(ObjectId, OldContent, NewContent),
                    {ok, fixed};
                update ->
                    case dgiot_parse:update_object(<<"Device">>, ObjectId, #{<<"content">> => NewContent}) of
                        {ok, _} ->
                            ?LOG(info, "设备 ~s 已更新", [ObjectId]),
                            {ok, fixed};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        {ok, _Same} ->
            {ok, unchanged};
        {error, Reason} ->
            {error, Reason}
    end.

%% 修复 content 字段
fix_content(Content, DevAddr) ->
    try
        %% 确保 common_params 存在
        CommonParams0 = maps:get(<<"common_params">>, Content, #{}),
        {StationName, StationId} = extract_station_info(DevAddr),
        CommonParams = ensure_common_params(CommonParams0, StationName, StationId),

        %% 确保 is_test_item_device 为 true
        IsTestItem = maps:get(<<"is_test_item_device">>, Content, true),
        FixedIsTestItem = if IsTestItem -> true; true -> true end,

        %% 保留 last_updated（若不存在则添加当前时间戳）
        LastUpdated = maps:get(<<"last_updated">>, Content, dgiot_datetime:now_secs()),

        %% 修复 steps
        Steps0 = maps:get(<<"steps">>, Content, []),
        Steps = fix_steps(Steps0),

        %% 组装新 content
        NewContent = Content#{
            <<"common_params">> => CommonParams,
            <<"is_test_item_device">> => FixedIsTestItem,
            <<"last_updated">> => LastUpdated,
            <<"steps">> => Steps,
            <<"test_item_count">> => length(Steps)
        },
        {ok, NewContent}
    catch
        Class:Reason:Stacktrace ->
            {error, {Class, Reason, Stacktrace}}
    end.

%% 从 devaddr 提取工位信息（格式如 "总测1_001"）
extract_station_info(DevAddr) when is_binary(DevAddr) ->
    case binary:split(DevAddr, <<"_">>) of
        [StationName, _Rest] ->
            case maps:find(StationName, ?STATION_MAP) of
                {ok, StationId} -> {StationName, StationId};
                error -> {<<"未知工位"/utf8>>, 0}
            end;
        _ ->
            {<<"未知工位"/utf8>>, 0}
    end;
extract_station_info(_) ->
    {<<"未知工位"/utf8>>, 0}.

%% 确保 common_params 包含必要字段
ensure_common_params(Params, StationName, StationId) ->
    Params#{
        <<"port">> => maps:get(<<"port">>, Params, 0),
        <<"station_name">> => maps:get(<<"station_name">>, Params, StationName),
        <<"station_number">> => maps:get(<<"station_number">>, Params, StationId),
        <<"test_station_name">> => maps:get(<<"test_station_name">>, Params, StationName)
    }.

%% 修复步骤列表
fix_steps(Steps) when is_list(Steps) ->
    lists:map(fun fix_step/1, Steps);
fix_steps(_) -> [].

%% 修复单个步骤
fix_step(Step) ->
    StepNumber = maps:get(<<"step_number">>, Step, 1),
    ActionType = maps:get(<<"action_type">>, Step, <<"send">>),
    Description = maps:get(<<"description">>, Step, <<>>),
    Target0 = maps:get(<<"target">>, Step, <<"1">>),
    %% 可选：将中文 target 统一为数字（若需保持兼容，可不执行此转换）
    Target = maps:get(Target0, ?TARGET_MAP, Target0),

    %% 处理 send 字段
    Send0 = maps:get(<<"send">>, Step, #{}),
    Send = case ActionType of
        <<"send">> -> ensure_send_content(Send0);
        <<"judge">> -> ensure_judge_content(Send0);
        _ -> Send0
    end,

    %% 处理 receive 字段（如果不需要，可以删除；但为保留原有数据，仅确保格式）
    Receive = maps:get(<<"receive">>, Step, #{}),

    %% 处理 wait 字段
    Wait0 = maps:get(<<"wait">>, Step, undefined),
    Wait = case is_number(Wait0) of
        true -> Wait0;
        false -> 0.0
    end,

    %% 保留 notes
    Notes = maps:get(<<"notes">>, Step, <<>>),

    %% 构建新步骤
    Step#{
        <<"step_number">> => StepNumber,
        <<"action_type">> => ActionType,
        <<"description">> => Description,
        <<"target">> => Target,
        <<"send">> => Send,
        <<"receive">> => Receive,
        <<"wait">> => Wait,
        <<"notes">> => Notes
    }.

%% 确保 send.content 为字符串
ensure_send_content(#{<<"content">> := Content} = Send) when is_binary(Content) ->
    Send;
ensure_send_content(#{<<"content">> := Content} = Send) when is_integer(Content) ->
    Send#{<<"content">> => integer_to_binary(Content)};
ensure_send_content(Send) ->
    Send#{<<"content">> => <<"0">>}.

%% 确保判定步骤的 send.content 为 "1" 或 "2"
ensure_judge_content(#{<<"content">> := Content} = Send) ->
    FixedContent = case Content of
        <<"1">> -> <<"1">>;
        <<"2">> -> <<"2">>;
        1 -> <<"1">>;
        2 -> <<"2">>;
        _ -> <<"1">>   % 默认合格
    end,
    Send#{<<"content">> => FixedContent};
ensure_judge_content(Send) ->
    Send#{<<"content">> => <<"1">>}.

%% 打印变更差异（dry-run 模式）
print_diff(ObjectId, Old, New) ->
    ?LOG(info, "设备 ~s 将更新：", [ObjectId]),
    ?LOG(info, "  old content: ~p", [Old]),
    ?LOG(info, "  new content: ~p", [New]).