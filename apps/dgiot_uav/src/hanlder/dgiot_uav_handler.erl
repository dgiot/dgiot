%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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
-module(dgiot_uav_handler).
-author("johnliu").
-behavior(dgiot_rest).
-dgiot_rest(all).

-include_lib("dgiot/include/logger.hrl").

-route_path("/uav").
-route_path("/uav/batch").

-define(UAV_PRODUCT_ID, <<"6235befb62">>).

%% 寄存器地址常量
-define(PLC_REG_ADDR, 51).          %% PLC 寄存器地址
-define(FIXTURE_REG_ADDR, 52).       %% 治具寄存器地址
-define(DRONE_REG_ADDR, 50).         %% 无人机寄存器地址

%% 从站地址常量
-define(FIXTURE_SLAVE, 10006).        %% 治具从站地址
-define(DRONE_SLAVE, 10007).          %% 无人机从站地址

-export([swagger_uav/0, handle/4]).

%% 抑制未使用变量警告
-compile(nowarn_unused_vars).

swagger_uav() ->
    [dgiot_http_server:bind(<<"/swagger_uav.json">>, ?MODULE, [], priv)].

handle(OperationID, Args, Context, Req) ->
    ?LOG(info, "收到请求: OperationID=~p, Args=~p", [OperationID, Args]),
    try do_request(OperationID, Args, Context, Req) of
        {ok, Resp} -> 
            ?LOG(info, "请求成功: OperationID=~p, Resp=~p", [OperationID, Resp]),
            {200, #{}, Resp, Req};
        {error, {Code, Msg}} -> 
            ?LOG(error, "请求失败: OperationID=~p, Code=~p, Msg=~p", [OperationID, Code, Msg]),
            {Code, #{}, Msg}
    catch
        exit:{timeout, _} = Reason ->
            ?LOG(error, "调用超时: ~p", [Reason]),
            {504, #{}, #{<<"status">> => 1, <<"msg">> => <<"PLC 响应超时"/utf8>>}};
        Class:Reason:Stack ->
            ?LOG(error, "Handler异常: ~p:~p ~p", [Class, Reason, Stack]),
            {500, #{}, #{<<"status">> => 1, <<"msg">> => <<"内部错误"/utf8>>}}
    end.

%% 单步接口
do_request(post_uav,
           #{<<"devAddr">> := DevAddr,
             <<"object">> := Object,
             <<"instruction_code">> := InstCode} = AllArgs,
           _Context, _Req) ->
    WaitTime = maps:get(<<"wait_time">>, AllArgs, 0),
    TestItemId = maps:get(<<"test_item_id">>, AllArgs, undefined),
    StepIndex = maps:get(<<"step_index">>, AllArgs, undefined),

    ?LOG(error, "单步指令: devAddr=~p, object=~p, instruction_code=~p, wait_time=~p, test_item_id=~p, step_index=~p",
         [DevAddr, Object, InstCode, WaitTime, TestItemId, StepIndex]),

    case validate_devaddr(DevAddr) of
        ok ->
            %% 根据对象类型获取通信参数和实际值
            {SlaveAddr, InstAddr, Value} = object_to_addrs_and_value(Object, DevAddr, InstCode),
            ?LOG(debug, "映射结果: SlaveAddr=~p, InstAddr=~p, Value=~p", [SlaveAddr, InstAddr, Value]),

            %% 调用命令调度器（传递测试项信息）
            try dgiot_uav_command_scheduler:send_command(DevAddr, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) of
                ok ->
                    ?LOG(info, "指令下发成功: devAddr=~p, object=~p, code=~p", [DevAddr, Object, InstCode]),
                    record_test_step(TestItemId, StepIndex, <<"executed">>, DevAddr),
                    if WaitTime > 0 -> 
                        ?LOG(debug, "等待 ~p 秒", [WaitTime]),
                        timer:sleep(round(WaitTime * 1000)); 
                       true -> ok 
                    end,
                    {ok, #{<<"status">> => 0,
                           <<"msg">> => <<"指令下发成功"/utf8>>,
                           <<"data">> => #{
                               <<"timestamp">> => erlang:system_time(millisecond),
                               <<"station_id">> => parse_station_id(DevAddr)
                           }}};
                {error, Reason} ->
                    ?LOG(error, "指令下发失败: ~p", [Reason]),
                    %% 安全转换错误原因
                    MsgBin = safe_term_to_binary(Reason),
                    {error, {500, #{<<"status">> => 1, <<"msg">> => MsgBin}}}
            catch
                _:Why ->
                    ?LOG(error, "调用 send_command 异常: ~p", [Why]),
                    {error, {500, #{<<"status">> => 1, <<"msg">> => <<"内部调度异常"/utf8>>}}}
            end;
        {error, Reason} ->
            ?LOG(error, "工位地址无效: ~p", [Reason]),
            {error, {400, #{<<"status">> => 1, <<"msg">> => Reason}}}
    end;

%% 批量接口
do_request(post_uav_batch, Body, _Context, _Req) ->
    ?LOG(info, "批量指令: Body=~p", [Body]),
    try
        #{<<"devAddr">> := DevAddr, <<"steps">> := Steps} = Body,
        ?LOG(info, "批量指令 devAddr=~p, steps数量=~p", [DevAddr, length(Steps)]),
        case validate_devaddr(DevAddr) of
            ok ->
                Results = lists:map(fun(Step) ->
                    #{<<"object">> := Object,
                      <<"instruction_code">> := InstCode,
                      <<"wait_time">> := WaitTime} = Step,
                    TestItemId = maps:get(<<"test_item_id">>, Step, undefined),
                    StepIndex = maps:get(<<"step_index">>, Step, undefined),

                    ?LOG(debug, "处理步骤: object=~p, code=~p, wait=~p, step_index=~p",
                         [Object, InstCode, WaitTime, StepIndex]),

                    {SlaveAddr, InstAddr, Value} = object_to_addrs_and_value(Object, DevAddr, InstCode),

                    try dgiot_uav_command_scheduler:send_command(DevAddr, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) of
                        ok ->
                            ?LOG(debug, "步骤成功: object=~p, code=~p", [Object, InstCode]),
                            record_test_step(TestItemId, StepIndex, <<"executed">>, DevAddr),
                            timer:sleep(round(WaitTime * 1000)),
                            #{<<"step">> => StepIndex, <<"success">> => true, <<"message">> => <<"指令下发成功"/utf8>>};
                        {error, Reason} ->
                            ?LOG(warning, "步骤失败: object=~p, code=~p, reason=~p", [Object, InstCode, Reason]),
                            MsgBin = safe_term_to_binary(Reason),
                            #{<<"step">> => StepIndex, <<"success">> => false, <<"message">> => MsgBin}
                    catch
                        _:Why ->
                            ?LOG(error, "步骤异常: object=~p, code=~p, why=~p", [Object, InstCode, Why]),
                            #{<<"step">> => StepIndex, <<"success">> => false, <<"message">> => <<"内部异常"/utf8>>}
                    end
                end, Steps),
                SuccessCount = length([R || #{<<"success">> := true} = R <- Results]),
                ?LOG(info, "批量执行完成: 成功 ~p / ~p 步", [SuccessCount, length(Steps)]),
                {ok, #{<<"status">> => 0,
                       <<"msg">> => <<"指令集执行完成"/utf8>>,
                       <<"data">> => #{
                           <<"executed_steps">> => SuccessCount,
                           <<"results">> => Results
                       }}};
            {error, Reason} ->
                ?LOG(error, "工位地址无效: ~p", [Reason]),
                {error, {400, #{<<"status">> => 1, <<"msg">> => Reason}}}
        end
    catch
        _:_ ->
            ?LOG(error, "请求体格式错误: Body=~p", [Body]),
            {error, {400, #{<<"status">> => 1, <<"msg">> => <<"请求体格式错误"/utf8>>}}}
    end;

do_request(put_amis_device_objectid, #{<<"objectId">> := ObjectId} = Args, _Context, _Req) ->
    ?LOG(info, "更新AMIS设备: objectId=~p, Args=~p", [ObjectId, Args]),
    Body = maps:get(<<"body">>, Args, #{}),
    case dgiot_parse:update_object(<<"Device">>, ObjectId, Body) of
        {ok, UpdatedDevice} ->
            ?LOG(info, "设备更新成功: objectId=~p", [ObjectId]),
            {ok, #{<<"status">> => 0, <<"msg">> => <<"更新成功"/utf8>>, <<"data">> => UpdatedDevice}};
        {error, Reason} ->
            ?LOG(error, "设备更新失败: objectId=~p, Reason=~p", [ObjectId, Reason]),
            {error, {500, #{<<"status">> => 1, <<"msg">> => <<"设备更新失败"/utf8>>}}}
    end;

do_request(_, _, _, _) ->
    ?LOG(error, "不支持的请求方法"),
    {error, {405, #{<<"status">> => 1, <<"msg">> => <<"Method Not Allowed"/utf8>>}}}.

%% 安全转换任意 Erlang 项为二进制
safe_term_to_binary(Term) ->
    try iolist_to_binary(io_lib:format("~p", [Term]))
    catch _:_ -> <<"未知错误">>
    end.

%% 验证工位地址格式
validate_devaddr(<<"D", Bin/binary>>) ->
    case catch binary_to_integer(Bin) of
        N when is_integer(N), N >= 1100, N =< 1700 -> 
            ?LOG(debug, "工位地址有效: D~p", [N]),
            ok;
        _ -> 
            ?LOG(warning, "工位地址无效: D~s", [Bin]),
            {error, <<"工位地址无效"/utf8>>}
    end;
validate_devaddr(_) ->
    {error, <<"工位地址格式应为 D1100~D1700"/utf8>>}.

parse_station_id(<<"D", Bin/binary>>) ->
    binary_to_integer(Bin).

%% 根据对象类型和指令码映射端口、指令地址和实际值
object_to_addrs_and_value(<<"PLC"/utf8>>, DevAddr, InstCode) ->
    StationId = parse_station_id(DevAddr),
    {StationId, ?PLC_REG_ADDR, InstCode};

object_to_addrs_and_value(<<"治具"/utf8>>, _DevAddr, InstCode) ->
    Value = case InstCode of
        1 -> 16#FF00;  % 控制大继电器上电
        2 -> 16#0000;  % 控制大继电器断电
        3 -> 16#FF00;  % 启动无人机
        4 -> 16#0000;  % 关闭无人机
        5 -> 16#FF00;  % 风速管堵上
        6 -> 16#0000;  % 风速管打开
        7 -> 16#0001;  % 测试引信9,10点电阻
        8 -> 16#0002;  % 测试引信7,8点电阻
        9 -> 16#0004;  % 测试引信7和后翼安装钉电阻
        10 -> 16#0006; % 测试引信8和后翼安装钉电阻
        11 -> 16#0008; % 测无人机电池端口电阻
        12 -> 16#000A; % 测试引信5点与地电压
        13 -> 16#0008; % 测试引信1点与地电压
        14 -> 16#000D; % 读取工位信息
        _ -> 16#0000
    end,
    {?FIXTURE_SLAVE, ?FIXTURE_REG_ADDR, Value};

object_to_addrs_and_value(<<"无人机"/utf8>>, _DevAddr, InstCode) ->
    {?DRONE_SLAVE, ?DRONE_REG_ADDR, InstCode}.

%% 记录测试步骤到无人机物模型（带异常保护）
record_test_step(undefined, _, _, _) -> ok;
record_test_step(TestItemId, StepIndex, Result, DevAddr) ->
    spawn(fun() ->
        try
            StationId = parse_station_id(DevAddr),
            DroneId = case catch dgiot_uav_business_service:get_drone_by_station(StationId) of
                {ok, Id} -> Id;
                _ -> 
                    ?LOG(warning, "无法获取无人机ID，使用默认值"),
                    <<"unknown">>
            end,
            TestData = #{
                <<"test_item_device_id">> => TestItemId,
                <<"test_step">> => StepIndex,
                <<"test_result">> => Result,
                <<"createdat">> => erlang:system_time(millisecond)
            },
            case catch uav_thing_model:save_thing_model_data(?UAV_PRODUCT_ID, DroneId, TestData) of
                ok -> ?LOG(debug, "测试步骤记录成功");
                Err -> ?LOG(warning, "测试步骤记录失败: ~p", [Err])
            end,
            
            %% 同时调用聚合器进行数据汇聚
            Timestamp = erlang:system_time(millisecond),
            case whereis(dgiot_uav_aggregator) of
                undefined ->
                    ?LOG(warning, "聚合器未启动，尝试启动"),
                    dgiot_uav_aggregator:start_link(),
                    timer:sleep(100);
                _ -> ok
            end,
            dgiot_uav_aggregator:aggregate(DroneId, ?UAV_PRODUCT_ID, TestData, Timestamp),
            ?LOG(debug, "测试步骤数据已提交到聚合器: DroneId=~s, TestItemId=~s", [DroneId, TestItemId])
        catch _:_ -> 
            ?LOG(warning, "记录测试步骤时发生异常，忽略")
        end
    end).