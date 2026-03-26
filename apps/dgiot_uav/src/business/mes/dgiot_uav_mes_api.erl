%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_mes_api - 无人机MES上报API
%%%
%%% 实现向MES系统上报设备状态、测试数据等的核心接口。
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_mes_api).

%% API
-export([
    % 设备状态上报
    report_device_status/4,
    report_production_status/4,
    report_device_fault/6,
    report_drone_fault/5,
    
    % 测试开始上报
    report_test_start/4,
    
    % 测试结果上报
    report_test_completion/5,
    
    % 无人机上线状态上报
    report_drone_online_to_mes/4,
    
    % 通用上报函数
    send_to_mes/1,
    
    % MES配置获取
    get_mes_config/0
]).

-include_lib("dgiot/include/logger.hrl").
-include("dgiot_uav.hrl").

%% 产线状态类型编码（根据Postman文档更新）
-define(LINE_STA_START, 1).      % 开工
-define(LINE_STA_PRODUCTION, 2). % 生产/开工
-define(LINE_STA_COMPLETION, 3). % 完工
-define(LINE_STA_DEVICE_FAULT, 4). % 设备异常
-define(LINE_STA_DRONE_FAULT, 5).  % 机身异常

%% 无人机所在大工序类型
-define(DRONE_TYPE_FINAL_TEST, 1).    % 1-总测
-define(DRONE_TYPE_BURN_IN, 2).       % 2-拷机
-define(DRONE_TYPE_INSPECTION, 3).    % 3-所检
-define(DRONE_TYPE_MAGNETIC, 4).      % 4-磁航向

%% 获取MES配置
get_mes_config() ->
    #{
        api_url => application:get_env(dgiot_uav, mes_api_url, "http://172.1.2.222/lezao/jymes/api/equip/proExec"),
        timeout => application:get_env(dgiot_uav, mes_api_timeout, 30000),  % 增加到30秒,适应MES服务响应
        token => application:get_env(dgiot_uav, mes_api_token, "")
    }.

%%%===================================================================
%%% API
%%%===================================================================

-spec report_device_status(binary(), binary(), binary(), map()) -> 
    {ok, map()} | {error, term()}.
report_device_status(FuncId, LineNo, DroneNo, ExtraData) ->
    ?LOG(info, "上报设备状态: func=~p, line=~p, drone=~p", [FuncId, LineNo, DroneNo]),
    MesData = #{
        <<"func_id">> => FuncId,
        <<"line_no">> => LineNo,
        <<"line_sta">> => ?LINE_STA_START,
        <<"drone_no">> => DroneNo,
        <<"date_time">> => dgiot_uav_mes_utils:get_current_timestamp(),
        <<"data_record">> => ExtraData
    },
    send_to_mes(MesData).

-spec report_production_status(binary(), binary(), binary(), map()) -> 
    {ok, map()} | {error, term()}.
report_production_status(FuncId, LineNo, DroneNo, DataRecord) ->
    ?LOG(info, "上报生产状态: func=~p, line=~p, drone=~p", [FuncId, LineNo, DroneNo]),
    case dgiot_uav_mes_utils:validate_production_data(DataRecord) of
        true ->
            MesData = #{
                <<"func_id">> => FuncId,
                <<"line_no">> => LineNo,
                <<"line_sta">> => ?LINE_STA_PRODUCTION,
                <<"drone_no">> => DroneNo,
                <<"date_time">> => dgiot_uav_mes_utils:get_current_timestamp(),
                <<"data_record">> => DataRecord
            },
            send_to_mes(MesData);
        false ->
            {error, invalid_production_data}
    end.

-spec report_device_fault(binary(), binary(), binary(), binary(), integer(), map()) -> 
    {ok, map()} | {error, term()}.
report_device_fault(FuncId, LineNo, DeviceId, Location, FaultLevel, FaultData) ->
    ?LOG(warning, "上报设备故障: func=~p, line=~p, device=~p, location=~p, level=~p", 
         [FuncId, LineNo, DeviceId, Location, FaultLevel]),
    FaultRecord = #{
        <<"device_id">> => DeviceId,
        <<"location">> => Location,
        <<"device_code">> => dgiot_uav_mes_utils:generate_fault_code(FaultLevel),
        <<"device_sta">> => 0,
        <<"fault_ts">> => dgiot_uav_mes_utils:get_current_timestamp(),
        <<"device_lev">> => FaultLevel
    },
    FullFaultRecord = maps:merge(FaultRecord, FaultData),
    MesData = #{
        <<"func_id">> => FuncId,
        <<"line_no">> => LineNo,
        <<"line_sta">> => ?LINE_STA_DEVICE_FAULT,
        <<"date_time">> => dgiot_uav_mes_utils:get_current_timestamp(),
        <<"data_record">> => FullFaultRecord
    },
    send_to_mes(MesData).

-spec report_drone_fault(binary(), binary(), binary(), binary(), map()) -> 
    {ok, map()} | {error, term()}.
report_drone_fault(FuncId, LineNo, DroneNo, FaultDesc, FaultData) ->
    ?LOG(warning, "上报机身故障: func=~p, line=~p, drone=~p, desc=~p", 
         [FuncId, LineNo, DroneNo, FaultDesc]),
    FaultRecord = #{
        <<"drone_no">> => DroneNo,
        <<"fault_desc">> => FaultDesc,
        <<"fault_time">> => dgiot_uav_mes_utils:get_current_timestamp()
    },
    FullFaultRecord = maps:merge(FaultRecord, FaultData),
    MesData = #{
        <<"func_id">> => FuncId,
        <<"line_no">> => LineNo,
        <<"line_sta">> => ?LINE_STA_DRONE_FAULT,
        <<"date_time">> => dgiot_uav_mes_utils:get_current_timestamp(),
        <<"data_record">> => FullFaultRecord
    },
    send_to_mes(MesData).

-spec report_test_start(binary(), binary(), binary(), map()) -> 
    {ok, map()} | {error, term()}.
report_test_start(FuncId, LineNo, DroneNo, TestData) ->
    ?LOG(info, "上报测试开始: func=~p, line=~p, drone=~p", [FuncId, LineNo, DroneNo]),
    %% 根据工位类型打印预计测试时间
    case binary:match(LineNo, <<"总测">>) of
        {_, _} -> ?LOG(info, "总测工位测试开始，预计测试时间：5分钟", []);
        _ -> ok
    end,
    case binary:match(LineNo, <<"磁航向">>) of
        {_, _} -> ?LOG(info, "磁航向工位测试开始，预计测试时间：2分钟", []);
        _ -> ok
    end,
    case binary:match(LineNo, <<"拷机">>) of
        {_, _} -> ?LOG(info, "拷机工位测试开始，预计测试时间：15分钟", []);
        _ -> ok
    end,
    case binary:match(LineNo, <<"桁架">>) of
        {_, _} -> ?LOG(info, "桁架工位测试开始，预计测试时间：3分钟", []);
        _ -> ok
    end,
    DataRecord = #{
        <<"drone_no">> => DroneNo,
        <<"drone_type">> => ?DRONE_TYPE_FINAL_TEST,
        <<"line_proc_no">> => dgiot_uav_mes_utils:get_line_proc_no(LineNo),
        <<"eqp_action_list">> => <<"机身放置到倍速链"/utf8>>
    },
    FullDataRecord = maps:merge(DataRecord, TestData),
    MesData = #{
        <<"func_id">> => FuncId,
        <<"line_no">> => LineNo,
        <<"line_sta">> => ?LINE_STA_PRODUCTION,
        <<"drone_no">> => DroneNo,
        <<"date_time">> => dgiot_uav_mes_utils:get_current_timestamp(),
        <<"data_record">> => FullDataRecord
    },
    send_to_mes(MesData).

-spec report_test_completion(binary(), binary(), binary(), integer(), map()) -> 
    {ok, map()} | {error, term()}.
report_test_completion(FuncId, LineNo, DroneNo, TestType, TestResults) ->
    ?LOG(info, "上报测试完成: func=~p, line=~p, drone=~p, type=~p", 
         [FuncId, LineNo, DroneNo, TestType]),
    %% 确保测试结果包含通过状态
    EnhancedResults = TestResults#{<<"test_status">> => <<"通过">>},
    DataRecord = #{
        <<"drone_no">> => DroneNo,
        <<"parameter_list">> => dgiot_uav_mes_utils:format_test_parameters(EnhancedResults)
    },
    FinalDataRecord = case maps:get(<<"rpt_link">>, TestResults, undefined) of
        undefined -> DataRecord;
        RptLink -> DataRecord#{<<"rpt_link">> => RptLink}
    end,
    MesData = #{
        <<"func_id">> => FuncId,
        <<"line_no">> => LineNo,
        <<"line_sta">> => ?LINE_STA_COMPLETION,
        <<"drone_no">> => DroneNo,
        <<"date_time">> => dgiot_uav_mes_utils:get_current_timestamp(),
        <<"data_record">> => FinalDataRecord
    },
    send_to_mes(MesData).

-spec send_to_mes(map()) -> {ok, map()} | {error, term()}.
send_to_mes(MesData) ->
    ?LOG(debug, "发送数据到MES: ~p", [MesData]),
    case dgiot_uav_mes_utils:validate_mes_data(MesData) of
        true ->
            JsonData = jsx:encode(MesData),
            
            %% 使用error级别打印MES报文（确保在任何日志级别下都能看到）
            ?LOG(error, "~n========================================~n"
                        "MES上报报文（工序开始）~n"
                        "========================================~n"
                        "~s~n"
                        "========================================", [JsonData]),
            
            %% 获取MES配置
            #{api_url := ApiUrl, timeout := Timeout, token := Token} = get_mes_config(),
            
            Headers = [
                {"token", Token},
                {"Content-Type", "application/json;charset=UTF-8"}
            ],
            case httpc:request(post, 
                    {ApiUrl, Headers, "application/json", JsonData},
                    [{timeout, Timeout}], 
                    []) of
                {ok, {{_, 200, _}, _ResponseHeaders, Body}} ->
                    ResponseBody = list_to_binary(Body),
                    ?LOG(error, "~n========================================~n"
                                "MES响应报文（成功）~n"
                                "========================================~n"
                                "HTTP状态码: 200~n"
                                "响应体: ~s~n"
                                "========================================", [ResponseBody]),
                    case jsx:decode(ResponseBody, [return_maps]) of
                        #{<<"code">> := 200} = Response ->
                            ?LOG(info, "MES上报成功: ~p", [Response]),
                            {ok, Response};
                        #{<<"code">> := Code, <<"msg">> := Msg} ->
                            ?LOG(error, "MES上报失败: code=~p, msg=~p", [Code, Msg]),
                            {error, {mes_error, Code, Msg}};
                        _ ->
                            ?LOG(error, "MES响应格式错误: ~p", [Body]),
                            {error, invalid_response_format}
                    end;
                {ok, {{_, StatusCode, _StatusText},_ResponseHeaders, Body}} ->
                    ResponseBody = list_to_binary(Body),
                    ?LOG(error, "~n========================================~n"
                                "MES响应报文（HTTP错误）~n"
                                "========================================~n"
                                "HTTP状态码: ~p~n"
                                "响应体: ~s~n"
                                "========================================", [StatusCode, ResponseBody]),
                    {error, {http_error, StatusCode, Body}};
                {error, Reason} ->
                    ?LOG(error, "~n========================================~n"
                                "MES请求失败~n"
                                "========================================~n"
                                "错误原因: ~p~n"
                                "========================================", [Reason]),
                    {error, Reason}
            end;
        false ->
            ?LOG(error, "~n========================================~n"
                        "MES数据验证失败~n"
                        "========================================~n"
                        "无效的数据: ~p~n"
                        "========================================", [MesData]),
            {error, invalid_mes_data}
    end.

%%%===================================================================
%%% 无人机上线状态上报
%%%===================================================================

%% @doc 上报无人机上线状态到MES系统
-spec report_drone_online_to_mes(binary(), integer(), binary(), map()) -> 
    {ok, map()} | {error, term()}.
report_drone_online_to_mes(DroneId, StationAddr, LineNo, ExtraData) ->
    ?LOG(info, "上报无人机上线状态: drone=~p, station=~p, line=~p", [DroneId, StationAddr, LineNo]),
    
    %% 构建MES数据
    StationIdBin = integer_to_binary(StationAddr),
    StationInfo = case StationAddr of
        1 -> <<"测试线PLC工位"/utf8>>;
        2 -> <<"磁航向工位"/utf8>>;
        3 -> <<"机器人1工位"/utf8>>;
        4 -> <<"机器人2工位"/utf8>>;
        _ -> <<"未知工位"/utf8>>
    end,
    
    MesData = #{
        <<"func_id">> => <<"DRONE_ONLINE">>,
        <<"line_no">> => LineNo,
        <<"line_sta">> => ?LINE_STA_PRODUCTION,
        <<"drone_no">> => DroneId,
        <<"date_time">> => dgiot_uav_mes_utils:get_current_timestamp(),
        <<"data_record">> => #{
            <<"station_id">> => StationIdBin,
            <<"station_info">> => StationInfo,
            <<"drone_status">> => <<"online">>,
            <<"connection_time">> => dgiot_uav_mes_utils:get_current_timestamp(),
            <<"extra_data">> => ExtraData
        }
    },
    
    %% 调用通用发送函数
    send_to_mes(MesData).