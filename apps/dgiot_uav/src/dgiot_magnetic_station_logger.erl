%%--------------------------------------------------------------------
%% @doc
%% dgiot_magnetic_station_logger - 磁航向工位专用日志模块
%% 提供详细的日志记录功能，便于调试和问题排查
%% @end
%%--------------------------------------------------------------------
-module(dgiot_magnetic_station_logger).

%% API
-export([
    log_stage_start/2,
    log_stage_complete/3,
    log_step_start/2,
    log_step_complete/3,
    log_plc_request/3,
    log_plc_response/3,
    log_eb90_command/3,
    log_telemetry_data/3,
    log_binding_event/3,
    log_error/3,
    log_summary/2
]).

-include_lib("dgiot/include/logger.hrl").
-include("dgiot_uav.hrl").

%% 日志前缀
-define(LOG_PREFIX, "【磁航向工位】").
-define(SEP, "========================================").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 记录测试阶段开始
-spec log_stage_start(binary(), binary()) -> ok.
log_stage_start(StageName, Description) ->
    ?LOG(info, "~s ~n【阶段开始】~n  阶段名称: ~s~n  阶段描述: ~s~n~s",
        [?SEP, StageName, Description, ?SEP]).

%% @doc 记录测试阶段完成
-spec log_stage_complete(binary(), map(), term()) -> ok.
log_stage_complete(StageName, Stats, Status) ->
    ?LOG(info, "~s ~n【阶段完成】~n  阶段名称: ~s~n  状态: ~p~n  统计: ~p~n~s",
        [?SEP, StageName, Status, Stats, ?SEP]).

%% @doc 记录测试步骤开始
-spec log_step_start(binary(), binary()) -> ok.
log_step_start(StepName, StepDesc) ->
    ?LOG(info, "~s ~n【步骤开始】~n  步骤名称: ~s~n  步骤描述: ~s~n~s",
        [?SEP, StepName, StepDesc, ?SEP]).

%% @doc 记录测试步骤完成
-spec log_step_complete(binary(), map(), term()) -> ok.
log_step_complete(StepName, Result, Status) ->
    ?LOG(info, "~s ~n【步骤完成】~n  步骤名称: ~s~n  状态: ~p~n  结果: ~p~n~s",
        [?SEP, StepName, Status, Result, ?SEP]).

%% @doc 记录PLC请求
-spec log_plc_request(binary(), binary(), term()) -> ok.
log_plc_request(StationAddr, FunctionCode, RequestData) ->
    ?LOG(info, "~s PLC请求~n  工位地址: ~s~n  功能码: ~s~n  请求数据: ~p~n~s",
        [?LOG_PREFIX, StationAddr, FunctionCode, RequestData, ?SEP]).

%% @doc 记录PLC响应
-spec log_plc_response(binary(), binary(), term()) -> ok.
log_plc_response(StationAddr, FunctionCode, ResponseData) ->
    ?LOG(info, "~s PLC响应~n  工位地址: ~s~n  功能码: ~s~n  响应数据: ~p~n~s",
        [?LOG_PREFIX, StationAddr, FunctionCode, ResponseData, ?SEP]).

%% @doc 记录EB90指令
-spec log_eb90_command(binary(), binary(), binary()) -> ok.
log_eb90_command(CommandName, CommandType, CommandData) ->
    ?LOG(info, "~s EB90指令下发~n  指令名称: ~s~n  指令类型: ~s~n  指令数据: ~s~n~s",
        [?LOG_PREFIX, CommandName, CommandType, dgiot_utils:binary_to_hex(CommandData), ?SEP]).

%% @doc 记录遥测数据
-spec log_telemetry_data(binary(), integer(), term()) -> ok.
log_telemetry_data(DataType, Sequence, TelemetryData) ->
    ?LOG(info, "~s 遥测数据发送~n  数据类型: ~s~n  序列号: ~p~n  数据内容: ~p~n~s",
        [?LOG_PREFIX, DataType, Sequence, TelemetryData, ?SEP]).

%% @doc 记录绑定事件
-spec log_binding_event(binary(), binary(), map()) -> ok.
log_binding_event(EventType, DroneId, BindingData) ->
    ?LOG(info, "~s 绑定事件~n  事件类型: ~s~n  无人机ID: ~s~n  绑定数据: ~p~n~s",
        [?LOG_PREFIX, EventType, DroneId, BindingData, ?SEP]).

%% @doc 记录错误
-spec log_error(binary(), binary(), term()) -> ok.
log_error(ErrorType, ErrorContext, ErrorReason) ->
    ?LOG(error, "~s 错误信息~n  错误类型: ~s~n  错误上下文: ~s~n  错误原因: ~p~n~s",
        [?LOG_PREFIX, ErrorType, ErrorContext, ErrorReason, ?SEP]).

%% @doc 记录测试总结
-spec log_summary(map(), map()) -> ok.
log_summary(TestStats, ResultStats) ->
    ?LOG(info, "~s 测试总结~n  测试统计: ~p~n  结果统计: ~p~n~s",
        [?SEP, TestStats, ResultStats, ?SEP]).

%%%===================================================================
%%% 内部函数
%%%===================================================================
