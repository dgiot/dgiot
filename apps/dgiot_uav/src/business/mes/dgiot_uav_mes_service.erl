%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_mes_service - 无人机MES服务转发模块
%%% 
%%% 提供统一的MES上报接口，实际调用 dgiot_uav_mes_api 模块。
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_mes_service).

-include_lib("dgiot/include/logger.hrl").

-export([
    report_device_status/4
]).

%% @doc 上报设备状态
-spec report_device_status(binary(), binary(), binary(), map()) -> 
    {ok, map()} | {error, term()}.
report_device_status(FuncId, LineNo, DroneNo, ExtraData) ->
    ?LOG(info, "MES服务转发设备状态上报: FuncId=~p, LineNo=~p, DroneNo=~p", 
         [FuncId, LineNo, DroneNo]),
    dgiot_uav_mes_api:report_device_status(FuncId, LineNo, DroneNo, ExtraData).