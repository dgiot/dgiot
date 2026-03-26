%%%-------------------------------------------------------------------
%%% @doc
%%% uav_thing_model_extra - 额外命令物模型转换模块
%%% 包含以下数据的物模型转换：
%%% - 版本信息 (0xC1)
%%% - 航点信息 (0xB4)
%%% - 舵面校准 (0xDE)
%%% - 电池状态 (0xA1)
%%% - 链路状态 (新增)
%%% @end
%%%-------------------------------------------------------------------
-module(uav_thing_model_extra).

-export([
    convert_version/1,
    convert_waypoint/1,
    convert_surface_calibration/1,
    convert_battery/1,
    convert_link/1  % 新增
]).

-include_lib("dgiot_uav/include/extra_commands.hrl").
-include_lib("dgiot/include/logger.hrl").

%%%===================================================================
%%% 版本号转换
%%%===================================================================

-spec convert_version(#version_info{}) -> map().
convert_version(#version_info{
    frame_length = _FrameLen,
    drone_model = _DroneModel,
    drone_id = _DroneId,
    version_string = VersionStr,
    crc = _Crc
}) ->
    #{
        <<"version_string">> => VersionStr,
        <<"timestamp">> => erlang:system_time(millisecond)
    }.

%%%===================================================================
%%% 航点信息转换
%%%===================================================================

-spec convert_waypoint(#waypoint_info{}) -> map().
convert_waypoint(#waypoint_info{
    latitude = Lat,
    longitude = Lon,
    altitude = Alt,
    total_count = Total,
    waypoint_index = Idx,
    crc = _Crc
}) ->
    #{
        <<"waypoint_latitude">> => Lat,
        <<"waypoint_longitude">> => Lon,
        <<"waypoint_altitude">> => Alt,
        <<"waypoint_total_count">> => Total,
        <<"waypoint_index">> => Idx,
        <<"timestamp">> => erlang:system_time(millisecond)
    }.

%%%===================================================================
%%% 舵面校准转换
%%%===================================================================

-spec convert_surface_calibration(#surface_calibration{}) -> map().
convert_surface_calibration(#surface_calibration{
    channel = Ch,
    pwm_center = Pwm,
    up_ratio = Up,
    down_ratio = Down,
    crc = _Crc
}) ->
    #{
        <<"surface_channel">> => Ch,
        <<"surface_pwm_center">> => Pwm,
        <<"surface_up_ratio">> => Up,
        <<"surface_down_ratio">> => Down,
        <<"timestamp">> => erlang:system_time(millisecond)
    }.

%%%===================================================================
%%% 电池状态转换
%%%===================================================================

-spec convert_battery(#battery_status{}) -> map().
convert_battery(#battery_status{
    status_byte = StatusByte,
    voltage = Voltage,
    activate_state = ActivateState,
    temperature1 = Temp1,
    temp2_or_count = Temp2OrCount,
    sequence = Seq,
    cmd_result = Result
}) ->
    #{
        <<"battery_status_byte">> => StatusByte,
        <<"battery_voltage">> => Voltage,
        <<"battery_activate_state">> => ActivateState,
        <<"battery_temp1">> => Temp1,
        <<"battery_temp2">> => Temp2OrCount,
        <<"battery_sequence">> => Seq,
        <<"battery_cmd_result">> => Result,
        <<"timestamp">> => erlang:system_time(millisecond)
    }.

%%%===================================================================
%%% 链路状态转换
%%%===================================================================

-spec convert_link(#link_status{}) -> map().
convert_link(#link_status{
    network_access_flag = AccessFlag,
    node_address = NodeAddr,
    granted_nodes_count = Granted,
    denied_nodes_count = Denied,
    online_nodes = Online,
    latest_denied_node = LatestDenied,
    crc1 = Crc1,
    crc2 = Crc2
}) ->
    #{
        <<"link_access_flag">> => AccessFlag,
        <<"link_node_address">> => NodeAddr,
        <<"link_granted_nodes">> => Granted,
        <<"link_denied_nodes">> => Denied,
        <<"link_online_nodes">> => list_to_binary(lists:join(<<",">>, [integer_to_binary(Addr) || Addr <- Online])),
        <<"link_latest_denied">> => LatestDenied,
        <<"link_crc1">> => Crc1,
        <<"link_crc2">> => Crc2,
        <<"timestamp">> => erlang:system_time(millisecond)
    }.