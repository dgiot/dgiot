%%%-------------------------------------------------------------------
%%% @doc
%%% auto_thing_d1 - D1帧字段映射模块
%%% 定义 D1 遥测数据的所有物模型字段映射。
%%% @end
%%%-------------------------------------------------------------------
-module(auto_thing_d1).

-export([field_mappings/0]).

-include_lib("dgiot/include/logger.hrl").

%% 字段映射记录（与主模块一致）
-record(field_map, {
    identifier :: binary(),
    name :: binary(),
    type :: binary(),
    min :: number(),
    max :: number(),
    unit :: binary(),
    step :: number(),
    group :: binary()
}).

%% @doc 返回 D1 字段映射列表
-spec field_mappings() -> [#field_map{}].
field_mappings() ->
    BaseList = [
        {<<"createdat">>,                <<"时间戳"/utf8>>,          <<"date">>,   0,   4294967295, <<"ms"/utf8>>, 1},
        {<<"latitude">>,                 <<"纬度"/utf8>>,          <<"double">>, -90, 90,   <<"°"/utf8>>,     1.0e-7},
        {<<"longitude">>,                <<"经度"/utf8>>,          <<"double">>, -180,180, <<"°"/utf8>>,     1.0e-7},
        {<<"heading">>,                  <<"航向角"/utf8>>,        <<"float">>,  0,   360,  <<"°"/utf8>>,     0.1},
        {<<"pitch">>,                    <<"俯仰角"/utf8>>,        <<"float">>,  -90, 90,   <<"°"/utf8>>,     0.01},
        {<<"roll">>,                     <<"横滚角"/utf8>>,        <<"float">>,  -90, 90,   <<"°"/utf8>>,     0.01},
        {<<"relative_altitude">>,         <<"相对高度"/utf8>>,      <<"float">>,  0,   5000, <<"m"/utf8>>,     0.1},
        {<<"airspeed">>,                 <<"空速"/utf8>>,          <<"float">>,  0,   100,  <<"m/s"/utf8>>,   0.01},
        {<<"elevator_angle">>,            <<"升降舵角"/utf8>>,      <<"float">>,  -90, 90,   <<"°"/utf8>>,     0.01},
        {<<"rudder_angle">>,              <<"方向舵角"/utf8>>,      <<"float">>,  -90, 90,   <<"°"/utf8>>,     0.01},
        {<<"aileron_angle">>,             <<"副翼舵角"/utf8>>,      <<"float">>,  -90, 90,   <<"°"/utf8>>,     0.01},
        {<<"throttle_angle">>,            <<"油门舵角"/utf8>>,      <<"float">>,  0,   100,  <<"°"/utf8>>,     0.01},
        {<<"gps_altitude">>,              <<"卫导高度"/utf8>>,      <<"float">>,  -500,5000,<<"m"/utf8>>,     0.2},
        {<<"baro_altitude">>,             <<"气压高度"/utf8>>,      <<"float">>,  -500,5000,<<"m"/utf8>>,     0.2},
        {<<"east_velocity">>,             <<"东向速度"/utf8>>,      <<"float">>,  -50, 50,   <<"m/s"/utf8>>,   0.01},
        {<<"north_velocity">>,            <<"北向速度"/utf8>>,      <<"float">>,  -50, 50,   <<"m/s"/utf8>>,   0.01},
        {<<"flight_time">>,               <<"飞行时间"/utf8>>,      <<"int">>,    0,   86400,<<"s"/utf8>>,     1},
        {<<"battery_heating_flag">>,       <<"电池加热标志"/utf8>>,  <<"enum">>,   0,   1,     <<""/utf8>>,      1},
        {<<"reset_type">>,                <<"复位类型"/utf8>>,      <<"enum">>,   0,   1,     <<""/utf8>>,      1},
        {<<"reset_count">>,               <<"复位次数"/utf8>>,      <<"int">>,    0,   255,   <<""/utf8>>,      1},
        {<<"detonation_power_status">>,    <<"起爆供电状态"/utf8>>,  <<"enum">>,   0,   1,     <<""/utf8>>,      1},
        {<<"target_heading">>,            <<"目标航向"/utf8>>,      <<"float">>,  0,   360,   <<"°"/utf8>>,     0.1},
        {<<"target_waypoint">>,           <<"目标航点"/utf8>>,      <<"int">>,    0,   255,   <<""/utf8>>,      1},
        {<<"battery_voltage">>,           <<"电池电压"/utf8>>,      <<"float">>,  0,   30,    <<"V"/utf8>>,     0.2},
        {<<"climb_rate">>,                <<"爬升率"/utf8>>,        <<"float">>,  -50, 50,    <<"m/s"/utf8>>,   0.01},
        {<<"data_binding_executed">>,      <<"已执行数据装订"/utf8>>,<<"enum">>,   0,   255,   <<""/utf8>>,      1},
        {<<"flight_mode">>,               <<"飞行模式"/utf8>>,      <<"enum">>,   0,   255,   <<""/utf8>>,      1},
        {<<"gps_horizontal_accuracy">>,    <<"卫导水平定位精度"/utf8>>, <<"float">>,0,10, <<"m"/utf8>>,      0.1},
        {<<"flight_control_switch_command">>, <<"飞控开关指令回报"/utf8>>, <<"int">>, 0,255, <<""/utf8>>, 1},
        {<<"gps_satellite_count">>,        <<"卫导可用星数"/utf8>>, <<"int">>,    0,   24,    <<""/utf8>>,      1},
        %% 故障状态展开
        {<<"fault_remote_link">>,          <<"遥控链路故障"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"fault_gps_position">>,         <<"卫导定位故障"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"fault_imu_position">>,         <<"IMU定位故障"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"fault_airspeed">>,             <<"空速故障"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"fault_baro_altitude">>,        <<"气压高度故障"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"fault_payload">>,              <<"载荷故障"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"fault_beidou">>,                <<"北斗故障"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"fault_rotation_speed">>,       <<"转速故障"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"fault_ferroelectric">>,         <<"铁电故障"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"fault_battery_voltage">>,       <<"电池电压故障"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"fault_warhead">>,               <<"引战故障"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"fault_launch_tube">>,           <<"发射筒故障"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        %% 警告标识展开
        {<<"warning_recovery_alert">>,      <<"进入回收警示"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"warning_emergency_data_unbound">>, <<"应急数据未装订"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"warning_ground_station_pos_unbound">>, <<"地面站位置未装订"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"warning_cruise_route_unbound">>, <<"巡飞航线未装订"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"warning_link_failure_mode_unbound">>, <<"链路失效模式未装订"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"warning_et_route_unbound">>,    <<"ET航线未装订"/utf8>>, <<"enum">>, 0, 1, <<>>, 1},
        {<<"ferroelectric_fault">>,         <<"铁电故障原始值"/utf8>>, <<"int">>, 0, 65535, <<>>, 1},
        {<<"attack_mode">>,               <<"攻击模式"/utf8>>,      <<"enum">>,   0,   255,   <<""/utf8>>,      1},
        {<<"soft_command_overload">>,      <<"软化指令过载"/utf8>>,  <<"float">>,  0,   10,    <<"m/s²"/utf8>>,  0.2},
        {<<"soft_command_path_inclination">>, <<"软化指令航迹倾角"/utf8>>, <<"float">>, -90,90, <<"°"/utf8>>, 0.5},
        {<<"path_inclination">>,          <<"航迹倾角"/utf8>>,      <<"float">>,  -90,90,   <<"°"/utf8>>,     0.5},
        {<<"soft_command_path_deflection">>, <<"软化指令航迹偏角"/utf8>>, <<"float">>, -180,180, <<"°"/utf8>>, 0.02},
        {<<"combined_north_velocity">>,    <<"组合北向速度"/utf8>>,  <<"float">>,  -50,50,   <<"m/s"/utf8>>,   0.01},
        {<<"combined_east_velocity">>,     <<"组合东向速度"/utf8>>,  <<"float">>,  -50,50,   <<"m/s"/utf8>>,   0.01},
        {<<"combined_vertical_velocity">>, <<"组合天向速度"/utf8>>,  <<"float">>,  -50,50,   <<"m/s"/utf8>>,   0.01},
        {<<"gps_position_flag">>,          <<"卫导定位标志"/utf8>>,  <<"int">>,    0,   255,   <<""/utf8>>,      1},
        {<<"longitudinal_miss_distance">>, <<"纵向脱靶量"/utf8>>,    <<"float">>,  -1000,1000, <<""/utf8>>,    0.1},
        {<<"lateral_miss_distance">>,      <<"横向脱靶量"/utf8>>,    <<"float">>,  -1000,1000, <<""/utf8>>,    0.1},
        %% ===== 新增 D1 电池字段（对应字节124-126）=====
        {<<"battery_status">>,             <<"电池状态"/utf8>>,      <<"int">>,    0,   255,   <<>>,           1},
        {<<"battery_temp1">>,              <<"电池温度1"/utf8>>,     <<"int">>,   -128, 127,   <<"℃"/utf8>>,    1},
        {<<"battery_temp2">>,              <<"电池温度2"/utf8>>,     <<"int">>,   -128, 127,   <<"℃"/utf8>>,    1}
    ],
    [field_map_from_tuple(T, <<"D1">>) || T <- BaseList].

%% 内部辅助函数
field_map_from_tuple({Id, Name, Type, Min, Max, Unit, Step}, Group) ->
    #field_map{
        identifier = Id,
        name = Name,
        type = Type,
        min = Min,
        max = Max,
        unit = Unit,
        step = Step,
        group = Group
    }.