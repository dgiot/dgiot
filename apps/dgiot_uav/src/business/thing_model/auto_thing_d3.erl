%%%-------------------------------------------------------------------
%%% @doc
%%% auto_thing_d3 - D3帧字段映射模块
%%% 定义 D3 遥测数据的所有物模型字段映射（包含北斗和卫导拆分后的字段）。
%%% @end
%%%-------------------------------------------------------------------
-module(auto_thing_d3).

-export([field_mappings/0]).

-include_lib("dgiot/include/logger.hrl").

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

-spec field_mappings() -> [#field_map{}].
field_mappings() ->
    BaseList = [
        {<<"ground_speed_direction">>,       <<"地速方向"/utf8>>,          <<"float">>, 0,   360, <<"°"/utf8>>,     0.1},
        {<<"beidou_snr_gt46_count">>,         <<"北斗信噪比>46星数"/utf8>>, <<"int">>,   0,   24,  <<""/utf8>>,      1},
        {<<"beidou_snr_gt44_count">>,         <<"北斗信噪比>44星数"/utf8>>, <<"int">>,   0,   24,  <<""/utf8>>,      1},
        {<<"beidou_snr_gt42_count">>,         <<"北斗信噪比>42星数"/utf8>>, <<"int">>,   0,   24,  <<""/utf8>>,      1},
        {<<"beidou_snr_gt40_count">>,         <<"北斗信噪比>40星数"/utf8>>, <<"int">>,   0,   24,  <<""/utf8>>,      1},
        {<<"beidou_snr_gt38_count">>,         <<"北斗信噪比>38星数"/utf8>>, <<"int">>,   0,   24,  <<""/utf8>>,      1},
        {<<"beidou_snr_gt35_count">>,         <<"北斗信噪比>35星数"/utf8>>, <<"int">>,   0,   24,  <<""/utf8>>,      1},
        {<<"gps_snr_gt46_count">>,            <<"GPS信噪比>46星数"/utf8>>, <<"int">>,   0,   24,  <<""/utf8>>,      1},
        {<<"gps_snr_gt44_count">>,            <<"GPS信噪比>44星数"/utf8>>, <<"int">>,   0,   24,  <<""/utf8>>,      1},
        {<<"gps_snr_gt42_count">>,            <<"GPS信噪比>42星数"/utf8>>, <<"int">>,   0,   24,  <<""/utf8>>,      1},
        {<<"gps_snr_gt40_count">>,            <<"GPS信噪比>40星数"/utf8>>, <<"int">>,   0,   24,  <<""/utf8>>,      1},
        {<<"gps_snr_gt38_count">>,            <<"GPS信噪比>38星数"/utf8>>, <<"int">>,   0,   24,  <<""/utf8>>,      1},
        {<<"gps_snr_gt35_count">>,            <<"GPS信噪比>35星数"/utf8>>, <<"int">>,   0,   24,  <<""/utf8>>,      1},
        {<<"magnetic_error_x">>,               <<"磁强误差X"/utf8>>,       <<"float">>, -1000,1000, <<""/utf8>>,   0.1},
        {<<"magnetic_error_y">>,               <<"磁强误差Y"/utf8>>,       <<"float">>, -1000,1000, <<""/utf8>>,   0.1},
        {<<"magnetic_error_z">>,               <<"磁强误差Z"/utf8>>,       <<"float">>, -1000,1000, <<""/utf8>>,   0.1},
        {<<"magnetic_value_x">>,               <<"磁强值X"/utf8>>,         <<"float">>, -1000,1000, <<""/utf8>>,   0.1},
        {<<"magnetic_value_y">>,               <<"磁强值Y"/utf8>>,         <<"float">>, -1000,1000, <<""/utf8>>,   0.1},
        {<<"magnetic_value_z">>,               <<"磁强值Z"/utf8>>,         <<"float">>, -1000,1000, <<""/utf8>>,   0.1},
        {<<"snr_source">>,                   <<"信噪比源"/utf8>>,         <<"enum">>, 0, 1, <<>>, 1},
        {<<"position_source">>,               <<"位置源"/utf8>>,           <<"enum">>, 0, 1, <<>>, 1},
        {<<"magnetic_type">>,                 <<"磁强类型"/utf8>>,         <<"enum">>, 0, 1, <<>>, 1},
        {<<"beidou_self_destruct_status">>,   <<"北斗自毁状态"/utf8>>,      <<"enum">>,  0,   1,     <<""/utf8>>,      1},
        {<<"beidou_pdop">>,                 <<"北斗PDOP"/utf8>>,          <<"float">>, 0,   50,    <<""/utf8>>,      0.2},
        {<<"gps_pdop">>,                    <<"GPS PDOP"/utf8>>,          <<"float">>, 0,   50,    <<""/utf8>>,      0.2},
        {<<"main_loop_time">>,               <<"主循环时间"/utf8>>,        <<"float">>, 0,   100,   <<"ms"/utf8>>,    0.1},
        {<<"gps_altitude">>,                 <<"GPS高度"/utf8>>,          <<"float">>, -500,5000, <<"m"/utf8>>,     0.1},
        {<<"gps_latitude">>,                 <<"GPS纬度"/utf8>>,          <<"double">>,-90, 90,   <<"°"/utf8>>,     1.0e-7},
        {<<"gps_longitude">>,                <<"GPS经度"/utf8>>,          <<"double">>,-180,180, <<"°"/utf8>>,     1.0e-7},
        {<<"beidou_altitude">>,               <<"北斗高度"/utf8>>,         <<"float">>, -500,5000, <<"m"/utf8>>,     0.1},
        {<<"beidou_latitude">>,               <<"北斗纬度"/utf8>>,         <<"double">>,-90, 90,   <<"°"/utf8>>,     1.0e-7},
        {<<"beidou_longitude">>,              <<"北斗经度"/utf8>>,         <<"double">>,-180,180, <<"°"/utf8>>,     1.0e-7},
        {<<"gps_satellite_count">>,           <<"GPS卫星数"/utf8>>,        <<"int">>,   0,   24,    <<""/utf8>>,      1},
        {<<"beidou_satellite_count">>,        <<"北斗卫星数"/utf8>>,       <<"int">>,   0,   24,    <<""/utf8>>,      1},
        {<<"payload_switch_command">>,        <<"载荷开关指令回报"/utf8>>, <<"int">>,   0,   255,   <<""/utf8>>,      1},
        {<<"warhead_switch_command">>,        <<"引战开关指令回报"/utf8>>, <<"int">>,   0,   255,   <<""/utf8>>,      1},
        {<<"launch_tube_command">>,           <<"发射筒指令回报"/utf8>>,   <<"int">>,   0,   255,   <<""/utf8>>,      1},
        {<<"magnetic_heading">>,             <<"磁航向"/utf8>>,           <<"float">>, 0,   360,   <<"°"/utf8>>,     0.1},
        {<<"magnetic_calibration_status">>,   <<"磁航向校准状态"/utf8>>,   <<"enum">>,  0,   255,   <<""/utf8>>,      1},
        {<<"launch_tube_switch_command">>,     <<"发射筒开关指令"/utf8>>,   <<"int">>,   0,   255,   <<""/utf8>>,      1},
        {<<"seeker_elevation_angle">>,        <<"导引头高低角"/utf8>>,     <<"float">>, -60, 30,    <<"°"/utf8>>,     0.1},
        {<<"seeker_azimuth_angle">>,          <<"导引头方位角"/utf8>>,     <<"float">>, 0,   360,   <<"°"/utf8>>,     0.1},
        {<<"seeker_elevation_rate">>,         <<"导引头高低角速度"/utf8>>, <<"float">>, -300,300,  <<"°/s"/utf8>>,   0.1},
        {<<"seeker_azimuth_rate">>,           <<"导引头方位角速度"/utf8>>, <<"float">>, -300,300,  <<"°/s"/utf8>>,   0.1},
        {<<"line_of_sight_elevation">>,        <<"视线高低角"/utf8>>,       <<"float">>, -90, 90,    <<"°"/utf8>>,     0.1},
        {<<"line_of_sight_azimuth">>,          <<"视线方位角"/utf8>>,       <<"float">>, -180,180,  <<"°"/utf8>>,     0.1},
        {<<"flight_control_temp1">>,          <<"飞控温度1"/utf8>>,        <<"float">>, -40, 85,    <<"°C"/utf8>>,    0.1},
        {<<"flight_control_temp2">>,          <<"飞控温度2"/utf8>>,        <<"float">>, -40, 85,    <<"°C"/utf8>>,    0.1},
        %% 战斗部状态展开
        {<<"warhead_self_destruct">>,          <<"自毁标志"/utf8>>,        <<"enum">>, 0, 1, <<>>, 1},
        {<<"warhead_attack_mode">>,            <<"攻击状态"/utf8>>,        <<"enum">>, 0, 1, <<>>, 1},
        {<<"warhead_capacitor_charged">>,      <<"电容充电"/utf8>>,        <<"enum">>, 0, 1, <<>>, 1},
        {<<"warhead_second_safety_released">>, <<"第二级保险解除"/utf8>>,  <<"enum">>, 0, 1, <<>>, 1},
        {<<"warhead_first_safety_released">>,  <<"第一级保险解除"/utf8>>,  <<"enum">>, 0, 1, <<>>, 1},
        {<<"warhead_self_test_complete">>,     <<"自检完成"/utf8>>,        <<"enum">>, 0, 1, <<>>, 1},
        {<<"warhead_device_normal">>,           <<"设备正常"/utf8>>,       <<"enum">>, 0, 1, <<>>, 1},
        {<<"wing_deployed">>,                  <<"翼展开"/utf8>>,          <<"enum">>, 0, 1, <<>>, 1},
        {<<"isolation_status">>,                <<"隔离状态"/utf8>>,       <<"enum">>, 0, 1, <<>>, 1},
        {<<"detonation_flag">>,                 <<"起爆标志"/utf8>>,       <<"enum">>, 0, 1, <<>>, 1},
        {<<"conductive_membrane_valid">>,       <<"导电膜有效"/utf8>>,     <<"enum">>, 0, 1, <<>>, 1},
        {<<"warhead_acceleration">>,         <<"战斗部加速度"/utf8>>,     <<"int">>,   0,   255,   <<""/utf8>>,      1},
        {<<"laser_range_value">>,             <<"激光测距值"/utf8>>,       <<"float">>, 0,   1000,  <<"m"/utf8>>,     0.2},
        {<<"touch_detonation_voltage">>,      <<"触炸导电膜电平"/utf8>>,   <<"float">>, 0,   30,    <<"V"/utf8>>,     0.1},
        {<<"launch_tube_status">>,            <<"发射筒状态字"/utf8>>,     <<"int">>,   0,   255,   <<""/utf8>>,      1},
        {<<"launch_tube_ignition_voltage">>,   <<"发射筒点火电压"/utf8>>,   <<"int">>,   0,   30,    <<"V"/utf8>>,      1},
        {<<"warhead_voltage">>,              <<"引战电压"/utf8>>,         <<"float">>, 0,   30,    <<"V"/utf8>>,     0.1},
        {<<"payload_voltage">>,              <<"载荷电压"/utf8>>,         <<"float">>, 0,   30,    <<"V"/utf8>>,     0.1},
        {<<"night_flight_voltage">>,          <<"夜航电压"/utf8>>,         <<"float">>, 0,   30,    <<"V"/utf8>>,     0.1},
        {<<"power_5v2">>,                    <<"5.2V电源"/utf8>>,         <<"float">>, 0,   6,     <<"V"/utf8>>,     0.1},
        {<<"power_5v0">>,                    <<"5.0V电源"/utf8>>,         <<"float">>, 0,   6,     <<"V"/utf8>>,     0.1},
        {<<"power_8v4_1">>,                  <<"8.4V电源1"/utf8>>,       <<"float">>, 0,   10,    <<"V"/utf8>>,     0.1},
        {<<"power_8v4_2">>,                  <<"8.4V电源2"/utf8>>,       <<"float">>, 0,   10,    <<"V"/utf8>>,     0.1},
        {<<"hard_switch_voltage">>,            <<"硬开关电压"/utf8>>,      <<"float">>, 0,   30,    <<"V"/utf8>>,     0.1},
        %% 开关状态展开
        {<<"soft_switch1">>,                   <<"软开关1"/utf8>>,        <<"enum">>, 0, 1, <<>>, 1},
        {<<"soft_switch2">>,                   <<"软开关2"/utf8>>,        <<"enum">>, 0, 1, <<>>, 1},
        {<<"left_wing_switch">>,                <<"左机翼开关"/utf8>>,    <<"enum">>, 0, 1, <<>>, 1},
        {<<"right_wing_switch">>,               <<"右机翼开关"/utf8>>,    <<"enum">>, 0, 1, <<>>, 1},
        {<<"hard_switch_measure">>,             <<"硬开关测量"/utf8>>,    <<"enum">>, 0, 1, <<>>, 1},
        {<<"fuze_charging_voltage">>,          <<"引信充电电压"/utf8>>,    <<"int">>,   0,   255,   <<""/utf8>>,      1},
        {<<"guidance_stabilization_coef">>,    <<"制导镇定系数"/utf8>>,    <<"int">>,   0,   255,   <<""/utf8>>,      1},
        {<<"wind_speed1">>,                   <<"风速1"/utf8>>,           <<"float">>, 0,   50,    <<"m/s"/utf8>>,   0.2},
        {<<"wind_direction1">>,               <<"风向1"/utf8>>,           <<"float">>, 0,   360,   <<"°"/utf8>>,     0.1},
        {<<"wind_speed2">>,                   <<"风速2"/utf8>>,           <<"float">>, 0,   50,    <<"m/s"/utf8>>,   0.2},
        {<<"wind_direction2">>,               <<"风向2"/utf8>>,           <<"float">>, 0,   360,   <<"°"/utf8>>,     0.1},
        {<<"payload_electronic_zoom">>,        <<"载荷电子变倍"/utf8>>,    <<"int">>,   0,   5,     <<""/utf8>>,      1},
        {<<"softened_payload_tracking_flag">>,  <<"软化载荷跟踪标志"/utf8>>,<<"float">>, 0,   100,   <<""/utf8>>,      0.01},
        {<<"payload_tracking_flag">>,           <<"载荷跟踪标志"/utf8>>,    <<"int">>,   0,   1,     <<""/utf8>>,      1},
        {<<"drone_type">>,                     <<"无人机类型"/utf8>>,      <<"enum">>,  0,   255,   <<""/utf8>>,      1},
        {<<"sight_azimuth_heading_deviation">>, <<"视线方位-航向偏差"/utf8>>, <<"int">>, -180,180, <<"°"/utf8>>,      1}
    ],
    [field_map_from_tuple(T, <<"D3">>) || T <- BaseList].

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