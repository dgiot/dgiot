%%%-------------------------------------------------------------------
%%% @doc
%%% auto_thing_d2 - D2帧字段映射模块
%%% 定义 D2 遥测数据的所有物模型字段映射。
%%% 修复：添加缺失的 warhead_frame_freq 字段。
%%% @end
%%%-------------------------------------------------------------------
-module(auto_thing_d2).

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
        %% 引战帧频（新增）
        {<<"warhead_frame_freq">>,           <<"引战帧频"/utf8>>,         <<"int">>,   0,   255,  <<"Hz"/utf8>>,    1},
        {<<"roll_angle_given">>,           <<"横滚角给定"/utf8>>,       <<"float">>, -180,180, <<"°"/utf8>>,     0.1},
        {<<"pitch_angle_given">>,          <<"俯仰角给定"/utf8>>,       <<"float">>, -90, 90,  <<"°"/utf8>>,     0.1},
        {<<"airspeed_given">>,            <<"空速给定"/utf8>>,         <<"int">>,   0,   100, <<"m/s"/utf8>>,   1},
        {<<"onboard_nav_status">>,         <<"板载导航状态"/utf8>>,     <<"int">>,   0,   255, <<""/utf8>>,      1},
        {<<"origin_distance">>,           <<"原点距离"/utf8>>,         <<"int">>,   0,   10000,<<"m"/utf8>>,    1},
        {<<"origin_azimuth">>,            <<"原点方位"/utf8>>,         <<"float">>, 0,   360, <<"°"/utf8>>,     0.1},
        {<<"distance_to_go">>,             <<"待飞距"/utf8>>,           <<"int">>,   0,   10000,<<"m"/utf8>>,    1},
        {<<"lateral_deviation">>,         <<"侧偏距"/utf8>>,           <<"float">>, -100,100,<<"m"/utf8>>,     0.1},
        {<<"command_altitude">>,          <<"指令高度"/utf8>>,         <<"float">>, -500,5000,<<"m"/utf8>>,    0.1},
        {<<"rotation_speed">>,            <<"转速"/utf8>>,             <<"int">>,   0,   10000,<<"RPM"/utf8>>,  1},
        {<<"total_flight_time">>,          <<"飞行总时间"/utf8>>,       <<"int">>,   0,   10000,<<"min"/utf8>>,  1},
        {<<"flight_sortie">>,             <<"飞行架次"/utf8>>,         <<"int">>,   0,   255,  <<""/utf8>>,      1},
        {<<"remote_frame_freq">>,          <<"遥控帧频"/utf8>>,         <<"int">>,   0,   255,  <<"Hz"/utf8>>,    1},
        {<<"gps_frame_freq">>,             <<"卫导帧频"/utf8>>,         <<"int">>,   0,   255,  <<"Hz"/utf8>>,    1},
        {<<"beidou_frame_freq">>,          <<"北斗帧频"/utf8>>,         <<"int">>,   0,   255,  <<"Hz"/utf8>>,    1},
        {<<"payload_frame_freq">>,         <<"载荷帧频"/utf8>>,         <<"int">>,   0,   255,  <<"Hz"/utf8>>,    1},
        {<<"launch_tube_frame_freq">>,      <<"发射筒帧频"/utf8>>,       <<"int">>,   0,   255,  <<"Hz"/utf8>>,    1},
        {<<"magnetic_heading_frame_freq">>, <<"磁航向帧频"/utf8>>,       <<"int">>,   0,   255,  <<"Hz"/utf8>>,    1},
        {<<"gps_relative_altitude">>,      <<"卫导相对高度"/utf8>>,     <<"float">>, -500,5000,<<"m"/utf8>>,     0.1},
        {<<"baro_relative_altitude">>,     <<"气压相对高度"/utf8>>,     <<"float">>, -500,5000,<<"m"/utf8>>,     0.1},
        %% 飞行模态展开
        {<<"throttle_mode">>,              <<"油门模态"/utf8>>,        <<"enum">>, 0, 7, <<>>, 1},
        {<<"longitudinal_mode">>,          <<"纵向模态"/utf8>>,        <<"enum">>, 0, 7, <<>>, 1},
        {<<"lateral_mode">>,               <<"横向模态"/utf8>>,        <<"enum">>, 0, 7, <<>>, 1},
        {<<"in_air">>,                     <<"飞机在空中"/utf8>>,      <<"enum">>, 0, 1, <<>>, 1},
        {<<"circle_mode">>,                <<"盘旋模式"/utf8>>,        <<"enum">>, 0, 1, <<>>, 1},
        {<<"navigation_mode">>,             <<"导航模态"/utf8>>,        <<"enum">>, 0, 15, <<>>, 1},
        %% 北斗状态展开
        {<<"beidou_code_type">>,            <<"北斗军民码类型"/utf8>>,  <<"enum">>, 0, 3, <<>>, 1},
        {<<"beidou_position_valid">>,       <<"北斗定位有效"/utf8>>,    <<"enum">>, 0, 1, <<>>, 1},
        {<<"launch_flag">>,               <<"发射标志"/utf8>>,         <<"enum">>,  0,   255,  <<""/utf8>>,      1},
        {<<"pull_up_height">>,             <<"拉起高度"/utf8>>,         <<"int">>,   0,   255,  <<"m"/utf8>>,      1},
        {<<"angular_rate_x">>,             <<"角速率X"/utf8>>,          <<"float">>, -500,500, <<"°/s"/utf8>>,   0.01},
        {<<"angular_rate_y">>,             <<"角速率Y"/utf8>>,          <<"float">>, -500,500, <<"°/s"/utf8>>,   0.01},
        {<<"angular_rate_z">>,             <<"角速率Z"/utf8>>,          <<"float">>, -500,500, <<"°/s"/utf8>>,   0.01},
        {<<"acceleration_x">>,             <<"加速度X"/utf8>>,         <<"float">>, -50, 50,  <<"m/s²"/utf8>>,  0.01},
        {<<"acceleration_y">>,             <<"加速度Y"/utf8>>,         <<"float">>, -50, 50,  <<"m/s²"/utf8>>,  0.01},
        {<<"acceleration_z">>,             <<"加速度Z"/utf8>>,         <<"float">>, -50, 50,  <<"m/s²"/utf8>>,  0.01},
        {<<"pitch_calibration">>,          <<"俯仰角校准值"/utf8>>,    <<"float">>, -10, 10,   <<"°"/utf8>>,     0.1},
        {<<"roll_calibration">>,           <<"横滚角校准值"/utf8>>,    <<"float">>, -10, 10,   <<"°"/utf8>>,     0.1},
        {<<"heading_calibration">>,        <<"航向角校准值"/utf8>>,    <<"float">>, -10, 10,   <<"°"/utf8>>,     0.1},
        {<<"elevator_calibration">>,       <<"升降舵校准值"/utf8>>,    <<"float">>, -10, 10,   <<"°"/utf8>>,     0.1},
        {<<"aileron_calibration">>,        <<"副翼校准值"/utf8>>,      <<"float">>, -10, 10,   <<"°"/utf8>>,     0.1},
        {<<"rudder_calibration">>,         <<"方向舵校准值"/utf8>>,    <<"float">>, -10, 10,   <<"°"/utf8>>,     0.1},
        {<<"airspeed_calibration_coef">>,   <<"空速校准系数"/utf8>>,    <<"float">>, 0.8,1.2,  <<""/utf8>>,      0.001},
        {<<"airspeed_calibration_offset">>, <<"空速校准偏移量"/utf8>>,  <<"float">>, -200,1000,<<""/utf8>>,      0.1},
        {<<"airspeed_zero_offset">>,        <<"空速零偏"/utf8>>,        <<"float">>, -10, 10,   <<""/utf8>>,      0.1},
        {<<"pitch_integral">>,             <<"俯仰角积分"/utf8>>,      <<"float">>, -100,100, <<""/utf8>>,      0.1},
        {<<"altitude_integral">>,          <<"高度积分"/utf8>>,        <<"float">>, -100,100, <<""/utf8>>,      0.1},
        {<<"glide_altitude_integral">>,     <<"下滑段高度积分"/utf8>>,  <<"float">>, -100,100, <<""/utf8>>,      0.1},
        {<<"airspeed_to_throttle_integral">>,<<"空速到油门积分"/utf8>>, <<"float">>, -10, 10,   <<""/utf8>>,      0.01},
        {<<"roll_integral">>,              <<"横滚角积分"/utf8>>,      <<"float">>, -100,100, <<""/utf8>>,      0.1},
        {<<"acceleration_integral">>,      <<"加速度积分"/utf8>>,      <<"float">>, -100,100, <<""/utf8>>,      0.1},
        {<<"airspeed_to_pitch_integral">>,   <<"空速到俯仰角积分"/utf8>>,<<"float">>, -100,100, <<""/utf8>>,      0.1},
        {<<"lateral_deviation_correction">>, <<"侧偏距修正"/utf8>>,     <<"int">>,   -100,100, <<""/utf8>>,      1},
        %% 载荷状态展开
        {<<"payload_type">>,                <<"载荷类型"/utf8>>,       <<"enum">>, 0, 7, <<>>, 1},
        {<<"payload_compression_mode">>,    <<"压缩模式"/utf8>>,       <<"enum">>, 0, 3, <<>>, 1},
        {<<"payload_image_stabilization">>, <<"稳像状态"/utf8>>,       <<"enum">>, 0, 1, <<>>, 1},
        {<<"payload_work_state">>,          <<"载荷工作状态"/utf8>>,   <<"enum">>, 0, 3, <<>>, 1},
        {<<"ir_zoom">>,                     <<"红外变倍"/utf8>>,       <<"enum">>, 0, 3, <<>>, 1},
        {<<"vis_zoom">>,                    <<"可见光变倍"/utf8>>,     <<"enum">>, 0, 7, <<>>, 1},
        {<<"image_enhance">>,               <<"图像增强"/utf8>>,       <<"enum">>, 0, 3, <<>>, 1},
        {<<"payload_protect_state">>,        <<"载荷保护状态"/utf8>>,  <<"enum">>, 0, 1, <<>>, 1},
        {<<"payload_target_relative_height">>,<<"载荷目标相对高"/utf8>>,<<"int">>,    0,   10000,<<"m"/utf8>>,    1},
        {<<"target_relative_height_flag">>,  <<"目标相对高标志"/utf8>>,  <<"enum">>,  0,   1,     <<""/utf8>>,      1},
        {<<"control_surface_status">>,      <<"舵面动作状态"/utf8>>,    <<"enum">>,  0,   3,     <<""/utf8>>,      1}
    ],
    [field_map_from_tuple(T, <<"D2">>) || T <- BaseList].

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