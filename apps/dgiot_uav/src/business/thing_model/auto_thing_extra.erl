%%%-------------------------------------------------------------------
%%% @doc
%%% auto_thing_extra - 额外命令字段映射模块
%%% 包含版本、航点、舵面校准、电池应答、测试项及链路状态字段映射。
%%% 修改：测试项保留 TEST_ITEM 组，链路独立为 LINK 组，其余合并为 AUXILIARY 组。
%%% @end
%%%-------------------------------------------------------------------
-module(auto_thing_extra).

-export([
    version_field_mappings/0,
    waypoint_field_mappings/0,
    surface_calibration_field_mappings/0,
    battery_field_mappings/0,
    test_item_field_mappings/0,
    link_field_mappings/0
]).

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

%% 版本字段映射
-spec version_field_mappings() -> [#field_map{}].
version_field_mappings() ->
    BaseList = [
        {<<"version_string">>, <<"飞控软件版本"/utf8>>, <<"text">>, 0, 0, <<>>, 1}
    ],
    [field_map_from_tuple(T, <<"AUXILIARY">>) || T <- BaseList].

%% 航点字段映射
-spec waypoint_field_mappings() -> [#field_map{}].
waypoint_field_mappings() ->
    BaseList = [
        {<<"waypoint_latitude">>, <<"航点纬度"/utf8>>, <<"double">>, -90, 90, <<"°"/utf8>>, 1.0e-7},
        {<<"waypoint_longitude">>, <<"航点经度"/utf8>>, <<"double">>, -180, 180, <<"°"/utf8>>, 1.0e-7},
        {<<"waypoint_altitude">>, <<"航点高度"/utf8>>, <<"int">>, -1000, 3000, <<"m"/utf8>>, 1},
        {<<"waypoint_total_count">>, <<"航点总数"/utf8>>, <<"int">>, 6, 30, <<>>, 1},
        {<<"waypoint_index">>, <<"航点序号"/utf8>>, <<"int">>, 1, 30, <<>>, 1}
    ],
    [field_map_from_tuple(T, <<"AUXILIARY">>) || T <- BaseList].

%% 舵面校准字段映射
-spec surface_calibration_field_mappings() -> [#field_map{}].
surface_calibration_field_mappings() ->
    BaseList = [
        {<<"surface_channel">>, <<"舵面通道"/utf8>>, <<"enum">>, 1, 4, <<>>, 1},
        {<<"surface_pwm_center">>, <<"PWM中位"/utf8>>, <<"float">>, -10, 10, <<"ms"/utf8>>, 0.001},
        {<<"surface_up_ratio">>, <<"上偏比例"/utf8>>, <<"float">>, 0.5, 2.0, <<>>, 0.001},
        {<<"surface_down_ratio">>, <<"下偏比例"/utf8>>, <<"float">>, 0.5, 2.0, <<>>, 0.001}
    ],
    [field_map_from_tuple(T, <<"AUXILIARY">>) || T <- BaseList].

%% 电池字段映射
-spec battery_field_mappings() -> [#field_map{}].
battery_field_mappings() ->
    BaseList = [
        {<<"battery_status_byte">>, <<"电池状态字节"/utf8>>, <<"int">>, 0, 255, <<>>, 1},
        {<<"battery_voltage">>, <<"电池电压"/utf8>>, <<"float">>, 0, 40, <<"V"/utf8>>, 0.01},
        {<<"battery_activate_state">>, <<"电池激活状态"/utf8>>, <<"enum">>, 0, 255, <<>>, 1},
        {<<"battery_temp1">>, <<"电池温度1"/utf8>>, <<"int">>, -128, 127, <<"℃"/utf8>>, 1},
        {<<"battery_temp2">>, <<"电池温度2"/utf8>>, <<"int">>, -128, 127, <<"℃"/utf8>>, 1},
        {<<"battery_sequence">>, <<"通信序列编号"/utf8>>, <<"int">>, 0, 255, <<>>, 1},
        {<<"battery_cmd_result">>, <<"指令执行结果"/utf8>>, <<"enum">>, 0, 255, <<>>, 1}
    ],
    [field_map_from_tuple(T, <<"AUXILIARY">>) || T <- BaseList].

%% 测试项字段映射（单独保留 TEST_ITEM 组）
-spec test_item_field_mappings() -> [#field_map{}].
test_item_field_mappings() ->
    BaseList = [
        {<<"test_item_device_id">>, <<"测试项设备ID"/utf8>>, <<"text">>, 0, 0, <<>>, 1},
        {<<"test_step">>, <<"测试步骤"/utf8>>, <<"int">>, 0, 1000, <<>>, 1},
        {<<"test_result">>, <<"测试结果"/utf8>>, <<"text">>, 0, 0, <<>>, 1},
        {<<"fuse1_ground_voltage">>, <<"保险丝1对地电压"/utf8>>, <<"float">>, 0, 30, <<"V"/utf8>>, 0.1},
        {<<"fuse5_ground_voltage">>, <<"保险丝5对地电压"/utf8>>, <<"float">>, 0, 30, <<"V"/utf8>>, 0.1},
        {<<"battery_port_resistance">>, <<"电池端口电阻"/utf8>>, <<"float">>, 0, 100, <<"Ω"/utf8>>, 0.01},
        {<<"fuse8_wing_nail_resistance">>, <<"保险丝8翼钉电阻"/utf8>>, <<"float">>, 0, 100, <<"Ω"/utf8>>, 0.01},
        {<<"fuse7_wing_nail_resistance">>, <<"保险丝7翼钉电阻"/utf8>>, <<"float">>, 0, 100, <<"Ω"/utf8>>, 0.01},
        {<<"fuse7_8_resistance">>, <<"保险丝7-8电阻"/utf8>>, <<"float">>, 0, 100, <<"Ω"/utf8>>, 0.01},
        {<<"fuse9_10_resistance">>, <<"保险丝9-10电阻"/utf8>>, <<"float">>, 0, 100, <<"Ω"/utf8>>, 0.01}
    ],
    % 所有测试项字段统一放入 TEST_ITEM 组（包含原 TEST_ITEM 和 FIXTURE_TEST 字段）
    [field_map_from_tuple(T, <<"TEST_ITEM">>) || T <- BaseList] ++
    [field_map_from_tuple(T, <<"TEST_ITEM">>) || T <- lists:sublist(BaseList, 8, 7)].

%% 链路状态字段映射（独立为 LINK 组）
-spec link_field_mappings() -> [#field_map{}].
link_field_mappings() ->
    BaseList = [
        {<<"link_up_ber">>, <<"上行接收信道误码率"/utf8>>, <<"int">>, 0, 255, <<>>, 1},
        {<<"link_air_status">>, <<"空中节点状态字节"/utf8>>, <<"int">>, 0, 255, <<>>, 1},
        {<<"link_air_agc">>, <<"空中节点接收AGC"/utf8>>, <<"int">>, 0, 255, <<>>, 1},
        {<<"link_work_channel">>, <<"工作频道"/utf8>>, <<"int">>, 1, 9, <<>>, 1},
        {<<"link_air_set_channel">>, <<"空中节点设置频道"/utf8>>, <<"int">>, 1, 9, <<>>, 1},
        {<<"link_air_set_addr">>, <<"空中节点设置地址"/utf8>>, <<"int">>, 0, 65535, <<>>, 1},
        {<<"link_down_ber">>, <<"下行接收信道误码率"/utf8>>, <<"int">>, 0, 255, <<>>, 1},
        {<<"link_ground_status">>, <<"地面状态字节"/utf8>>, <<"int">>, 0, 255, <<>>, 1},
        {<<"link_ground_agc1">>, <<"地面直收1AGC"/utf8>>, <<"int">>, 0, 255, <<>>, 1},
        {<<"link_ground_agc2">>, <<"地面直收2AGC"/utf8>>, <<"int">>, 0, 255, <<>>, 1},
        {<<"link_ground_agc3">>, <<"下行转发AGC"/utf8>>, <<"int">>, 0, 255, <<>>, 1},
        {<<"link_ground_work_channel">>, <<"地面工作频道"/utf8>>, <<"int">>, 1, 9, <<>>, 1},
        {<<"link_ground_set_channel">>, <<"地面设置频道"/utf8>>, <<"int">>, 1, 9, <<>>, 1},
        {<<"link_ground_set_addr">>, <<"地面设置地址"/utf8>>, <<"int">>, 0, 65535, <<>>, 1},
        {<<"link_ground_power">>, <<"地面功率状态"/utf8>>, <<"int">>, 1, 3, <<>>, 1},
        {<<"link_ground_work_addr">>, <<"地面工作地址"/utf8>>, <<"int">>, 0, 65535, <<>>, 1},
        {<<"link_range">>, <<"测距值"/utf8>>, <<"int">>, 0, 65535, <<"m">>, 1},
        {<<"link_air_temp">>, <<"空中节点温度"/utf8>>, <<"int">>, -128, 127, <<"℃">>, 1},
        {<<"link_access_flag">>, <<"入网申请标志"/utf8>>, <<"enum">>, 0, 255, <<>>, 1},
        {<<"link_node_address">>, <<"本节点地址"/utf8>>, <<"int">>, 0, 65535, <<>>, 1},
        {<<"link_granted_nodes">>, <<"获得许可节点数"/utf8>>, <<"int">>, 0, 8, <<>>, 1},
        {<<"link_denied_nodes">>, <<"被拒绝节点数"/utf8>>, <<"int">>, 0, 8, <<>>, 1},
        {<<"link_online_nodes">>, <<"在线节点地址列表"/utf8>>, <<"text">>, 0, 0, <<>>, 1},
        {<<"link_latest_denied">>, <<"最近被拒绝节点"/utf8>>, <<"int">>, 0, 65535, <<>>, 1},
        {<<"link_crc1">>, <<"CRC1校验值"/utf8>>, <<"int">>, 0, 65535, <<>>, 1},
        {<<"link_crc2">>, <<"CRC2校验值"/utf8>>, <<"int">>, 0, 65535, <<>>, 1}
    ],
    [field_map_from_tuple(T, <<"LINK">>) || T <- BaseList].

%% 辅助函数
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