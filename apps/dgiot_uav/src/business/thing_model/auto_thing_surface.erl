%%%-------------------------------------------------------------------
%%% @doc
%%% auto_thing_surface - 舵面相关字段映射模块
%%% 包含无人机物模型中的舵面数据及舵面传感器自身物模型。
%%% @end
%%%-------------------------------------------------------------------
-module(auto_thing_surface).

-export([surface_field_mappings/0, surface_device_field_mappings/0]).

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

%% SURFACE 字段映射（无人机物模型中的舵面数据，五个舵面各10个字段）
-spec surface_field_mappings() -> [#field_map{}].
surface_field_mappings() ->
    Positions = [
        {<<"zqy">>, <<"左前翼"/utf8>>},
        {<<"yqy">>, <<"右前翼"/utf8>>},
        {<<"zcw">>, <<"左侧翼"/utf8>>},
        {<<"ycw">>, <<"右侧翼"/utf8>>},
        {<<"zhj">>, <<"治具基准"/utf8>>}
    ],
    lists:flatmap(fun({Pos, PosName}) ->
        [
            field_map_from_tuple({<<Pos/binary, "_acceleration_x">>, <<PosName/binary, "加速度X"/utf8>>, <<"float">>, -360, 360, <<"g"/utf8>>, 0.001}, <<"SURFACE">>),
            field_map_from_tuple({<<Pos/binary, "_acceleration_y">>, <<PosName/binary, "加速度Y"/utf8>>, <<"float">>, -360, 360, <<"g"/utf8>>, 0.001}, <<"SURFACE">>),
            field_map_from_tuple({<<Pos/binary, "_acceleration_z">>, <<PosName/binary, "加速度Z"/utf8>>, <<"float">>, -360, 360, <<"g"/utf8>>, 0.001}, <<"SURFACE">>),
            field_map_from_tuple({<<Pos/binary, "_angular_x">>, <<PosName/binary, "角速度X"/utf8>>, <<"float">>, -2000, 2000, <<"°/s"/utf8>>, 0.1}, <<"SURFACE">>),
            field_map_from_tuple({<<Pos/binary, "_angular_y">>, <<PosName/binary, "角速度Y"/utf8>>, <<"float">>, -2000, 2000, <<"°/s"/utf8>>, 0.1}, <<"SURFACE">>),
            field_map_from_tuple({<<Pos/binary, "_angular_z">>, <<PosName/binary, "角速度Z"/utf8>>, <<"float">>, -2000, 2000, <<"°/s"/utf8>>, 0.1}, <<"SURFACE">>),
            field_map_from_tuple({<<Pos/binary, "_roll">>, <<PosName/binary, "横滚角"/utf8>>, <<"float">>, -180, 180, <<"°"/utf8>>, 0.01}, <<"SURFACE">>),
            field_map_from_tuple({<<Pos/binary, "_pitch">>, <<PosName/binary, "俯仰角"/utf8>>, <<"float">>, -90, 90, <<"°"/utf8>>, 0.01}, <<"SURFACE">>),
            field_map_from_tuple({<<Pos/binary, "_yaw">>, <<PosName/binary, "航向角"/utf8>>, <<"float">>, 0, 360, <<"°"/utf8>>, 0.01}, <<"SURFACE">>),
            field_map_from_tuple({<<Pos/binary, "_temperature">>, <<PosName/binary, "温度"/utf8>>, <<"float">>, -40, 85, <<"℃"/utf8>>, 0.1}, <<"SURFACE">>)
        ]
    end, Positions).

%% 舵面传感器自身物模型（10个测量值）
-spec surface_device_field_mappings() -> [#field_map{}].
surface_device_field_mappings() ->
    [
        field_map_from_tuple({<<"acceleration_x">>, <<"加速度X"/utf8>>, <<"float">>, -16, 16, <<"g"/utf8>>, 0.001}, <<"SURFACE_DEVICE">>),
        field_map_from_tuple({<<"acceleration_y">>, <<"加速度Y"/utf8>>, <<"float">>, -16, 16, <<"g"/utf8>>, 0.001}, <<"SURFACE_DEVICE">>),
        field_map_from_tuple({<<"acceleration_z">>, <<"加速度Z"/utf8>>, <<"float">>, -16, 16, <<"g"/utf8>>, 0.001}, <<"SURFACE_DEVICE">>),
        field_map_from_tuple({<<"angular_x">>, <<"角速度X"/utf8>>, <<"float">>, -2000, 2000, <<"°/s"/utf8>>, 0.1}, <<"SURFACE_DEVICE">>),
        field_map_from_tuple({<<"angular_y">>, <<"角速度Y"/utf8>>, <<"float">>, -2000, 2000, <<"°/s"/utf8>>, 0.1}, <<"SURFACE_DEVICE">>),
        field_map_from_tuple({<<"angular_z">>, <<"角速度Z"/utf8>>, <<"float">>, -2000, 2000, <<"°/s"/utf8>>, 0.1}, <<"SURFACE_DEVICE">>),
        field_map_from_tuple({<<"roll">>, <<"横滚角"/utf8>>, <<"float">>, -180, 180, <<"°"/utf8>>, 0.01}, <<"SURFACE_DEVICE">>),
        field_map_from_tuple({<<"pitch">>, <<"俯仰角"/utf8>>, <<"float">>, -90, 90, <<"°"/utf8>>, 0.01}, <<"SURFACE_DEVICE">>),
        field_map_from_tuple({<<"yaw">>, <<"航向角"/utf8>>, <<"float">>, 0, 360, <<"°"/utf8>>, 0.01}, <<"SURFACE_DEVICE">>),
        field_map_from_tuple({<<"temperature">>, <<"温度"/utf8>>, <<"float">>, -40, 85, <<"℃"/utf8>>, 0.1}, <<"SURFACE_DEVICE">>)
    ].

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