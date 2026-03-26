%%%-------------------------------------------------------------------
%%% @doc
%%% auto_thing_noise - 噪音传感器相关字段映射模块
%%% 包含无人机物模型中的噪音数据及噪音传感器自身物模型。
%%% @end
%%%-------------------------------------------------------------------
-module(auto_thing_noise).

-export([noise_field_mappings/0, noise_device_field_mappings/0]).

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

%% NOISE 字段映射（无人机物模型中的单个噪音传感器噪声值）
-spec noise_field_mappings() -> [#field_map{}].
noise_field_mappings() ->
    [
        field_map_from_tuple({<<"noise">>, <<"噪音传感器噪声值"/utf8>>, <<"float">>, 30, 130, <<"dB"/utf8>>, 0.1}, <<"TEST_ITEM">>)
    ].

%% 噪音传感器自身物模型（仅噪声值）
-spec noise_device_field_mappings() -> [#field_map{}].
noise_device_field_mappings() ->
    [
        field_map_from_tuple({<<"noise">>, <<"噪声值"/utf8>>, <<"float">>, 30, 130, <<"dB"/utf8>>, 0.1}, <<"NOISE_DEVICE">>)
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