%%%-------------------------------------------------------------------
%%% @doc
%%% auto_thing - 自动生成无人机、舵面传感器、噪音传感器物模型的聚合模块。
%%% 收集各子模块的字段映射，并提供统一的更新接口。
%%% 
%%% 主要功能：
%%% - 根据字段映射列表生成物模型属性列表（generate_thing_from_fields/1）
%%% - 更新指定产品的物模型（update_product_thing/2, update_product_thing_safe/1/2）
%%% - 更新特定产品的物模型：无人机（update_uav_thing/0）、舵面传感器（update_surface_device_thing/0）、噪音传感器（update_noise_device_thing/0）
%%% - 重建 TDengine 超级表（recreate_td_super_table/2）及对应的简化版本
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(auto_thing).

-compile({nowarn_unused_function, [build_property/1, generate_enum_list/1]}).

-export([
    generate_thing_from_fields/1,
    update_product_thing/2,
    update_product_thing_safe/1,
    update_product_thing_safe/2,
    update_uav_thing/0,
    update_surface_device_thing/0,
    update_noise_device_thing/0,
    recreate_td_super_table/2,
    recreate_uav_super_table/0,
    recreate_surface_super_table/0,
    recreate_noise_super_table/0
]).

-include_lib("dgiot/include/logger.hrl").

%% 产品ID常量
-define(UAV_PRODUCT_ID, <<"6235befb62">>).
-define(SURFACE_PRODUCT_ID, <<"de7130b0a1">>).   % 舵面传感器产品ID
-define(NOISE_PRODUCT_ID, <<"51f2902af3">>).     % 噪音传感器产品ID
-define(TD_CHANNEL_ID, <<"24b9b4bc50">>).

%% 字段映射记录
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

%%%===================================================================
%%% API 函数
%%%===================================================================

-spec generate_thing_from_fields([#field_map{}]) -> map().
generate_thing_from_fields(FieldMappings) ->
    Properties0 = [build_property(Map) || Map <- FieldMappings],
    UniquePropsMap = lists:foldl(fun(Prop, Acc) ->
        Id = maps:get(<<"identifier">>, Prop),
        Acc#{Id => Prop}
    end, #{}, Properties0),
    Properties = maps:values(UniquePropsMap),
    #{<<"properties">> => Properties}.

-spec update_product_thing(binary(), [#field_map{}]) -> ok | {error, term()}.
update_product_thing(ProductId, FieldMappings) ->
    Props = generate_thing_from_fields(FieldMappings),
    ProductUpdate = #{<<"objectId">> => ProductId, <<"thing">> => Props},
    dgiot_product:put(ProductUpdate).

-spec update_product_thing_safe(binary()) -> ok | {error, term()}.
update_product_thing_safe(ProductId) ->
    AllFields = case ProductId of
        ?UAV_PRODUCT_ID ->
            auto_thing_d1:field_mappings() ++
            auto_thing_d2:field_mappings() ++
            auto_thing_d3:field_mappings() ++
            auto_thing_surface:surface_field_mappings() ++
            auto_thing_extra:test_item_field_mappings() ++
            auto_thing_noise:noise_field_mappings() ++
            auto_thing_extra:version_field_mappings() ++
            auto_thing_extra:waypoint_field_mappings() ++
            auto_thing_extra:surface_calibration_field_mappings() ++
            auto_thing_extra:battery_field_mappings() ++
            auto_thing_extra:link_field_mappings();
        ?SURFACE_PRODUCT_ID ->
            auto_thing_surface:surface_device_field_mappings();
        ?NOISE_PRODUCT_ID ->
            auto_thing_noise:noise_device_field_mappings();
        _ ->
            []
    end,
    update_product_thing_safe(ProductId, AllFields).

-spec update_product_thing_safe(binary(), [#field_map{}]) -> ok | {error, term()}.
update_product_thing_safe(ProductId, FieldMappings) ->
    NewThing = generate_thing_from_fields(FieldMappings),
    Body = #{<<"thing">> => NewThing},
    case dgiot_parse:update_object(<<"Product">>, ProductId, Body) of
        {ok, Result} ->
            ?LOG(info, "产品 ~p 物模型更新成功", [ProductId]),
            _ = refresh_product_cache(ProductId),
            {ok, Result};
        {error, Reason} ->
            ?LOG(error, "产品 ~p 物模型更新失败: ~p", [ProductId, Reason]),
            {error, Reason}
    end.

%% @doc 更新无人机产品物模型（包含 D1/D2/D3/SURFACE/TEST_ITEM/NOISE 及新增命令字段）
-spec update_uav_thing() -> ok | {error, term()}.
update_uav_thing() ->
    update_product_thing_safe(?UAV_PRODUCT_ID).

%% @doc 更新舵面传感器产品物模型（仅包含10个测量值）
-spec update_surface_device_thing() -> ok | {error, term()}.
update_surface_device_thing() ->
    update_product_thing_safe(?SURFACE_PRODUCT_ID).

%% @doc 更新噪音传感器产品物模型（仅包含 noise 字段）
-spec update_noise_device_thing() -> ok | {error, term()}.
update_noise_device_thing() ->
    update_product_thing_safe(?NOISE_PRODUCT_ID).

refresh_product_cache(ProductId) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, _Product} ->
            case erlang:function_exported(dgiot_product, lookup_prod, 1) of
                true -> dgiot_product:lookup_prod(ProductId);
                false ->
                    ?LOG(info, "未找到 dgiot_product:load_product/1，等待 2 秒让缓存自动过期"),
                    timer:sleep(2000)
            end;
        {error, Reason} ->
            ?LOG(error, "刷新产品缓存失败: ~p", [Reason])
    end.

-spec recreate_td_super_table(binary(), binary()) -> ok | {error, term()}.
recreate_td_super_table(ProductId, ChannelId) ->
    ?LOG(info, "开始重建 TDengine 超级表 for ProductId=~p, ChannelId=~p", [ProductId, ChannelId]),
    DB = dgiot_tdengine:get_database(ChannelId, ProductId),
    Stable = <<"_", ProductId/binary>>,
    SqlDrop = <<"DROP TABLE IF EXISTS ", DB/binary, Stable/binary, ";">>,
    ?LOG(info, "执行 SQL: ~s", [SqlDrop]),
    case dgiot_tdengine:batch_sql(ChannelId, DB, SqlDrop) of
        {ok, _} ->
            ?LOG(info, "表删除成功，现在更新物模型..."),
            case auto_thing:update_product_thing_safe(ProductId) of
                {ok, _} ->
                    ?LOG(info, "物模型更新成功，超级表将在下次插入时自动重建（或已重建）"),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "更新物模型失败: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG(error, "删除表失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 简化重建无人机超级表
-spec recreate_uav_super_table() -> ok | {error, term()}.
recreate_uav_super_table() ->
    recreate_td_super_table(?UAV_PRODUCT_ID, ?TD_CHANNEL_ID).

%% @doc 简化重建舵面传感器超级表
-spec recreate_surface_super_table() -> ok | {error, term()}.
recreate_surface_super_table() ->
    recreate_td_super_table(?SURFACE_PRODUCT_ID, ?TD_CHANNEL_ID).

%% @doc 简化重建噪音传感器超级表
-spec recreate_noise_super_table() -> ok | {error, term()}.
recreate_noise_super_table() ->
    recreate_td_super_table(?NOISE_PRODUCT_ID, ?TD_CHANNEL_ID).

%%%===================================================================
%%% 内部函数：构建属性对象和枚举列表
%%%===================================================================

-spec build_property(#field_map{}) -> map().
build_property(#field_map{identifier = Id, name = Name, type = Type,
                          min = Min, max = Max, unit = Unit, step = Step,
                          group = Group}) ->
    BaseProp = #{
        <<"accessMode">> => <<"r">>,
        <<"dataForm">> => #{
            <<"address">> => Id,
            <<"protocol">> => <<"UAV">>,
            <<"round">> => <<"all">>,
            <<"strategy">> => 20,
            <<"countround">> => <<"all">>,
            <<"countstrategy">> => 20,
            <<"collection">> => <<"%{s}">>,
            <<"countcollection">> => <<"%{s}">>,
            <<"control">> => <<"%{d}">>,
            <<"data">> => <<"null">>,
            <<"offset">> => 0,
            <<"order">> => 0,
            <<"operatetype">> => <<"readCoils">>,
            <<"originaltype">> => <<"short16_AB">>,
            <<"rate">> => 1,
            <<"slaveid">> => Id,
            <<"iscount">> => <<"0">>
        },
        <<"dataSource">> => #{<<"dis">> => []},
        <<"devicetype">> => Group,
        <<"identifier">> => Id,
        <<"isaccumulate">> => false,
        <<"isshow">> => true,
        <<"isstorage">> => true,
        <<"moduleType">> => <<"properties">>,
        <<"name">> => Name,
        <<"required">> => false,
        <<"index">> => 0
    },
    DataType = case Type of
        <<"enum">> ->
            #{
                <<"type">> => <<"enum">>,
                <<"specs">> => #{<<"enumList">> => generate_enum_list(Id)},
                <<"das">> => []
            };
        <<"text">> ->
            #{
                <<"type">> => <<"text">>,
                <<"specs">> => #{<<"size">> => 12},
                <<"das">> => []
            };
        _ ->
            #{
                <<"type">> => Type,
                <<"specs">> => #{<<"min">> => Min, <<"max">> => Max, <<"unit">> => Unit, <<"step">> => Step},
                <<"das">> => []
            }
    end,
    BaseProp#{<<"dataType">> => DataType}.

-spec generate_enum_list(binary()) -> [map()].
generate_enum_list(<<"battery_heating_flag">>) ->
    [#{<<"text">> => <<"正常"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"加热中"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"reset_type">>) ->
    [#{<<"text">> => <<"上电复位"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"看门狗复位"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"detonation_power_status">>) ->
    [#{<<"text">> => <<"未供电"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"已供电"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"data_binding_executed">>) ->
    [#{<<"text">> => <<"未执行"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"载荷控制"/utf8>>, <<"value">> => 16#FD}];
generate_enum_list(<<"flight_mode">>) ->
    [#{<<"text">> => <<"未知"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"高度保持"/utf8>>, <<"value">> => 16#12},
     #{<<"text">> => <<"返航"/utf8>>, <<"value">> => 16#13},
     #{<<"text">> => <<"盘旋"/utf8>>, <<"value">> => 16#14},
     #{<<"text">> => <<"导航"/utf8>>, <<"value">> => 16#15},
     #{<<"text">> => <<"起飞"/utf8>>, <<"value">> => 16#16},
     #{<<"text">> => <<"降落"/utf8>>, <<"value">> => 16#17},
     #{<<"text">> => <<"复飞"/utf8>>, <<"value">> => 16#19},
     #{<<"text">> => <<"攻击"/utf8>>, <<"value">> => 16#1B},
     #{<<"text">> => <<"桶滚"/utf8>>, <<"value">> => 16#1C}];
generate_enum_list(<<"attack_mode">>) ->
    [#{<<"text">> => <<"未知"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"图像制导"/utf8>>, <<"value">> => 1},
     #{<<"text">> => <<"位置制导"/utf8>>, <<"value">> => 2}];
%% 故障类枚举（0=正常，1=故障）
generate_enum_list(<<"fault_", _/binary>>) ->
    [#{<<"text">> => <<"正常"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"故障"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"warning_", _/binary>>) ->
    [#{<<"text">> => <<"否"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"是"/utf8>>, <<"value">> => 1}];
%% D2 飞行模态枚举
generate_enum_list(<<"throttle_mode">>) ->
    [#{<<"text">> => <<"遥控"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"空速控制"/utf8>>, <<"value">> => 1},
     #{<<"text">> => <<"着陆油门"/utf8>>, <<"value">> => 2}];
generate_enum_list(<<"longitudinal_mode">>) ->
    [#{<<"text">> => <<"起飞控制"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"俯冲"/utf8>>, <<"value">> => 1},
     #{<<"text">> => <<"高度控制"/utf8>>, <<"value">> => 2},
     #{<<"text">> => <<"爬升"/utf8>>, <<"value">> => 3},
     #{<<"text">> => <<"高度斜坡控制"/utf8>>, <<"value">> => 4},
     #{<<"text">> => <<"攻击导引"/utf8>>, <<"value">> => 5},
     #{<<"text">> => <<"遥控"/utf8>>, <<"value">> => 6}];
generate_enum_list(<<"lateral_mode">>) ->
    [#{<<"text">> => <<"遥控"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"滚转角控制"/utf8>>, <<"value">> => 1},
     #{<<"text">> => <<"航向控制"/utf8>>, <<"value">> => 2},
     #{<<"text">> => <<"航迹控制"/utf8>>, <<"value">> => 3},
     #{<<"text">> => <<"攻击控制"/utf8>>, <<"value">> => 4}];
generate_enum_list(<<"in_air">>) ->
    [#{<<"text">> => <<"在地上"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"在空中"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"circle_mode">>) ->
    [#{<<"text">> => <<"不盘旋"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"盘旋"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"navigation_mode">>) ->
    [#{<<"text">> => <<"无导引"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"自主起飞"/utf8>>, <<"value">> => 1},
     #{<<"text">> => <<"航线导引"/utf8>>, <<"value">> => 2},
     #{<<"text">> => <<"绕点左盘导"/utf8>>, <<"value">> => 3},
     #{<<"text">> => <<"自动回收降落导引"/utf8>>, <<"value">> => 4},
     #{<<"text">> => <<"攻击导引"/utf8>>, <<"value">> => 5},
     #{<<"text">> => <<"复飞导引"/utf8>>, <<"value">> => 6}];
generate_enum_list(<<"beidou_code_type">>) ->
    [#{<<"text">> => <<"未定义"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"军码"/utf8>>, <<"value">> => 1},
     #{<<"text">> => <<"民码"/utf8>>, <<"value">> => 2},
     #{<<"text">> => <<"其他"/utf8>>, <<"value">> => 3}];
generate_enum_list(<<"beidou_position_valid">>) ->
    [#{<<"text">> => <<"定位无效"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"定位有效"/utf8>>, <<"value">> => 1}];
%% 载荷状态字枚举
generate_enum_list(<<"payload_type">>) ->
    [#{<<"text">> => <<"未知"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"平台式可见光"/utf8>>, <<"value">> => 5},
     #{<<"text">> => <<"平台式红外白热"/utf8>>, <<"value">> => 6},
     #{<<"text">> => <<"平台式红外黑热"/utf8>>, <<"value">> => 7}];
generate_enum_list(<<"payload_compression_mode">>) ->
    [#{<<"text">> => <<"压缩模式0"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"其他"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"payload_image_stabilization">>) ->
    [#{<<"text">> => <<"稳像关"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"稳像开"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"payload_work_state">>) ->
    [#{<<"text">> => <<"载荷休眠"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"手动调节"/utf8>>, <<"value">> => 1},
     #{<<"text">> => <<"自动调节"/utf8>>, <<"value">> => 2},
     #{<<"text">> => <<"故障"/utf8>>, <<"value">> => 3}];
generate_enum_list(<<"ir_zoom">>) ->
    [#{<<"text">> => <<"预留"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"1x"/utf8>>, <<"value">> => 1},
     #{<<"text">> => <<"2x"/utf8>>, <<"value">> => 2},
     #{<<"text">> => <<"3x"/utf8>>, <<"value">> => 3}];
generate_enum_list(<<"vis_zoom">>) ->
    [#{<<"text">> => <<"预置"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"1x"/utf8>>, <<"value">> => 1},
     #{<<"text">> => <<"2x"/utf8>>, <<"value">> => 2},
     #{<<"text">> => <<"3x"/utf8>>, <<"value">> => 3},
     #{<<"text">> => <<"4x"/utf8>>, <<"value">> => 4},
     #{<<"text">> => <<"5x"/utf8>>, <<"value">> => 5}];
generate_enum_list(<<"image_enhance">>) ->
    [#{<<"text">> => <<"不增强"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"增强未知"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"payload_protect_state">>) ->
    [#{<<"text">> => <<"工作态"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"保护态"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"target_relative_height_flag">>) ->
    [#{<<"text">> => <<"无效"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"有效"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"control_surface_status">>) ->
    [#{<<"text">> => <<"正常舵面输出"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"舵面测试使能"/utf8>>, <<"value">> => 2},
     #{<<"text">> => <<"动力试车使能"/utf8>>, <<"value">> => 3}];
%% D3 枚举
generate_enum_list(<<"snr_source">>) ->
    [#{<<"text">> => <<"GPS信噪比"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"北斗信噪比"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"position_source">>) ->
    [#{<<"text">> => <<"GPS位置"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"北斗位置"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"magnetic_type">>) ->
    [#{<<"text">> => <<"磁强误差"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"磁强值"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"beidou_self_destruct_status">>) ->
    [#{<<"text">> => <<"未自毁"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"已自毁"/utf8>>, <<"value">> => 1}];
%% 战斗部状态（0正常，1异常）
generate_enum_list(<<"warhead_", _/binary>>) ->
    [#{<<"text">> => <<"否"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"是"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"wing_deployed">>) ->
    [#{<<"text">> => <<"未展开"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"已展开"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"isolation_status">>) ->
    [#{<<"text">> => <<"隔离"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"解除隔离"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"detonation_flag">>) ->
    [#{<<"text">> => <<"未起爆"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"已起爆"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"conductive_membrane_valid">>) ->
    [#{<<"text">> => <<"无效"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"有效"/utf8>>, <<"value">> => 1}];
%% 开关状态（0关，1开）
generate_enum_list(<<"soft_switch1">>) -> 
    [#{<<"text">> => <<"关"/utf8>>, <<"value">> => 0}, #{<<"text">> => <<"开"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"soft_switch2">>) -> 
    [#{<<"text">> => <<"关"/utf8>>, <<"value">> => 0}, #{<<"text">> => <<"开"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"left_wing_switch">>) -> 
    [#{<<"text">> => <<"关"/utf8>>, <<"value">> => 0}, #{<<"text">> => <<"开"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"right_wing_switch">>) -> 
    [#{<<"text">> => <<"关"/utf8>>, <<"value">> => 0}, #{<<"text">> => <<"开"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"hard_switch_measure">>) -> 
    [#{<<"text">> => <<"关"/utf8>>, <<"value">> => 0}, #{<<"text">> => <<"开"/utf8>>, <<"value">> => 1}];
generate_enum_list(<<"drone_type">>) ->
    [#{<<"text">> => <<"未知"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"任务型"/utf8>>, <<"value">> => 2},
     #{<<"text">> => <<"训练型"/utf8>>, <<"value">> => 4}];
%% 电池激活状态
generate_enum_list(<<"battery_activate_state">>) ->
    [#{<<"text">> => <<"激活已关闭"/utf8>>, <<"value">> => 16#A1},
     #{<<"text">> => <<"激活已开启"/utf8>>, <<"value">> => 16#1A}];
%% 电池指令执行结果
generate_enum_list(<<"battery_cmd_result">>) ->
    [#{<<"text">> => <<"激活成功"/utf8>>, <<"value">> => 16#77},
     #{<<"text">> => <<"激活未执行"/utf8>>, <<"value">> => 16#99},
     #{<<"text">> => <<"其他"/utf8>>, <<"value">> => 0}];
%% 舵面通道
generate_enum_list(<<"surface_channel">>) ->
    [#{<<"text">> => <<"左前舵"/utf8>>, <<"value">> => 1},
     #{<<"text">> => <<"右前舵"/utf8>>, <<"value">> => 2},
     #{<<"text">> => <<"左垂尾"/utf8>>, <<"value">> => 3},
     #{<<"text">> => <<"右垂尾"/utf8>>, <<"value">> => 4}];
%% 链路状态枚举
generate_enum_list(<<"link_access_flag">>) ->
    [#{<<"text">> => <<"无效"/utf8>>, <<"value">> => 0},
     #{<<"text">> => <<"有效"/utf8>>, <<"value">> => 16#AA}];
%% 默认返回空列表
generate_enum_list(_) ->
    [].