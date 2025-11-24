%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(dgiot_device).
-author("kenneth").
-include_lib("dgiot_device/include/dgiot_device.hrl").
-include_lib("dgiot/include/dgiot_mnesia.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TIMEOUT, 60000).

-export([create_device/1, create_device/3]).
-export([get_sub_device/1, get_sub_device/2, save_subdevice/2, get_subdevice/2, get_subdevices/2, save_subdevice/3, get_parent_id/1]).
-export([parse_cache_Device/1, sync_parse/1, get/2, post/1, post/2, put/1, save/1, save/2, lookup/1, lookup/2, delete/1, delete/2]).
-export([save_profile/1, get_profile/1, get_profile/2, get_online/1, online/1, offline/1, offline_child/1, enable/1, disable/1]).
-export([put_color/3, get_color/2, put_location/3, get_location/1, get_address/3, get_productid/1]).
-export([get_acl/1, get_roleids/1, get_readonly_acl/1, save_log/3, get_url/1, get_appname/1, save_log/4]).

%% ===================================================================
%% 设备缓存和数据同步函数
%% ===================================================================

%% @doc 解析缓存设备
-spec parse_cache_Device(Skip :: integer()) -> any().
parse_cache_Device(Skip) ->
    dgiot_device_cache:parse_cache_Device(Skip).

%% @doc 同步解析设备数据
-spec sync_parse(OffLine :: term()) -> any().
sync_parse(OffLine) ->
    dgiot_device_sync:sync_parse(OffLine).

%% ===================================================================
%% 设备数据操作函数
%% ===================================================================

%% @doc 创建设备 - 完整设备数据
%% @param Device 设备数据映射
%% @returns {ok, CreatedDevice} | {error, Reason} 创建设备结果
-spec create_device(Device :: map()) -> {ok, map()} | {error, term()}.
create_device(Device) ->
    case dgiot_device_manager:create_device(Device) of
        {ok, CreatedDevice} ->
            ?LOG(info, "Device created successfully: ~p", [maps:get(<<"devaddr">>, Device, <<"unknown">>)]),
            {ok, CreatedDevice};
        {error, Reason} ->
            ?LOG(error, "Failed to create device ~p: ~p", [maps:get(<<"devaddr">>, Device, <<"unknown">>), Reason]),
            {error, Reason}
    end.

%% @doc 创建设备 - 简化参数
%% @param ProductId 产品ID
%% @param DeviceAddr 设备地址
%% @param Ip 设备IP地址
%% @returns ok | pass 创建设备结果
-spec create_device(ProductId :: binary(), DeviceAddr :: binary(), Ip :: binary()) -> ok | pass.
create_device(ProductId, DeviceAddr, Ip) ->
    case dgiot_device_manager:create_device(ProductId, DeviceAddr, Ip) of
        ok -> 
            ?LOG(info, "Device ~p/~p created successfully", [ProductId, DeviceAddr]),
            ok;
        pass ->
            ?LOG(debug, "Device ~p/~p creation passed (already exists)", [ProductId, DeviceAddr]),
            pass;
        Result ->
            ?LOG(error, "Unexpected result when creating device ~p/~p: ~p", [ProductId, DeviceAddr, Result]),
            Result
    end.

%% @doc 获取设备信息
-spec get(ProductId :: binary(), DevAddr :: binary()) -> {ok, map()} | {error, term()}.
get(ProductId, DevAddr) ->
    dgiot_device_manager:get_device(ProductId, DevAddr).

%% @doc 提交设备数据
-spec post(Device :: map()) -> any().
post(Device) ->
    dgiot_device_post:post(Device).

%% @doc 提交设备数据（带令牌）
-spec post(Device :: map(), Token :: binary()) -> any().
post(Device, Token) ->
    dgiot_device_post:post(Device, Token).

%% @doc 更新设备数据
-spec put(Device :: map()) -> any().
put(Device) ->
    dgiot_device_put:put(Device).

%% @doc 保存设备 - 通过产品和设备地址
-spec save(ProductId :: binary(), DevAddr :: binary()) -> any().
save(ProductId, DevAddr) ->
    dgiot_device_cache:save(ProductId, DevAddr).

%% @doc 保存设备 - 通过设备数据
-spec save(Device :: map()) -> any().
save(Device) ->
    dgiot_device_cache:save(Device).

%% @doc 查找设备 - 通过设备ID
-spec lookup(DeviceId :: binary()) -> {ok, map()} | {error, term()}.
lookup(DeviceId) ->
    dgiot_device_cache:lookup(DeviceId).

%% @doc 查找设备 - 通过产品和设备地址
-spec lookup(ProductId :: binary(), DevAddr :: binary()) -> {ok, map()} | {error, term()}.
lookup(ProductId, DevAddr) ->
    dgiot_device_cache:lookup(ProductId, DevAddr).

%% @doc 删除设备 - 通过设备ID
%% @param DeviceId 设备唯一标识符
%% @returns ok | {error, Reason} 删除结果
-spec delete(DeviceId :: binary()) -> ok | {error, term()}.
delete(DeviceId) ->
    case dgiot_device_manager:delete_device(DeviceId) of
        ok -> 
            ?LOG(info, "Device ~p deleted successfully", [DeviceId]),
            ok;
        {error, Reason} ->
            ?LOG(error, "Failed to delete device ~p: ~p", [DeviceId, Reason]),
            {error, Reason}
    end.

%% @doc 删除设备 - 通过产品和设备地址
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @returns ok | {error, Reason} 删除结果
-spec delete(ProductId :: binary(), DevAddr :: binary()) -> ok | {error, term()}.
delete(ProductId, DevAddr) ->
    case dgiot_device_manager:delete_device(ProductId, DevAddr) of
        ok -> 
            ?LOG(info, "Device ~p/~p deleted successfully", [ProductId, DevAddr]),
            ok;
        {error, Reason} ->
            ?LOG(error, "Failed to delete device ~p/~p: ~p", [ProductId, DevAddr, Reason]),
            {error, Reason}
    end.

%% @doc 获取父设备ID - 通过设备ID
%% @param DeviceId 设备唯一标识符
%% @returns ParentId | not_found 父设备ID或未找到
-spec get_parent_id(DeviceId :: binary()) -> binary() | not_found.
get_parent_id(DeviceId) ->
    case dgiot_device_cache:get_parent_id(DeviceId) of
        not_find ->
            ?LOG(debug, "Parent device not found for device ~p", [DeviceId]),
            not_found;
        ParentId ->
            ?LOG(debug, "Parent device ~p found for device ~p", [ParentId, DeviceId]),
            ParentId
    end.

%% @doc 保存设备配置
-spec save_profile(Device :: map()) -> any().
save_profile(Device) ->
    dgiot_device_cache:save_profile(Device).

%% @doc 获取设备配置
-spec get_profile(DeviceId :: binary()) -> any().
get_profile(DeviceId) ->
    dgiot_device_cache:get_profile(DeviceId).

%% @doc 获取设备配置项
-spec get_profile(DeviceId :: binary(), Key :: binary()) -> any().
get_profile(DeviceId, Key) ->
    dgiot_device_cache:get_profile(DeviceId, Key).

%% @doc 获取产品ID
-spec get_productid(DeviceId :: binary()) -> binary() | not_find.
get_productid(DeviceId) ->
    dgiot_device_manager:get_productid(DeviceId).

%% ===================================================================
%% 设备状态管理函数
%% ===================================================================

%% @doc 获取设备在线状态
-spec get_online(DeviceId :: binary()) -> boolean().
get_online(DeviceId) ->
    dgiot_device_state:get_online(DeviceId).

%% @doc 设置设备在线
-spec online(DeviceId :: binary()) -> ok | pass.
online(DeviceId) ->
    dgiot_device_state:online(DeviceId).

%% @doc 设置设备离线
-spec offline(DeviceId :: binary()) -> ok | pass.
offline(DeviceId) ->
    dgiot_device_state:offline(DeviceId).

%% @doc 设置子设备离线
-spec offline_child(DeviceId :: binary()) -> any().
offline_child(DeviceId) ->
    dgiot_device_cache:offline_child(DeviceId).

%% @doc 启用设备
-spec enable(DeviceId :: binary()) -> ok | pass.
enable(DeviceId) ->
    dgiot_device_state:enable(DeviceId).

%% @doc 禁用设备
-spec disable(DeviceId :: binary()) -> ok | pass.
disable(DeviceId) ->
    dgiot_device_state:disable(DeviceId).

%% @doc 设置设备颜色
-spec put_color(DeviceId :: binary(), Identifier :: binary(), Value :: term()) -> ok.
put_color(DeviceId, Identifier, Value) ->
    dgiot_device_state:put_color(DeviceId, Identifier, Value).

%% @doc 获取设备颜色
-spec get_color(DeviceId :: binary(), Identifier :: binary()) -> {binary(), integer(), integer()}.
get_color(DeviceId, Identifier) ->
    dgiot_device_state:get_color(DeviceId, Identifier).

%% @doc 设置设备位置
-spec put_location(DeviceId :: binary(), Longitude :: float(), Latitude :: float()) -> ok | pass.
put_location(DeviceId, Longitude, Latitude) ->
    dgiot_device_state:put_location(DeviceId, Longitude, Latitude).

%% @doc 获取设备位置
-spec get_location(DeviceId :: binary()) -> map().
get_location(DeviceId) ->
    dgiot_device_state:get_location(DeviceId).

%% @doc 获取设备地址
-spec get_address(DeviceId :: binary(), DgLon :: float(), DgLat :: float()) -> binary().
get_address(DeviceId, DgLon, DgLat) ->
    dgiot_device_state:get_address(DeviceId, DgLon, DgLat).

%% ===================================================================
%% 子设备管理函数
%% ===================================================================

%% @doc 获取子设备列表 - 通过DTU设备ID
-spec get_sub_device(DtuDeviceId :: binary()) -> list().
get_sub_device(DtuDeviceId) ->
    dgiot_device_subdevice:get_sub_device(DtuDeviceId).

%% @doc 获取子设备列表 - 通过DTU地址和会话令牌
-spec get_sub_device(DtuAddr :: binary(), SessionToken :: binary()) -> list().
get_sub_device(DtuAddr, SessionToken) ->
    dgiot_device_subdevice:get_sub_device(DtuAddr, SessionToken).

%% @doc 获取子设备列表 - 带指定字段
-spec get_subdevices(DtuDeviceId :: binary(), Keys :: list()) -> list().
get_subdevices(DtuDeviceId, Keys) ->
    dgiot_device_subdevice:get_subdevices(DtuDeviceId, Keys).

%% @doc 保存子设备关系 - 通过产品和设备地址
-spec save_subdevice({ProductId :: binary(), DevAddr :: binary()}, {DtuAddr :: binary(), SlaveId :: binary()}) -> ok.
save_subdevice(ProductDevAddr, DtuSlaveId) ->
    dgiot_device_subdevice:save_subdevice(ProductDevAddr, DtuSlaveId).

%% @doc 保存子设备关系 - 通过设备ID
-spec save_subdevice(DeviceId :: binary(), DtuDeviceId :: binary(), SlaveId :: binary()) -> ok.
save_subdevice(DeviceId, DtuDeviceId, SlaveId) ->
    dgiot_device_subdevice:save_subdevice(DeviceId, DtuDeviceId, SlaveId).

%% @doc 获取子设备信息
-spec get_subdevice(DtuAddr :: binary(), SlaveId :: binary()) -> term().
get_subdevice(DtuAddr, SlaveId) ->
    dgiot_device_subdevice:get_subdevice(DtuAddr, SlaveId).

%% ===================================================================
%% 设备权限管理函数
%% ===================================================================

%% @doc 获取设备ACL - 通过设备ID或角色名
-spec get_acl(DeviceId :: binary()) -> map();
             (Acl :: atom()) -> map().
get_acl(DeviceId) when is_binary(DeviceId) ->
    dgiot_device_permission:get_acl(DeviceId);
get_acl(Acl) when is_atom(Acl) ->
    dgiot_device_permission:get_acl_by_role(Acl).

%% @doc 获取设备角色ID列表
-spec get_roleids(DeviceId :: binary()) -> list().
get_roleids(DeviceId) ->
    dgiot_device_permission:get_roleids(DeviceId).

%% @doc 获取设备只读ACL
-spec get_readonly_acl(DeviceId :: binary()) -> map().
get_readonly_acl(DeviceId) ->
    dgiot_device_permission:get_readonly_acl(DeviceId).

%% @doc 获取设备应用名称
-spec get_appname(DeviceId :: binary()) -> binary().
get_appname(DeviceId) ->
    dgiot_device_permission:get_appname(DeviceId).

%% ===================================================================
%% 设备日志管理函数
%% ===================================================================

%% @doc 保存设备日志 - 通过设备ID
-spec save_log(DeviceId :: binary(), Payload :: term(), Domain :: binary()) -> ok | pass.
save_log(DeviceId, Payload, Domain) ->
    dgiot_device_logger:save_log(DeviceId, Payload, Domain).

%% @doc 保存设备日志 - 通过产品和设备地址
-spec save_log(ProductId :: binary(), DevAddr :: binary(), Data :: term(), Domain :: binary()) -> ok.
save_log(ProductId, DevAddr, Data, Domain) ->
    dgiot_device_logger:save_log(ProductId, DevAddr, Data, Domain).

%% @doc 获取设备URL
-spec get_url(AppName :: binary()) -> binary().
get_url(AppName) ->
    dgiot_device_logger:get_url(AppName).
