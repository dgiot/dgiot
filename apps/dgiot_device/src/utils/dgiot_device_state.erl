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

-module(dgiot_device_state).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([
    online/1,
    offline/1,
    get_online/1,
    enable/1,
    disable/1,
    put_color/3,
    get_color/2,
    put_location/3,
    get_location/1,
    get_address/3,
    update_device_status/2
]).

%% @doc 设置设备在线状态
-spec online(DeviceId :: binary()) -> ok | pass.
online(DeviceId) ->
    dgiot_device_cache:online(DeviceId).

%% @doc 设置设备离线状态
-spec offline(DeviceId :: binary()) -> ok | pass.
offline(DeviceId) ->
    dgiot_device_cache:offline(DeviceId).

%% @doc 获取设备在线状态
-spec get_online(DeviceId :: binary()) -> boolean().
get_online(DeviceId) ->
    dgiot_device_cache:get_online(DeviceId).

%% @doc 启用设备
-spec enable(DeviceId :: binary()) -> ok | pass.
enable(DeviceId) ->
    dgiot_device_cache:enable(DeviceId).

%% @doc 禁用设备
-spec disable(DeviceId :: binary()) -> ok | pass.
disable(DeviceId) ->
    dgiot_device_cache:disable(DeviceId).

%% @doc 设置设备颜色
-spec put_color(DeviceId :: binary(), Identifier :: binary(), Value :: term()) -> ok.
put_color(DeviceId, Identifier, Value) ->
    dgiot_data:insert(?DEVICE_DEVICE_COLOR, {DeviceId, Identifier}, Value).

%% @doc 获取设备颜色
-spec get_color(DeviceId :: binary(), Identifier :: binary()) -> {binary(), integer(), integer()}.
get_color(DeviceId, Identifier) ->
    case dgiot_data:get(?DEVICE_DEVICE_COLOR, {DeviceId, Identifier}) of
        not_find ->
            {<<"not">>, 0, 0};
        Color1 ->
            Color1
    end.

%% @doc 设置设备位置
-spec put_location(DeviceId :: binary(), Longitude :: float(), Latitude :: float()) -> ok | pass.
put_location(DeviceId, Longitude, Latitude) ->
    dgiot_device_cache:location(DeviceId, Longitude, Latitude).

%% @doc 获取设备位置
-spec get_location(DeviceId :: binary()) -> map().
get_location(DeviceId) ->
    dgiot_device_cache:get_location(DeviceId).

%% @doc 获取设备地址
-spec get_address(DeviceId :: binary(), DgLon :: float(), DgLat :: float()) -> binary().
get_address(DeviceId, DgLon, DgLat) ->
    dgiot_device_cache:get_address(DeviceId, DgLon, DgLat).

%% @doc 更新设备状态
-spec update_device_status(DeviceId :: binary(), Status :: binary()) -> ok | {error, term()}.
update_device_status(DeviceId, Status) when is_binary(Status) ->
    case Status of
        <<"ONLINE">> ->
            online(DeviceId);
        <<"OFFLINE">> ->
            offline(DeviceId);
        _ ->
            {error, invalid_status}
    end.
