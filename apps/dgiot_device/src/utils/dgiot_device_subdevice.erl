<<<<<<< HEAD
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

-module(dgiot_device_subdevice).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([
    get_sub_device/1,
    get_sub_device/2,
    get_subdevices/2,
    save_subdevice/2,
    save_subdevice/3,
    get_subdevice/2,
    get_parent_id/1
]).

%% @doc 获取子设备列表 - 通过DTU设备ID
-spec get_sub_device(DtuDeviceId :: binary()) -> list().
get_sub_device(DtuDeviceId) ->
    Query = #{<<"keys">> => [<<"route">>, <<"devaddr">>, <<"product">>],
        <<"where">> => #{<<"route.", DtuDeviceId/binary>> => #{<<"$regex">> => <<".+">>}},
        <<"order">> => <<"devaddr">>, <<"limit">> => 1000,
        <<"include">> => <<"product">>},
    case dgiot_parsex:query_object(<<"Device">>, Query) of
        {ok, #{<<"results">> := []}} -> [];
        {ok, #{<<"results">> := List}} -> List;
        _ -> []
    end.

%% @doc 获取子设备列表 - 通过DTU地址和会话令牌
-spec get_sub_device(DtuAddr :: binary(), SessionToken :: binary()) -> list().
get_sub_device(DtuAddr, SessionToken) ->
    Query = #{<<"keys">> => [<<"route">>, <<"devaddr">>, <<"product">>],
        <<"where">> => #{<<"route.", DtuAddr/binary>> => #{<<"$regex">> => <<".+">>}},
        <<"order">> => <<"devaddr">>, <<"limit">> => 1000,
        <<"include">> => <<"product">>},
    case dgiot_parsex:query_object(<<"Device">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := []}} -> [];
        {ok, #{<<"results">> := List}} -> List;
        _ -> []
    end.

%% @doc 获取子设备列表 - 带指定字段
-spec get_subdevices(DtuDeviceId :: binary(), Keys :: list()) -> list().
get_subdevices(DtuDeviceId, Keys) ->
    NewKeys = [<<"route">> | Keys],
    Query = #{<<"keys">> => NewKeys,
        <<"where">> => #{<<"route.", DtuDeviceId/binary>> => #{<<"$regex">> => <<".+">>}},
        <<"order">> => <<"devaddr">>, <<"limit">> => 100},
    case dgiot_parsex:query_object(<<"Device">>, Query) of
        {ok, #{<<"results">> := []}} -> [];
        {ok, #{<<"results">> := List}} -> List;
        _ -> []
    end.

%% @doc 保存子设备关系 - 通过产品和设备地址
-spec save_subdevice({ProductId :: binary(), DevAddr :: binary()}, {DtuAddr :: binary(), SlaveId :: binary()}) -> ok.
save_subdevice({ProductId, DevAddr}, {DtuAddr, SlaveId}) ->
    dgiot_device_cache:save_subdevice({ProductId, DevAddr}, {DtuAddr, SlaveId}).

%% @doc 保存子设备关系 - 通过设备ID
-spec save_subdevice(DeviceId :: binary(), DtuDeviceId :: binary(), SlaveId :: binary()) -> ok.
save_subdevice(DeviceId, DtuDeviceId, SlaveId) ->
    Parent = #{
        <<"__type">> => <<"Pointer">>,
        <<"className">> => <<"Device">>,
        <<"objectId">> => DtuDeviceId
    },
    case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"route">> := OldRoute}} ->
            dgiot_parsex:update_object(<<"Device">>, DeviceId, #{<<"route">> => OldRoute#{DtuDeviceId => SlaveId}, <<"parentId">> => Parent});
        _ ->
            dgiot_parsex:update_object(<<"Device">>, DeviceId, #{<<"route">> => #{DtuDeviceId => SlaveId}, <<"parentId">> => Parent})
    end.

%% @doc 获取子设备信息
-spec get_subdevice(DtuAddr :: binary(), SlaveId :: binary()) -> term().
get_subdevice(DtuAddr, SlaveId) ->
    dgiot_device_cache:get_subdevice(DtuAddr, SlaveId).

%% @doc 获取父设备ID
-spec get_parent_id(DeviceId :: binary()) -> binary().
get_parent_id(DeviceId) ->
    dgiot_device_cache:get_parent_id(DeviceId).
=======
-module(dgiot_device_subdevice).
-export([get_sub_device/1, get_sub_device/2, get_subdevices/2,
         save_subdevice/2, save_subdevice/3, get_subdevice/2]).
get_sub_device(_) -> [].
get_sub_device(_, _) -> [].
get_subdevices(_, _) -> [].
save_subdevice(X, Y) -> dgiot_device_cache:save_subdevice(X, Y).
save_subdevice(_, _, _) -> ok.
get_subdevice(X, Y) -> dgiot_device_cache:get_subdevice(X, Y).
>>>>>>> origin/dgaiot-plugins
