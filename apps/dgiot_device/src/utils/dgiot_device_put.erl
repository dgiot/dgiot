<<<<<<< HEAD
%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_device_put).
-author("jhonliu").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([put/1, batch_put/1]).

%% @doc 更新设备信息
%% 如果设备存在则更新，否则忽略
put(Device) ->
    case get_device_id(Device) of
        {ok, DeviceId} ->
            update_device(DeviceId, Device);
        {error, Reason} ->
            ?LOG(error, "Invalid device data: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 批量更新设备信息
batch_put(Devices) when is_list(Devices) ->
    StartTime = os:system_time(millisecond),
    Results = lists:map(fun put/1, Devices),
    
    {Successful, Failed} = lists:partition(fun
        ({ok, _}) -> true;
        ({error, _}) -> false
    end, Results),
    
    EndTime = os:system_time(millisecond),
    Duration = EndTime - StartTime,
    
    ?LOG(info, "Batch update completed: ~p successful, ~p failed, took ~p ms", 
         [length(Successful), length(Failed), Duration]),
    
    case Failed of
        [] ->
            {ok, [Id || {ok, Id} <- Successful]};
        _ ->
            {partial_success, [Id || {ok, Id} <- Successful], Failed}
    end.

%%% 内部函数

%% @doc 获取设备ID
get_device_id(#{<<"objectId">> := DeviceId}) when is_binary(DeviceId), byte_size(DeviceId) > 0 ->
    {ok, DeviceId};
get_device_id(Device) ->
    {error, {missing_or_invalid_device_id, Device}}.

%% @doc 更新设备数据
update_device(DeviceId, NewDevice) ->
    case dgiot_device_cache:lookup(DeviceId) of
        {ok, ExistingDevice} ->
            do_update_device(DeviceId, NewDevice, ExistingDevice);
        {error, not_find} ->
            ?LOG(debug, "Device not found, skipping update: ~p", [DeviceId]),
            {error, device_not_found};
        {error, Reason} ->
            ?LOG(error, "Failed to lookup device ~p: ~p", [DeviceId, Reason]),
            {error, Reason}
    end.

%% @doc 执行设备更新
do_update_device(DeviceId, NewDevice, ExistingDevice) ->
    try
        % 构建更新数据
        UpdateData = build_update_data(NewDevice, ExistingDevice),
        
        % 插入到Mnesia
        insert_updated_device(DeviceId, UpdateData, ExistingDevice),
        
        ?LOG(debug, "Successfully updated device: ~p", [DeviceId]),
        {ok, DeviceId}
    catch
        Type:Reason:Stacktrace ->
            ?LOG(error, "Failed to update device ~p: ~p:~p~nStacktrace: ~p", 
                 [DeviceId, Type, Reason, Stacktrace]),
            {error, {update_failed, Reason}}
    end.

%% @doc 构建更新数据
build_update_data(NewDevice, ExistingDevice) ->
    #{
        acl => get_updated_acl(NewDevice, ExistingDevice),
        status => get_updated_status(NewDevice, ExistingDevice),
        state => get_updated_state(NewDevice, ExistingDevice),
        time => get_updated_time(NewDevice, ExistingDevice),
        is_enable => get_updated_is_enable(NewDevice, ExistingDevice),
        longitude => get_updated_longitude(NewDevice, ExistingDevice),
        latitude => get_updated_latitude(NewDevice, ExistingDevice),
        parent_id => get_updated_parent_id(NewDevice)
    }.

%% @doc 获取更新的ACL
get_updated_acl(NewDevice, ExistingDevice) ->
    case maps:find(<<"ACL">>, NewDevice) of
        {ok, _} -> dgiot_role:get_acls(NewDevice);
        error -> maps:get(<<"acl">>, ExistingDevice)
    end.

%% @doc 获取更新的状态和时间
get_updated_status(NewDevice, #{<<"status">> := CurrentStatus, <<"time">> := OldTime}) ->
    case maps:get(<<"status">>, NewDevice, undefined) of
        <<"OFFLINE">> -> {false, OldTime};
        <<"ONLINE">> -> {true, dgiot_datetime:now_secs()};
        _ when CurrentStatus == false -> {false, OldTime};
        _ -> {true, dgiot_datetime:now_secs()}
    end.

%% @doc 获取更新的状态
get_updated_state(NewDevice, ExistingDevice) ->
    maps:get(<<"state">>, NewDevice, maps:get(<<"state">>, ExistingDevice)).

%% @doc 获取更新的使能状态
get_updated_is_enable(NewDevice, ExistingDevice) ->
    maps:get(<<"isEnable">>, NewDevice, maps:get(<<"isEnable">>, ExistingDevice)).

%% @doc 获取更新的经度
get_updated_longitude(NewDevice, ExistingDevice) ->
    get_location_coordinate(<<"longitude">>, NewDevice, ExistingDevice).

%% @doc 获取更新的纬度
get_updated_latitude(NewDevice, ExistingDevice) ->
    get_location_coordinate(<<"latitude">>, NewDevice, ExistingDevice).

%% @doc 获取位置坐标
get_location_coordinate(Coord, NewDevice, ExistingDevice) ->
    case maps:get(<<"location">>, NewDevice, undefined) of
        #{Coord := Value} -> Value;
        _ -> maps:get(Coord, ExistingDevice)
    end.

%% @doc 获取更新的父设备ID
get_updated_parent_id(NewDevice) ->
    case maps:get(<<"parentId">>, NewDevice, undefined) of
        #{<<"objectId">> := ParentId} -> ParentId;
        ParentId when is_binary(ParentId) -> ParentId;
        _ -> <<"">>
    end.

%% @doc 获取更新的时间
get_updated_time(_NewDevice, #{<<"time">> := OldTime}) ->
    OldTime.

%% @doc 插入更新后的设备数据
insert_updated_device(DeviceId, UpdateData, ExistingDevice) ->
    #{acl := Acl, status := {Status, Time}, state := State, 
      is_enable := IsEnable, longitude := Longitude, latitude := Latitude, 
      parent_id := ParentId} = UpdateData,
    
    #{<<"productid">> := ProductId, <<"devaddr">> := Devaddr, 
      <<"devicesecret">> := DeviceSecret, <<"node">> := Node} = ExistingDevice,
    
    dgiot_device_cache:insert_mnesia(
        DeviceId, Acl, Status, State, Time, IsEnable, 
        ProductId, Devaddr, DeviceSecret, Node, 
        Longitude, Latitude, ParentId
    ).
=======
-module(dgiot_device_put).
-export([put/1]).
put(X) -> dgiot_device_cache:put(X).
>>>>>>> origin/dgaiot-plugins
