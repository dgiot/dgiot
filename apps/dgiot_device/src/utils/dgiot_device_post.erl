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

-module(dgiot_device_post).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([post/1, post/2, batch_post/1]).

%% @doc 创建设备记录
%% 如果没有提供ACL，将使用默认权限
post(Device) ->
    try
        do_post(Device)
    catch
        Type:Reason:Stacktrace ->
            ?LOG(error, "Failed to create device: ~p:~p~nStacktrace: ~p", 
                 [Type, Reason, Stacktrace]),
            {error, Reason}
    end.

%% @doc 使用会话令牌创建设备记录
%% 根据用户角色设置设备ACL
post(Device, SessionToken) ->
    try
        do_post_with_session(Device, SessionToken)
    catch
        Type:Reason:Stacktrace ->
            ?LOG(error, "Failed to create device with session: ~p:~p~nStacktrace: ~p", 
                 [Type, Reason, Stacktrace]),
            {error, Reason}
    end.

%% @doc 批量创建设备记录
batch_post(Devices) when is_list(Devices) ->
    StartTime = os:system_time(millisecond),
    Results = lists:map(fun post/1, Devices),
    
    {Successful, Failed} = lists:partition(fun
        ({ok, _}) -> true;
        ({error, _}) -> false
    end, Results),
    
    EndTime = os:system_time(millisecond),
    Duration = EndTime - StartTime,
    
    ?LOG(info, "Batch post completed: ~p successful, ~p failed, took ~p ms", 
         [length(Successful), length(Failed), Duration]),
    
    case Failed of
        [] ->
            {ok, [Id || {ok, Id} <- Successful]};
        _ ->
            {partial_success, [Id || {ok, Id} <- Successful], Failed}
    end.

%%% 内部函数

%% @doc 执行设备创建
do_post(Device) ->
    % 提取设备基本信息
    #{<<"longitude">> := Longitude, <<"latitude">> := Latitude} = get_location(Device),
    Devaddr = maps:get(<<"devaddr">>, Device),
    Product = maps:get(<<"product">>, Device),
    ProductId = get_product_id(Product),
    DeviceSecret = get_device_secret(Device),
    DeviceId = get_device_id(Device, ProductId, Devaddr),
    
    % 获取其他必要信息
    ParentId = get_parent_id(Device),
    {Status, IsEnable} = get_status_and_enable(Device),
    Acl = dgiot_role:get_acls(Device),
    State = maps:get(<<"state">>, Device, 0),
    Now = dgiot_datetime:now_secs(),
    
    % 插入到Mnesia
    dgiot_device_cache:insert_mnesia(
        DeviceId, Acl, Status, State, Now, IsEnable, 
        ProductId, Devaddr, DeviceSecret, node(), 
        Longitude, Latitude, ParentId
    ),
    
    ?LOG(debug, "Successfully created device: ~p", [DeviceId]),
    {ok, DeviceId}.

%% @doc 使用会话令牌执行设备创建
do_post_with_session(Device, SessionToken) ->
    case maps:find(<<"ACL">>, Device) of
        {ok, _} ->
            % 如果设备已经有ACL，直接使用
            do_post(Device);
        error ->
            % 如果没有ACL，根据会话设置ACL
            DeviceId = get_device_id_from_device(Device),
            case set_device_acl_from_session(DeviceId, SessionToken) of
                {ok, SetAcl} ->
                    do_post(Device#{<<"ACL">> => SetAcl});
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 从会话设置设备ACL
set_device_acl_from_session(DeviceId, SessionToken) ->
    case dgiot_auth:get_session(dgiot_utils:to_binary(SessionToken)) of
        #{<<"roles">> := Roles} = _User ->
            case maps:values(Roles) of
                [#{<<"name">> := Role} | _] ->
                    SetAcl = #{
                        <<"role:", Role/binary>> => #{
                            <<"read">> => true,
                            <<"write">> => true
                        }
                    },
                    % 更新Parse中的设备ACL
                    case dgiot_parsex:update_object(<<"Device">>, DeviceId, #{<<"ACL">> => SetAcl}) of
                        {ok, _} ->
                            {ok, SetAcl};
                        {error, Reason} ->
                            ?LOG(error, "Failed to update device ACL: ~p", [Reason]),
                            {error, Reason}
                    end;
                _ ->
                    ?LOG(error, "No roles found in session for device ~p", [DeviceId]),
                    {error, no_roles_found}
            end;
        {error, Reason} ->
            ?LOG(error, "Invalid session token for device ~p: ~p", [DeviceId, Reason]),
            {error, invalid_session};
        _ ->
            ?LOG(error, "Unexpected session format for device ~p", [DeviceId]),
            {error, unexpected_session_format}
    end.

%% @doc 获取位置信息
get_location(Device) ->
    case maps:find(<<"location">>, Device) of
        {ok, Location} ->
            Location;
        error ->
            % 如果没有提供位置，使用默认位置
            #{<<"longitude">> => 120.065714, <<"latitude">> => 30.369491}
    end.

%% @doc 获取产品ID
get_product_id(#{<<"objectId">> := ProductId}) ->
    ProductId;
get_product_id(ProductId) when is_binary(ProductId) ->
    ProductId;
get_product_id(_) ->
    throw(invalid_product_id).

%% @doc 获取设备密钥
get_device_secret(Device) ->
    case maps:find(<<"deviceSecret">>, Device) of
        {ok, Secret} when byte_size(Secret) > 0 ->
            Secret;
        _ ->
            <<"oioojn">>  % 默认设备密钥
    end.

%% @doc 获取设备ID
get_device_id(#{<<"objectId">> := DeviceId}, _ProductId, _Devaddr) ->
    DeviceId;
get_device_id(_, ProductId, Devaddr) ->
    dgiot_parse_id:get_deviceid(ProductId, Devaddr).

%% @doc 从设备数据中获取设备ID
get_device_id_from_device(Device) ->
    Devaddr = maps:get(<<"devaddr">>, Device),
    Product = maps:get(<<"product">>, Device),
    ProductId = get_product_id(Product),
    get_device_id(Device, ProductId, Devaddr).

%% @doc 获取父设备ID
get_parent_id(Device) ->
    case maps:find(<<"parentId">>, Device) of
        {ok, #{<<"objectId">> := ParentId}} ->
            ParentId;
        {ok, ParentId} when is_binary(ParentId) ->
            ParentId;
        _ ->
            <<"">>  % 默认父设备ID为空
    end.

%% @doc 获取状态和使能状态
get_status_and_enable(Device) ->
    case maps:get(<<"status">>, Device, <<"OFFLINE">>) of
        <<"OFFLINE">> ->
            {false, maps:get(<<"isEnable">>, Device, false)};
        _ ->
            {true, maps:get(<<"isEnable">>, Device, true)}
    end.
