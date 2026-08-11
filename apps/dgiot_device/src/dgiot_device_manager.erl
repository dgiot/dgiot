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

-module(dgiot_device_manager).
-author("kenneth").
-include("dgiot_device.hrl").

-include_lib("dgiot/include/dgiot_mnesia.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([create_device/1, create_device/3,
         update_device/2,
         delete_device/1, delete_device/2,
         get_device/2,
         validate_device_data/1,
         get_productid/1]).


%% @doc 创建设备 - 完整设备数据
-spec create_device(Device :: map()) -> {ok, map()} | {error, term()}.
create_device(#{
                <<"status">> := Status,
                <<"brand">> := Brand,
                <<"devModel">> := DevModel,
                <<"name">> := Name,
                <<"devaddr">> := DevAddr,
                <<"product">> := ProductId,
                <<"ACL">> := Acl
               } = Device) ->

    DeviceId = maps:get(<<"objectId">>, Device, dgiot_parse_id:get_deviceid(ProductId, DevAddr)),

    case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
        {ok, Result} ->
            %% 设备已存在，更新状态
            Body = #{
                     <<"ip">> => maps:get(<<"ip">>, Device, maps:get(<<"ip">>, Result, <<>>)),
                     <<"status">> => Status
                    },
            dgiot_parsex:update_object(<<"Device">>, DeviceId, Body),
            dgiot_device:put(#{<<"objectId">> => DeviceId, <<"status">> => Status}),
            {ok, Result};
        _R ->
            %% 新建设备
            create_new_device(Device, ProductId, DevAddr, Name, Brand, DevModel, Acl)
    end.


%% @doc 创建设备 - 简化参数
-spec create_device(ProductId :: binary(), DeviceAddr :: binary(), Ip :: binary()) -> ok | pass.
create_device(ProductId, DeviceAddr, Ip) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
    dgiot_device:save_log(ProductId, DeviceAddr, DeviceAddr, <<"online">>),

    case dgiot_device:lookup(DeviceId) of
        {ok, _} ->
            %% 设备已存在，更新为在线状态
            Body = #{<<"status">> => <<"ONLINE">>},
            dgiot_device:online(DeviceId),
            dgiot_parsex:update_object(<<"Device">>, DeviceId, Body);
        _ ->
            %% 新建设备
            case dgiot_product:lookup_prod(ProductId) of
                not_find ->
                    pass;
                {ok, #{<<"ACL">> := Acl, <<"name">> := Name, <<"devType">> := DevType, <<"dynamicReg">> := true}} ->
                    <<DeviceSecret:10/binary, _/binary>> = dgiot_utils:to_md5(dgiot_utils:random()),
                    Device = #{
                               <<"ip">> => Ip,
                               <<"status">> => <<"ONLINE">>,
                               <<"deviceSecret">> => DeviceSecret,
                               <<"isEnable">> => true,
                               <<"brand">> => Name,
                               <<"devModel">> => DevType,
                               <<"name">> => DeviceAddr,
                               <<"devaddr">> => DeviceAddr,
                               <<"product">> => ProductId,
                               <<"ACL">> => Acl
                              },
                    dgiot_device:create_device(Device);
                _ ->
                    pass
            end
    end.


%% @doc 更新设备信息
-spec update_device(DeviceId :: binary(), Updates :: map()) -> ok | {error, term()}.
update_device(DeviceId, Updates) when is_binary(DeviceId), is_map(Updates) ->
    case dgiot_parsex:update_object(<<"Device">>, DeviceId, Updates) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.


%% @doc 删除设备 - 通过设备ID
-spec delete_device(DeviceId :: binary()) -> ok | {error, term()}.
delete_device(DeviceId) ->
<<<<<<< HEAD
    case dgiot_parsex:del_object(<<"Device">>, DeviceId) of
        {ok, _Result} ->
            ok;
        {error, Reason} ->
            case Reason of
                #{<<"code">> := 101} ->
                    %% Object not found - 幂等删除，静默成功
                    ok;
                {<<"Error">>, #{<<"code">> := 101}} ->
                    ok;
                _ ->
                    ?LOG(error, "Failed to delete device ~p: ~p", [DeviceId, Reason]),
                    {error, Reason}
            end
=======
    ?LOG(info, "Deleting device: ~p", [DeviceId]),
    case dgiot_parsex:del_object(<<"Device">>, DeviceId) of
        {ok, _Result} ->
            ?LOG(info, "Device deleted successfully: ~p", [DeviceId]),
            ok;
        {error, {<<"Error">>, #{<<"code">> := 101, <<"error">> := <<"Object not found.">>}}} ->
            ?LOG(warning, "Device not found: ~p", [DeviceId]),
            {error, device_not_found};
        {error, Reason} ->
            ?LOG(error, "Failed to delete device ~p: ~p", [DeviceId, Reason]),
            {error, Reason}
>>>>>>> origin/dgaiot-plugins
    end.


%% @doc 删除设备 - 通过产品和设备地址
-spec delete_device(ProductId :: binary(), DevAddr :: binary()) -> ok | {error, term()}.
delete_device(ProductId, DevAddr) ->
    ?LOG(info, "Deleting device by product ~p and address ~p", [ProductId, DevAddr]),
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    delete_device(DeviceId).


%% @doc 获取设备信息
-spec get_device(ProductId :: binary(), DevAddr :: binary()) -> {ok, map()} | {error, term()}.
get_device(ProductId, DevAddr) ->
    Keys = [<<"objectId">>, <<"status">>, <<"isEnable">>],
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),

    case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
        {ok, Device} ->
            case maps:get(<<"isEnable">>, Device, false) of
                false -> {error, forbidden};
                true -> {ok, maps:with(Keys, Device)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc 验证设备数据
-spec validate_device_data(Device :: map()) -> {ok, map()} | {error, term()}.
validate_device_data(Device) when is_map(Device) ->
    RequiredFields = [<<"product">>, <<"devaddr">>, <<"name">>],

    case validate_required_fields(Device, RequiredFields) of
        ok ->
            {ok, Device};
        {error, MissingField} ->
            {error, {missing_field, MissingField}}
    end.


%% @doc 获取设备的产品ID
-spec get_productid(DeviceId :: binary()) -> binary() | not_find.
get_productid(DeviceId) ->
    case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"product">> := #{<<"objectId">> := ProductId}}} ->
            ProductId;
        {ok, #{<<"product">> := ProductId}} when is_binary(ProductId) ->
            ProductId;
        _ ->
            not_find
    end.


%% ===================================================================
%% Internal functions
%% ===================================================================


%% @private 创建新设备
create_new_device(Device, ProductId, _DevAddr, Name, Brand, DevModel, Acl) ->
    {{Y, M, D}, {_, _, _}} = dgiot_datetime:local_time(),
    Batch_name = dgiot_utils:to_list(Y) ++ dgiot_utils:to_list(M) ++ dgiot_utils:to_list(D),
    <<DeviceSecret:10/binary, _/binary>> = dgiot_utils:to_md5(dgiot_utils:random()),

    NewDevice = Device#{
                  <<"basedata">> => maps:get(<<"basedata">>, Device, #{}),
                  <<"content">> => maps:get(<<"content">>, Device, #{}),
                  <<"profile">> => maps:get(<<"profile">>, Device, #{}),
                  <<"isEnable">> => maps:get(<<"isEnable">>, Device, true),
                  <<"product">> => #{
                                     <<"__type">> => <<"Pointer">>,
                                     <<"className">> => <<"Product">>,
                                     <<"objectId">> => ProductId
                                    },
                  <<"ACL">> => maps:without([<<"*">>], Acl),
                  <<"state">> => maps:get(<<"state">>, Device, 0),
                  <<"deviceSecret">> => maps:get(<<"deviceSecret">>, Device, DeviceSecret),
                  <<"detail">> => #{
                                    <<"desc">> => Name,
                                    <<"brand">> => Brand,
                                    <<"devModel">> => DevModel,
                                    <<"assetNum">> => maps:get(<<"assetNum">>, Device, <<"">>),
                                    <<"address">> => maps:get(<<"address">>, Device, <<"">>),
                                    <<"batchId">> => #{
                                                       <<"batch_name">> => dgiot_utils:to_binary(Batch_name),
                                                       <<"createdtime">> => dgiot_datetime:now_secs()
                                                      }
                                   }
                 },

    case dgiot_parsex:create_object(<<"Device">>, maps:without([<<"brand">>, <<"devModel">>], NewDevice)) of
        {ok, R} ->
            dgiot_device:post(NewDevice#{<<"product">> => ProductId}),
            {ok, R};
        R1 ->
            R1
    end.


%% @private 验证必需字段
validate_required_fields(Device, [Field | Rest]) ->
    case maps:is_key(Field, Device) of
        true -> validate_required_fields(Device, Rest);
        false -> {error, Field}
    end;
validate_required_fields(_Device, []) ->
    ok.
