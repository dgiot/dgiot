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

-module(dgiot_device).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/dgiot_mnesia.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TIMEOUT, 60000).

-export([create_device/1, create_device/3, get_sub_device/1, get_sub_device/2, save_subdevice/2, get_subdevice/2, get_subdevices/2, save_subdevice/3]).
-export([parse_cache_Device/1, sync_parse/1, get/2, post/1, post/2, put/1, save/1, save/2, lookup/1, lookup/2, delete/1, delete/2]).
-export([save_profile/1, get_profile/1, get_profile/2, get_online/1, online/1, offline/1, offline_child/1, enable/1, disable/1]).
-export([put_color/3, get_color/2, put_location/3, get_location/1, get_address/3, get_productid/1]).
-export([get_acl/1, get_readonly_acl/1, save_log/3, get_url/1, get_appname/1]).

parse_cache_Device(Skip) ->
    dgiot_device_cache:parse_cache_Device(Skip).

sync_parse(OffLine) ->
    dgiot_device_cache:sync_parse(OffLine).

post(Device) ->
    dgiot_device_cache:post(Device).

post(Device, Token) ->
    dgiot_device_cache:post(Device, Token).

put(Device) ->
    dgiot_device_cache:put(Device).

save(ProductId, DevAddr) ->
    dgiot_device_cache:save(ProductId, DevAddr).

save(Device) ->
    dgiot_device_cache:save(Device).

get_subdevice(DtuAddr, SlaveId) ->
    dgiot_device_cache:get_subdevice(DtuAddr, SlaveId).

lookup(DeviceId) ->
    dgiot_device_cache:lookup(DeviceId).

lookup(ProductId, DevAddr) ->
    dgiot_device_cache:lookup(ProductId, DevAddr).

delete(DeviceId) ->
    dgiot_device_cache:delete(DeviceId).

delete(ProductId, DevAddr) ->
    dgiot_device_cache:delete(ProductId, DevAddr).

save_profile(Device) ->
    dgiot_device_cache:save_profile(Device).

get_profile(DeviceId) ->
    dgiot_device_cache:get_profile(DeviceId).

get_profile(DeviceId, Key) ->
    dgiot_device_cache:get_profile(DeviceId, Key).

get_productid(DeviceId) ->
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"productid">> := ProductId}} ->
            ProductId;
        _ ->
            case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"product">> := #{<<"objectId">> := ProductId}}} ->
                    ProductId;
                _ ->
                    not_find
            end
    end.

get_online(DeviceId) ->
    dgiot_device_cache:get_online(DeviceId).

online(DeviceId) ->
    dgiot_device_cache:online(DeviceId).

offline(DeviceId) ->
    dgiot_device_cache:offline(DeviceId).

offline_child(DeviceId) ->
    dgiot_device_cache:offline_child(DeviceId).

enable(DeviceId) ->
    dgiot_device_cache:enable(DeviceId).

disable(DeviceId) ->
    dgiot_device_cache:disable(DeviceId).

put_color(DeviceId, Identifier, Value) ->
    dgiot_data:insert(?DEVICE_DEVICE_COLOR, {DeviceId, Identifier}, Value).

get_color(DeviceId, Identifier) ->
    case dgiot_data:get(?DEVICE_DEVICE_COLOR, {DeviceId, Identifier}) of
        not_find ->
            {<<"not">>, 0, 0};
        Color1 ->
            Color1
    end.

put_location(DeviceId, Longitude, Latitude) ->
    dgiot_device_cache:location(DeviceId, Longitude, Latitude).

get_location(DeviceId) ->
    dgiot_device_cache:get_location(DeviceId).

get_address(DeviceId, DgLon, DgLat) ->
    dgiot_device_cache:get_address(DeviceId, DgLon, DgLat).

save_subdevice({ProductId, DevAddr}, {DtuAddr, SlaveId}) ->
    dgiot_device_cache:save_subdevice({ProductId, DevAddr}, {DtuAddr, SlaveId}).

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

create_device(#{<<"status">> := Status, <<"brand">> := Brand, <<"devModel">> := DevModel, <<"name">> := Name,
    <<"devaddr">> := DevAddr, <<"product">> := ProductId, <<"ACL">> := Acl} = Device) ->
    DeviceId = maps:get(<<"objectId">>, Device, dgiot_parse_id:get_deviceid(ProductId, DevAddr)),
    case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"ip">> := Ip} = Result} ->
            Body = #{
                <<"ip">> => maps:get(<<"ip">>, Device, Ip),
                <<"status">> => Status},
            dgiot_parsex:update_object(<<"Device">>, DeviceId, Body),
            dgiot_device:put(#{<<"objectId">> => DeviceId, <<"status">> => Status}),
            {ok, Result};
        _R ->
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
            end
    end.

create_device(ProductId, DeviceAddr, Ip) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
    case dgiot_device:lookup(DeviceId) of
        {ok, _} ->
            Body = #{<<"status">> => <<"ONLINE">>},
            dgiot_device:online(DeviceId),
            dgiot_parsex:update_object(<<"Device">>, DeviceId, Body);
        _ ->
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

get(ProductId, DevAddr) ->
    Keys = [<<"objectId">>, <<"status">>, <<"isEnable">>],
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
        {ok, Device} ->
            case maps:get(<<"isEnable">>, Device, false) of
                false ->
                    {error, forbiden};
                true ->
                    {ok, maps:with(Keys, Device)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_url(AppName) ->
    Roleid = dgiot_parse_id:get_roleid(AppName),
    case dgiot_parsex:get_object(<<"_Role">>, Roleid) of
        {ok, #{<<"tag">> := #{<<"appconfig">> := #{<<"file">> := Url}}}} ->
            <<Url/binary>>;
        _ -> <<"">>
    end.


get_acl(DeviceId) when is_binary(DeviceId) ->
    case lookup(DeviceId) of
        {ok, #{<<"acl">> := Acls}} ->
            lists:foldl(fun(Acl, Acc) ->
                maps:merge(get_acl(Acl), Acc)
                        end, #{}, Acls);
        _ ->
            #{<<"*">> => #{
                <<"read">> => true,
                <<"write">> => true}
            }
    end;

get_acl(Acl) when is_atom(Acl) ->
    ACL = dgiot_utils:to_binary(Acl),
    #{ACL => #{
        <<"read">> => true,
        <<"write">> => true}
    }.

get_appname(DeviceId) ->
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"acl">> := [Acl | _]}} ->
            BinAcl = atom_to_binary(Acl),
            case BinAcl of
                <<"role:", Name/binary>> ->
                    Name;
                _ ->
                    <<"admin">>
            end;
        _ ->
            <<"admin">>
    end.

save_log(DeviceId, Payload, Domain) ->
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"devaddr">> := Devaddr, <<"productid">> := ProductId}} ->
            ?MLOG(info, #{
                <<"deviceid">> => DeviceId,
                <<"devaddr">> => Devaddr,
                <<"productid">> => ProductId,
                <<"msg">> => Payload}, Domain);
        _ ->
            pass
    end.


get_readonly_acl(DeviceId) ->
    case dgiot_device_cache:lookup(DeviceId) of
        {ok, #{<<"acl">> := AclList}} ->
            lists:foldl(
                fun(Role, Acc) ->
                    Acc#{Role => #{
                        <<"read">> => true,
                        <<"write">> => false}}
                end, #{}, AclList);
        _ ->
            #{}
    end.
