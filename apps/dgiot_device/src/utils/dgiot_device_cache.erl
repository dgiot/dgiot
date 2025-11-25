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

-module(dgiot_device_cache).
-author("kenneth").
-include("dgiot_device.hrl").
-dgiot_data("ets").
-export([init_ets/0]).
-include_lib("dgiot/include/logger.hrl").
-export([parse_cache_Device/1, save/1, save/2, save_subdevice/2, get_subdevice/2, lookup/1, lookup/2, delete/1, delete/2]).
-export([get_profile/1, get_profile/2, get_online/1, online/1, offline/1, offline_child/1, enable/1, disable/1, save_profile/1]).
-export([location/3, get_location/1, get_address/3]).
-export([put_content/1, put_profile/1, insert_mnesia/13, notification/6]).
-export([get_parent_id/1, get_parent_info/1]).
init_ets() ->
    dgiot_data:init(?DGIOT_LOCATION_ADDRESS, [public, named_table, set, {write_concurrency, true}, {read_concurrency, true}]).

%% Device 数量统计，权限统计，在线离线统计，产品下面设备数量统计等是用户非常关系的数据指标
parse_cache_Device({Skip}) ->
    case dgiot_parsex:query_object(<<"Device">>, #{<<"limit">> => 1000, <<"skip">> => Skip,
        <<"keys">> => [<<"ACL">>, <<"updatedAt">>, <<"state">>, <<"devaddr">>, <<"status">>, <<"isEnable">>, <<"profile">>, <<"product">>, <<"location">>, <<"deviceSecret">>, <<"parentId">>]}) of
        {ok, #{<<"results">> := Results}} when length(Results) == 0 ->
            load_end;
        {ok, #{<<"results">> := Devices}} ->
                save(Devices),
            {next, Skip + 1000};
        _ ->
            {next, Skip}
    end.

save(ProductId, DevAddr) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    online(DeviceId).
save([]) ->
    pass;
save([#{<<"objectId">> := _DeviceId, <<"devaddr">> := _Devaddr, <<"product">> := _Product}  = Device | Devices]) ->
    save_(Device),
    save(Devices);
save([#{<<"objectId">> := DeviceId}| Devices]) ->
    {ok, Device} = dgiot_parsex:get_object(<<"Device">>, DeviceId),
    save_(Device),
    save(Devices);
save(V) ->
    % io:format("~s ~p ~p ~n", [?FILE, ?LINE, V]),
    save_(V).

save_(#{<<"objectId">> := DeviceId, <<"devaddr">> := Devaddr, <<"product">> := Product} = Device) ->
    ProductId = maps:get(<<"objectId">>, Product),
    <<Sct:4/binary, _/binary>> = dgiot_utils:random(),
    DeviceSecret = maps:get(<<"deviceSecret">>, Device, Sct),
    DefaultParentId = <<"">>, % 默认父设备ID为空，表示没有父设备
    ParentId =
        case maps:find(<<"parentId">>, Device) of
            {ok, ParentItem} ->  % 处理找到的值
                case maps:find(<<"objectId">>, ParentItem) of
                    {ok, ParentId1} ->
                        ParentId1;
                    error ->
                        DefaultParentId
                end;
            error ->  % 处理未找到的情况
                DefaultParentId
        end,
    UpdatedAt =
        case maps:get(<<"updatedAt">>, Device, dgiot_datetime:now_secs()) of
            <<Data:10/binary, "T", Time:8/binary, _/binary>> ->
                dgiot_datetime:to_unixtime(dgiot_datetime:to_localtime(<<Data/binary, " ", Time/binary>>)) + dgiot_datetime:timezone() * 60 * 60;
            Now -> Now
        end,
    {Status, IsEnable} =
        case maps:get(<<"status">>, Device, <<"OFFLINE">>) of
            <<"OFFLINE">> ->
                {false, maps:get(<<"isEnable">>, Device, false)};
            _ ->
                {true, maps:get(<<"isEnable">>, Device, true)}
        end,
    {Longitude, Latitude} =
        case maps:find(<<"location">>, Device) of
            {ok, #{<<"longitude">> := Longitude1, <<"latitude">> := Latitude1}} ->
                {Longitude1, Latitude1};
            _ ->
                {120.065714, 30.369491}
        end,
    dgiot_mnesia:insert(DeviceId, ['Device', dgiot_role:get_acls(Device), Status, maps:get(<<"state">>, Device, 0), UpdatedAt, IsEnable, dgiot_utils:to_atom(ProductId), Devaddr, DeviceSecret, node(), Longitude, Latitude, ParentId]).


%% 缓存设备的profile配置
save_profile(#{<<"objectId">> := DeviceId, <<"profile">> := Profile, <<"product">> := #{<<"objectId">> := _ProductId}}) ->
%%    Keys = maps:values(dgiot_product:get_control(ProductId)),
    dgiot_data:insert(?DEVICE_PROFILE, DeviceId, Profile);

save_profile(_Device) ->
    pass.

get_profile(DeviceId) ->
    dgiot_data:get(?DEVICE_PROFILE, DeviceId).

get_profile(DeviceId, Key) ->
    case dgiot_data:get(?DEVICE_PROFILE, DeviceId) of
        #{Key := Value} ->
            Value;
        _ ->
            not_find
    end.

%% 根据产品的content 创建设备默认的content
put_content(#{<<"content">> := _Content}) ->
    pass;
put_content(#{<<"product">> := Product, <<"objectId">> := DeviceId}) ->
    ProductId = maps:get(<<"objectId">>, Product),
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"content">> := Content}} ->
            dgiot_parsex:update_object(<<"Device">>, DeviceId, #{<<"content">> => Content});
        _ ->
            pass
    end;
put_content(_) ->
    pass.

%% 根据产品的profile 创建设备默认的profile
put_profile(#{<<"profile">> := _Content}) ->
    pass;
put_profile(#{<<"product">> := Product, <<"objectId">> := DeviceId}) ->
    ProductId = maps:get(<<"objectId">>, Product),
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"profile">> := Profile}} ->
            dgiot_parsex:update_object(<<"Device">>, DeviceId, #{<<"profile">> => Profile});
        _ ->
            pass
    end;
put_profile(_) ->
    pass.

enable(DeviceId) ->
    case lookup(DeviceId) of
        {ok, #{<<"status">> := Status, <<"state">> := State, <<"acl">> := Acl, <<"devaddr">> := Devaddr, <<"productid">> := ProductId, <<"devicesecret">> := DeviceSecret,
            <<"node">> := Node, <<"longitude">> := Longitude, <<"latitude">> := Latitude, <<"parentId">> := ParentId}} ->
            insert_mnesia(DeviceId, Acl, Status, State, dgiot_datetime:now_secs(), true, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId);
        _ -> pass
    end.

disable(DeviceId) ->
    case lookup(DeviceId) of
        {ok, #{<<"status">> := Status, <<"state">> := State, <<"acl">> := Acl, <<"devaddr">> := Devaddr, <<"productid">> := ProductId, <<"devicesecret">> := DeviceSecret,
            <<"node">> := Node, <<"longitude">> := Longitude, <<"latitude">> := Latitude, <<"parentId">> := ParentId}} ->
            insert_mnesia(DeviceId, Acl, Status, State, dgiot_datetime:now_secs(), false, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId);
        _ -> pass
    end.

location(DeviceId, Longitude, Latitude) ->
    case lookup(DeviceId) of
        {ok, #{<<"status">> := Status, <<"state">> := State, <<"acl">> := Acl, <<"devaddr">> := Devaddr, <<"isEnable">> := IsEnable, <<"productid">> := ProductId, <<"devicesecret">> := DeviceSecret,
            <<"node">> := Node, <<"parentId">> := ParentId}} ->
            insert_mnesia(DeviceId, Acl, Status, State, dgiot_datetime:now_secs(), IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId);
        _ -> pass
    end.

get_location(DeviceId) ->
    case lookup(DeviceId) of
        {ok, #{<<"longitude">> := Longitude, <<"latitude">> := Latitude}} ->
            #{<<"longitude">> => Longitude, <<"latitude">> => Latitude};
        _ ->
            #{}
    end.

get_address(DeviceId, DgLon, DgLat) ->
    Address =
        case dgiot_gps:get_baidu_addr(DgLon, DgLat) of
            #{<<"baiduaddr">> := #{<<"formatted_address">> := Formatted_address}} ->
                Formatted_address;
            _ ->
                case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
                    {ok, #{<<"address">> := Addr}} ->
                        Addr;
                    _ ->
                        <<"">>
                end
        end,
    dgiot_data:insert(?DGIOT_LOCATION_ADDRESS, DeviceId, Address),
    Address.

get_online(DeviceId) ->
    OffLine = dgiot_data:get({device, offline}),
    Now = dgiot_datetime:now_secs(),
    case lookup(DeviceId) of
        {ok, #{<<"time">> := Ts}} when Now - Ts < OffLine ->
            true;
        _ ->
            false
    end.

online(DeviceId) ->
    case lookup(DeviceId) of
        {ok, #{<<"acl">> := Acl, <<"isEnable">> := IsEnable, <<"state">> := State,
            <<"devaddr">> := Devaddr, <<"productid">> := ProductId, <<"devicesecret">> := DeviceSecret, <<"node">> := Node,
            <<"longitude">> := Longitude, <<"latitude">> := Latitude, <<"parentId">> := ParentId}} ->
            insert_mnesia(DeviceId, Acl, true, State, dgiot_datetime:now_secs(), IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId);
        _ -> pass
    end.

offline(DeviceId) ->
    case lookup(DeviceId) of
        {ok, #{<<"time">> := Now, <<"acl">> := Acl, <<"isEnable">> := IsEnable, <<"state">> := State,
            <<"devaddr">> := Devaddr, <<"productid">> := ProductId, <<"devicesecret">> := DeviceSecret, <<"node">> := Node,
            <<"longitude">> := Longitude, <<"latitude">> := Latitude, <<"parentId">> := ParentId}} ->
            insert_mnesia(DeviceId, Acl, false, State, Now, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId),
            offline_child(DeviceId);
        _ -> pass
    end.

offline_child(DeviceId) ->
    case dgiot_task:get_pnque(DeviceId) of
        not_find ->
            pass;
        List ->
            F =
                fun(ProductId, DevAddr) ->
                    offline(dgiot_parse_id:get_deviceid(ProductId, DevAddr))
                end,
            [F(ProductId, DevAddr) || {ProductId, DevAddr} <- List]
    end.

lookup(DeviceId) ->
    case dgiot_mnesia:lookup(DeviceId) of
        {aborted, Reason} ->
            {error, Reason};
        {ok, [{mnesia, _K, ['Device', Acls, Status, State, Time, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId]}]} ->
            {ok, #{<<"status">> => Status, <<"time">> => Time, <<"state">> => State, <<"acl">> => Acls, <<"isEnable">> => IsEnable,
                <<"devaddr">> => Devaddr, <<"productid">> => dgiot_utils:to_binary(ProductId), <<"devicesecret">> => DeviceSecret, <<"node">> => Node,
                <<"longitude">> => Longitude, <<"latitude">> => Latitude, <<"parentId">> => ParentId}};
        _ ->
            {error, not_find}
    end.

lookup(ProductId, DevAddr) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    lookup(DeviceId).

save_subdevice({ProductId, DevAddr}, {DtuAddr, SlaveId}) ->
    dgiot_data:insert({DtuAddr, SlaveId}, {ProductId, DevAddr}).


get_subdevice(DtuAddr, SlaveId) ->
%%    todo 返回productid,写对应save
    dgiot_data:get({DtuAddr, SlaveId}).

delete(DeviceId) ->
    dgiot_mnesia:delete(DeviceId).

delete(ProductId, DevAddr) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    dgiot_mnesia:delete(DeviceId).

notification(DeviceId, Status, Longitude, Latitude, IsEnable, Now) ->
    dgiot_device_notify:notification(DeviceId, Status, Longitude, Latitude, IsEnable, Now).
        
get_parent_id(DeviceId) ->
    case lookup(DeviceId) of
        {ok, DeviceItem} ->
            get_parent_id_from_item(DeviceItem);
        {error, not_find} ->
            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                {ok, Device} ->
                    _ = save(Device),
                    get_parent_id_from_item(Device);
                _ ->
                    <<"">>
            end
    end.
    
get_parent_id_from_item(DeviceItem) ->
    case maps:get(<<"parentId">>, DeviceItem, <<"">>) of
        #{<<"objectId">> := ParentId} -> ParentId;
        ParentId when is_binary(ParentId) -> ParentId;
        _ -> <<"">>
    end.

%% 获取父设备的完整信息
get_parent_info(DeviceId) ->
    case get_parent_id(DeviceId) of
        <<"">> -> 
            #{deviceid => <<"">>, productid => <<"">>, devaddr => <<"">>};
        ParentId ->
            case dgiot_parse:get_object(<<"Device">>, ParentId) of
                {ok, ParentDevice} ->
                    #{
                        deviceid => ParentId,
                        productid => maps:get(<<"product">>, ParentDevice, <<"">>),
                        devaddr => maps:get(<<"devaddr">>, ParentDevice, <<"">>)
                    };
                _ ->
                    #{deviceid => ParentId, productid => <<"">>, devaddr => <<"">>}
            end
    end.

insert_mnesia(DeviceId, Acl, Status, State, Now, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId) ->
    dgiot_mnesia:insert(DeviceId, ['Device', Acl, Status, State, Now, IsEnable, dgiot_utils:to_atom(ProductId), Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId]).
