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
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_tdengine/include/dgiot_tdengine.hrl").

-export([parse_cache_Device/1, sync_parse/1, post/1, post/2, put/1, save/1, save/2, save_subdevice/2, get_subdevice/2, lookup/1, lookup/2, delete/1, delete/2]).
-export([get_profile/1, get_profile/2, get_online/1, online/1, offline/1, offline_child/1, enable/1, disable/1, save_profile/1]).
-export([location/3, get_location/1, get_address/1]).


%% Device 数量统计，权限统计，在线离线统计，产品下面设备数量统计等是用户非常关系的数据指标
parse_cache_Device(_ClassName) ->
%%    io:format("~s ~p ~p ~n", [?FILE, ?LINE, ClassName]),
    dgiot_product:load_cache(),
    Success = fun(Page) ->
        lists:map(fun(Device) ->
%%            save_profile(Device),
            dgiot_device:save(Device)
                  end, Page)
              end,
    Query = #{
        <<"order">> => <<"updatedAt">>,
        <<"keys">> => [<<"ACL">>, <<"updatedAt">>, <<"devaddr">>, <<"status">>, <<"isEnable">>, <<"profile">>, <<"product">>, <<"location">>, <<"deviceSecret">>],
        <<"where">> => #{}
    },
    dgiot_parse_loader:start(<<"Device">>, Query, 0, 500, 1000000, Success).


save(ProductId, DevAddr) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    online(DeviceId).

save(#{<<"objectId">> := _DeviceId, <<"devaddr">> := _Devaddr, <<"product">> := _Product} = Device) ->
    save_(Device);
save(#{<<"objectId">> := DeviceId}) ->
    {ok, Device} = dgiot_parse:get_object(<<"Device">>, DeviceId),
    save_(Device);
save(V) ->
    io:format("~s ~p ~p ~n", [?FILE, ?LINE, V]),
    V.

save_(#{<<"objectId">> := DeviceId, <<"devaddr">> := Devaddr, <<"product">> := Product} = Device) ->
    ProductId = maps:get(<<"objectId">>, Product),
    <<Sct:4/binary, _/binary>> = dgiot_utils:random(),
    DeviceSecret = maps:get(<<"deviceSecret">>, Device, Sct),
    UpdatedAt =
        case maps:get(<<"updatedAt">>, Device, dgiot_datetime:now_secs()) of
            <<Data:10/binary, "T", Time:8/binary, _/binary>> ->
                dgiot_datetime:to_unixtime(dgiot_datetime:to_localtime(<<Data/binary, " ", Time/binary>>)) + dgiot_datetime:timezone() * 60 * 60;
            Now -> Now
        end,
    Status =
        case maps:get(<<"status">>, Device, <<"OFFLINE">>) of
            <<"OFFLINE">> -> false;
            _ -> true
        end,
    IsEnable = maps:get(<<"isEnable">>, Device, false),
    #{<<"longitude">> := Longitude, <<"latitude">> := Latitude} =
        maps:get(<<"location">>, Device, #{<<"__type">> => <<"GeoPoint">>, <<"longitude">> => 120.161324, <<"latitude">> => 30.262441}),
    insert_mnesia(DeviceId, dgiot_role:get_acls(Device), Status, UpdatedAt, IsEnable, ProductId, Devaddr, DeviceSecret, node(), Longitude, Latitude).

post(Device) ->
    put_content(Device),
    put_profile(Device),
    Devaddr = maps:get(<<"devaddr">>, Device),
    Product = maps:get(<<"product">>, Device),
    ProductId = maps:get(<<"objectId">>, Product),
    DeviceSecret = maps:get(<<"deviceSecret">>, Device, <<"oioojn">>),
    DeviceId = maps:get(<<"objectId">>, Device, dgiot_parse_id:get_deviceid(ProductId, Devaddr)),
    case dgiot_product:lookup_prod(ProductId) of
        {ok, ProductInfo} ->
            Data = maps:with([<<"profile">>, <<"content">>], ProductInfo),
            dgiot_parse:update_object(<<"Device">>, DeviceId, Data);
        _ ->
            pass
    end,
    #{<<"longitude">> := Longitude, <<"latitude">> := Latitude} =
        maps:get(<<"location">>, Device, #{<<"__type">> => <<"GeoPoint">>, <<"longitude">> => 120.161324, <<"latitude">> => 30.262441}),
    Status =
        case maps:get(<<"status">>, Device, <<"OFFLINE">>) of
            <<"OFFLINE">> -> false;
            _ -> true
        end,
    IsEnable = maps:get(<<"isEnable">>, Device, false),
    insert_mnesia(DeviceId, dgiot_role:get_acls(Device), Status, dgiot_datetime:now_secs(), IsEnable, ProductId, Devaddr, DeviceSecret, node(), Longitude, Latitude).

post(#{<<"ACL">> := _Acl} = Device, _SessionToken) ->
    dgiot_device_cache:post(Device);

post(Device, SessionToken) ->
    Devaddr = maps:get(<<"devaddr">>, Device),
    Product = maps:get(<<"product">>, Device),
    ProductId = maps:get(<<"objectId">>, Product),
    DeviceId = maps:get(<<"objectId">>, Device, dgiot_parse_id:get_deviceid(ProductId, Devaddr)),
    SetAcl =
        case dgiot_auth:get_session(dgiot_utils:to_binary(SessionToken)) of
            #{<<"roles">> := Roles} = _User ->
                [#{<<"name">> := Role} | _] = maps:values(Roles),
                #{
                    <<"role:", Role/binary>> => #{
                        <<"read">> => true,
                        <<"write">> => true
                    }
                };
            Err ->
                ?LOG(error, "~s ~p DeviceId ~p  Err = ~p.~n", [?FILE, ?LINE, DeviceId, Err]),
                #{<<"role:admin">> => #{<<"read">> => true, <<"write">> => true}}
        end,
    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"ACL">> => SetAcl}),
    dgiot_device_cache:post(Device#{<<"ACL">> => SetAcl}).

put(Device) ->
    DeviceId = maps:get(<<"objectId">>, Device),
    case lookup(DeviceId) of
        {ok, #{<<"status">> := Status, <<"acl">> := Acl, <<"isEnable">> := IsEnable, <<"devaddr">> := Devaddr,
            <<"productid">> := ProductId, <<"devicesecret">> := DeviceSecret, <<"node">> := Node, <<"longitude">> := Longitude, <<"latitude">> := Latitude}} ->
            NewIsEnable = maps:get(<<"isEnable">>, Device, IsEnable),
            NewStatus =
                case maps:find(<<"status">>, Device) of
                    error ->
                        Status;
                    {ok, <<"OFFLINE">>} -> false;
                    _ -> true
                end,
            Topic = <<"$dg/user/devicestate/", DeviceId/binary, "/", "report">>,
            dgiot_mqtt:publish(DeviceId, Topic, jsx:encode(#{DeviceId => #{<<"isEnable">> => NewIsEnable}})),
            NewAcl =
                case maps:find(<<"ACL">>, Device) of
                    error ->
                        Acl;
                    _ ->
                        dgiot_role:get_acls(Device)
                end,
            insert_mnesia(DeviceId, NewAcl, NewStatus, dgiot_datetime:now_secs(), NewIsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude);
        _ ->
            pass
    end.

insert_mnesia(DeviceId, Acl, Status, Now, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude) ->
    dgiot_mnesia:insert(DeviceId, ['Device', Acl, Status, Now, IsEnable, dgiot_utils:to_atom(ProductId), Devaddr, DeviceSecret, Node, Longitude, Latitude]).

%% 缓存设备的profile配置
save_profile(#{<<"objectId">> := DeviceId, <<"profile">> := Profile, <<"product">> := #{<<"objectId">> := ProductId}}) ->
    Keys = maps:values(dgiot_product:get_control(ProductId)),
    dgiot_data:insert(?DEVICE_PROFILE, DeviceId, maps:with(Keys, Profile));

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
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"content">> => Content});
        _ ->
            pass
    end;
put_content(_) ->
    pass.

%% 根据产品的content 创建设备默认的content
put_profile(#{<<"profile">> := _Content}) ->
    pass;
put_profile(#{<<"product">> := Product, <<"objectId">> := DeviceId}) ->
    ProductId = maps:get(<<"objectId">>, Product),
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"profile">> := Profile}} ->
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"profile">> => Profile});
        _ ->
            pass
    end;
put_profile(_) ->
    pass.

enable(DeviceId) ->
    case lookup(DeviceId) of
        {ok, #{<<"status">> := Status, <<"acl">> := Acl, <<"devaddr">> := Devaddr, <<"productid">> := ProductId, <<"devicesecret">> := DeviceSecret,
            <<"node">> := Node, <<"longitude">> := Longitude, <<"latitude">> := Latitude}} ->
            insert_mnesia(DeviceId, Acl, Status, dgiot_datetime:now_secs(), true, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude);
        _ -> pass
    end.

disable(DeviceId) ->
    case lookup(DeviceId) of
        {ok, #{<<"status">> := Status, <<"acl">> := Acl, <<"devaddr">> := Devaddr, <<"productid">> := ProductId, <<"devicesecret">> := DeviceSecret,
            <<"node">> := Node, <<"longitude">> := Longitude, <<"latitude">> := Latitude}} ->
            insert_mnesia(DeviceId, Acl, Status, dgiot_datetime:now_secs(), false, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude);
        _ -> pass
    end.

location(DeviceId, Longitude, Latitude) ->
    case lookup(DeviceId) of
        {ok, #{<<"status">> := Status, <<"acl">> := Acl, <<"devaddr">> := Devaddr, <<"productid">> := ProductId, <<"devicesecret">> := DeviceSecret,
            <<"node">> := Node}} ->
            insert_mnesia(DeviceId, Acl, Status, dgiot_datetime:now_secs(), false, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude);
        _ -> pass
    end.

get_location(DeviceId) ->
    case lookup(DeviceId) of
        {ok, #{<<"longitude">> := Longitude, <<"latitude">> := Latitude}} ->
            #{<<"longitude">> => Longitude, <<"latitude">> => Latitude};
        _ ->
            #{}
    end.

get_address(DeviceId) ->
    case lookup(DeviceId) of
        {ok, #{<<"longitude">> := LonDeg, <<"latitude">> := LatDeg}} ->
            Address = dgiot_gps:get_baidu_addr(LonDeg, LatDeg),
            case Address of
                #{<<"baiduaddr">> := #{<<"formatted_address">> := Formatted_address}} ->
                    Formatted_address;
                _ ->
                    <<"">>
            end;
        _ ->
            <<"">>
    end.


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
        {ok, #{<<"acl">> := Acl, <<"isEnable">> := IsEnable,
            <<"devaddr">> := Devaddr, <<"productid">> := ProductId, <<"devicesecret">> := DeviceSecret, <<"node">> := Node,
            <<"longitude">> := Longitude, <<"latitude">> := Latitude}} ->
            insert_mnesia(DeviceId, Acl, true, dgiot_datetime:now_secs(), IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude);
        _ -> pass
    end.

offline(DeviceId) ->
    case lookup(DeviceId) of
        {ok, #{<<"time">> := Now, <<"acl">> := Acl, <<"isEnable">> := IsEnable,
            <<"devaddr">> := Devaddr, <<"productid">> := ProductId, <<"devicesecret">> := DeviceSecret, <<"node">> := Node,
            <<"longitude">> := Longitude, <<"latitude">> := Latitude}} ->
            insert_mnesia(DeviceId, Acl, false, Now, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude),
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

sync_parse(OffLine) ->
    Fun = fun(X) ->
        {_, DeviceId, V} = X,
        Now = dgiot_datetime:now_secs(),
        case V of
            ['Device', Acl, _, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude] when (Now - Last) < 0 ->
                case dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"status">> => <<"ONLINE">>, <<"isEnable">> => IsEnable, <<"lastOnlineTime">> => Last}) of
                    {ok, _R} ->
                        insert_mnesia(DeviceId, Acl, true, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude);
                    _ ->
                        pass
                end,
                timer:sleep(50);
            ['Device', Acl, true, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude] when (Now - Last) > OffLine ->
                case dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"status">> => <<"OFFLINE">>, <<"isEnable">> => IsEnable, <<"lastOnlineTime">> => Last}) of
                    {ok, _R} ->
                        insert_mnesia(DeviceId, Acl, false, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude);
                    _ ->
                        pass
                end,
                timer:sleep(50);
            ['Device', Acl, false, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude] when (Now - Last) < OffLine ->
                case dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"status">> => <<"ONLINE">>, <<"isEnable">> => IsEnable, <<"lastOnlineTime">> => Last}) of
                    {ok, _R} ->
                        insert_mnesia(DeviceId, Acl, true, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude);
                    _ ->
                        pass
                end,
                timer:sleep(50);
            _ ->
                pass
        end,
        false
          end,
    dgiot_mnesia:search(Fun, #{<<"skip">> => 0, <<"limit">> => 1000000}).

lookup(DeviceId) ->
    case dgiot_mnesia:lookup(DeviceId) of
        {aborted, Reason} ->
            {error, Reason};
        {ok, [{mnesia, _K, ['Device', Acls, Status, Time, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude]}]} ->
            {ok, #{<<"status">> => Status, <<"time">> => Time, <<"acl">> => Acls, <<"isEnable">> => IsEnable,
                <<"devaddr">> => Devaddr, <<"productid">> => dgiot_utils:to_binary(ProductId), <<"devicesecret">> => DeviceSecret, <<"node">> => Node,
                <<"longitude">> => Longitude, <<"latitude">> => Latitude}};
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



