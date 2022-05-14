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

-module(dgiot_device_profile).
-author("jonhliu").
-include("dgiot_device.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([post/2, put/2, delete/3, publish/3, publish/4]).

post('before', _BeforeData) ->
    ok;
post('after', _AfterData) ->
    ok.

put('before', #{<<"id">> := DeviceId, <<"profile">> := Profile} = Device) ->
    io:format("~s ~p Device = ~p.~n", [?FILE, ?LINE, Device]),
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"devaddr">> := Devaddr, <<"productid">> := ProductId}} ->
            Profile = maps:get(<<"profile">>, Device, #{}),
            ProfileTopic =
                case dgiot_product:lookup_prod(ProductId) of
                    {ok, #{<<"topics">> := #{<<"profile">> := ToipcTempl}}} ->
                        Topic = re:replace(ToipcTempl, <<"${productId}">>, ProductId, [{return, binary}]),
                        re:replace(Topic, <<"${deviceAddr}">>, Devaddr, [{return, binary}]);
                    _ ->
                        <<"$dg/device/", ProductId/binary, "/", Devaddr/binary, "/profile">>
                end,
            dgiot_mqtt:publish(DeviceId, ProfileTopic, jsx:encode(Profile));
        _ ->
            pass
    end;

put('after', #{<<"id">> := DeviceId, <<"profile">> := Profile}) ->
    io:format("~s ~p DeviceId ~p  Profile = ~p.~n", [?FILE, ?LINE, DeviceId, Profile]),
    dgiot_data:insert(?DEVICE_PROFILE, DeviceId, Profile);

put('after', _Device) ->
    ok.

delete('before', _BeforeData, _ProductId) ->
    ok;
delete('after', #{<<"objectId">> := DtuId}, _ProductId) ->
    dgiot_task:del_pnque(DtuId).

publish(ProductId, DeviceAddr, Profile) ->
    publish(ProductId, DeviceAddr, Profile, 2).

publish(ProductId, DeviceAddr, Profile, Delay) ->
    case dgiot_data:get({profile, channel}) of
        not_find ->
            pass;
        ChannelId ->
            dgiot_channelx:do_message(ChannelId, {sync_profile, self(), ProductId, DeviceAddr, Profile, Delay})
    end.