%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

%% @doc dgiot_bamis Protocol
-module(dgiot_bamis).
-include("dgiot_bamis.hrl").
-include_lib("dgiot/include/logger.hrl").
-dgiot_swagger(<<"amis">>).

-export([
    create_amis/3,
    put_amis_device/2,
    del_amis_device/1,
    created_amis_device/3
]).

-define(APP, ?MODULE).
del_amis_device(DeviceId) ->
    dgiot_device:delete(DeviceId).
%%修改设备
put_amis_device( #{<<"objectId">> := Deviceid} = Body, SessionToken) ->
    case dgiot_parse:get_object(<<"Device">>, Deviceid,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"data">> := OldRole}} ->
            dgiot_parse:update_object(<<"Device">>, Deviceid, #{
                <<"data">> => maps:without([
                    <<"parent">>,
                    <<"createdAt">>,
                    <<"updatedAt">>,
                    <<"ACL">>,
                    <<"objectId">>
                    ], maps:merge(OldRole, Body))},
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);
        Error -> Error
    end.

%%新设备
created_amis_device(DtuAddr, ChannelId, DTUIP) ->
    {ProductId, Acl, _Properties} = dgiot_data:get({amis, ChannelId}),
    Requests = #{
        <<"devaddr">> => DtuAddr,
        <<"name">> => <<"AMIS_", DtuAddr/binary>>,
        <<"ip">> => DTUIP,
        <<"isEnable">> => true,
        <<"product">> => ProductId,
        <<"ACL">> => Acl,
        <<"status">> => <<"ONLINE">>,
        <<"brand">> => <<"AMIS", DtuAddr/binary>>,
        <<"devModel">> => <<"AMIS">>
    },
    dgiot_device:create_device(Requests).

%%新设备
create_amis(DtuAddr, ChannelId, DTUIP) ->
    {ProductId, Acl, _Properties} = dgiot_data:get({amis, ChannelId}),
    Requests = #{
        <<"devaddr">> => DtuAddr,
        <<"name">> => <<"AMIS_", DtuAddr/binary>>,
        <<"ip">> => DTUIP,
        <<"isEnable">> => true,
        <<"product">> => ProductId,
        <<"ACL">> => Acl,
        <<"status">> => <<"ONLINE">>,
        <<"brand">> => <<"AMIS", DtuAddr/binary>>,
        <<"devModel">> => <<"AMIS">>
    },
    dgiot_device:create_device(Requests).
