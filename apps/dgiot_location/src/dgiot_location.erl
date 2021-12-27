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

%% @doc dgiot_location Protocol
-module(dgiot_location).
-include("dgiot_location.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([parse_frame/3, to_frame/1]).
-export([
    create_dtu/3,
    create_dtu/4,
    create_iq60/4
]).

-define(APP, ?MODULE).

%%新设备
create_dtu(mqtt, DtuAddr, ProductId, DTUIP) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"ACL">> := Acl}} ->
            Requests = #{
                <<"devaddr">> => DtuAddr,
                <<"name">> => <<"DTU_", DtuAddr/binary>>,
                <<"ip">> => DTUIP,
                <<"isEnable">> => true,
                <<"product">> => ProductId,
                <<"ACL">> => Acl,
                <<"status">> => <<"ONLINE">>,
                <<"brand">> => <<"DTU", DtuAddr/binary>>,
                <<"devModel">> => <<"DTU_">>
            },
            dgiot_device:create_device(Requests);
        _ ->
            pass
    end.

create_dtu(DtuAddr, ChannelId, DTUIP) ->
    ?LOG(info, "~p", [dgiot_data:get({dtu, ChannelId})]),
    {ProductId, Acl, _Properties} = dgiot_data:get({dtu, ChannelId}),
    Requests = #{
        <<"devaddr">> => DtuAddr,
        <<"name">> => <<"DTU_", DtuAddr/binary>>,
        <<"ip">> => DTUIP,
        <<"isEnable">> => true,
        <<"product">> => ProductId,
        <<"ACL">> => Acl,
        <<"status">> => <<"ONLINE">>,
        <<"brand">> => <<"DTU", DtuAddr/binary>>,
        <<"devModel">> => <<"DTU_">>
    },
    dgiot_device:create_device(Requests).

create_iq60(MeterAddr, ChannelId, DTUIP, DtuAddr) ->
    {ProductId, ACL, _Properties} = dgiot_data:get({meter, ChannelId}),
    Requests = #{
        <<"devaddr">> => MeterAddr,
        <<"name">> => <<"Meter_", MeterAddr/binary>>,
        <<"ip">> => DTUIP,
        <<"isEnable">> => true,
        <<"product">> => ProductId,
        <<"ACL">> => ACL,
        <<"route">> => #{DtuAddr => MeterAddr},
        <<"status">> => <<"ONLINE">>,
        <<"brand">> => <<"Meter", MeterAddr/binary>>,
        <<"devModel">> => <<"Meter">>
    },
    dgiot_device:create_device(Requests),
    {DtuProductId, _, _} = dgiot_data:get({dtu, ChannelId}),
    dgiot_task:save_pnque(DtuProductId, DtuAddr, ProductId, MeterAddr).


parse_frame(?LOCATION, _Buff, _Opts) ->
    ok.


% DLT376发送抄数指令
to_frame(#{
    <<"devaddr">> := _Addr,
    <<"di">> := _Di,
    <<"command">> := <<"r">>,
    <<"protocol">> := ?LOCATION,
    <<"data">> := <<"null">>
} = _Frame) ->
    ok.

