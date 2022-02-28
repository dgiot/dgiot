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
-export([
    create_amis/3
]).

-define(APP, ?MODULE).

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
