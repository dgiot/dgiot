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
-module(dgiot_bacnet).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").

-export([
    scan_opc/1
]).


%% 下发扫描OPC命令
%% topic: dgiot_bacnet_da
%% payload：
%%{
%%"cmdtype":"scan",
%%"opcserver":"Kepware.KEPServerEX.V6"
%%}
scan_opc(#{<<"OPCSEVER">> := OpcServer, <<"Topic">> := Topic}) ->
    Payload = #{
        <<"cmdtype">> => <<"scan">>,
        <<"opcserver">> => OpcServer
    },
    dgiot_mqtt:publish(<<"opcserver">>, Topic, jsx:encode(Payload)).

