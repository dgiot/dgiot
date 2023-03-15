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
-module(dgiot_bacnet_utils).
-author("johnliu").
-include_lib("dgiot_bacnet.hrl").
-include_lib("dgiot/include/logger.hrl").
-protocol([?BACNET]).

%% API
-export([whois/0]).

%% ACK <<"810B00190120FFFF00FF1000C40203F7A12205C49103220104">>
whois() ->
    % 构建BACnet Who-Is消息
    WhoIs = <<"810b000c0120ffff00ff1008">>,
    % 将Who-Is消息发送到BACnet广播地址
    dgiot_utils:hex_to_binary(WhoIs).
