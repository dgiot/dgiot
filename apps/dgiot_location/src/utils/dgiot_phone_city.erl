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

%% @doc dgiot_geoip
-module(dgiot_phone_city).
-include("dgiot_location.hrl").
-include_lib("dgiot/include/logger.hrl").
-dgiot_data("ets").
-export([init_ets/0]).

-export([
    load/0,
    lookup/1
]).


init_ets() ->
    dgiot_data:init(?PHONE_CITYE).

load() ->
    DownloadPath = code:priv_dir(dgiot_location) ++ "/csv/phone_city.csv",
    Fun =
        fun
            ([_Code, Phone, Provice, City | _]) ->
                dgiot_data:insert(?PHONE_CITYE, Phone, <<Provice/binary, City/binary, ""/utf8>>);
            (_) ->
                pass
        end,
    dgiot_csv:read_from_csv(DownloadPath, Fun),
    ok.

lookup(<<Phone:7/binary, _Rest/binary>>) ->
    case dgiot_data:get(?PHONE_CITYE, Phone) of
        not_find ->
            <<"">>;
        Address ->
            Address
    end;
lookup(_) ->
    <<"">>.
