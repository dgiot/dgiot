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

%% @doc dgiot_onenet Protocol
-module(dgiot_onenet_sim).
-include_lib("dgiot/include/logger.hrl").
-export([
    position_location_message/1
]).

%% CMIOT_API25L00-物联卡实时位置经纬度查询
%% dgiot_onenet:position_location_message(<<"1440499147259">>).
position_location_message(Msisdn) ->
    Server = dgiot_utils:to_list(application:get_env(dgiot_http, onenet_server, "")),
    AppId = dgiot_utils:to_list(application:get_env(dgiot_http, onenet_appid, "")),
    Secret = dgiot_utils:to_list(application:get_env(dgiot_http, onenet_secret, "")),
    Url = Server ++ "/iotapi/position-location-message?msisdn=" ++ dgiot_utils:to_list(Msisdn) ++ "&appid=" ++ AppId ++ "&secret=" ++ Secret,
    case dgiot_http_client:request(get, {Url, []}) of
        {ok, #{<<"status">> := <<"0">>} = Result} ->
%%            io:format("~s ~p Result = ~ts.~n", [?FILE, ?LINE, dgiot_json:encode(Result)]),
            Result;
        {_, Error} ->
            Error
    end.













