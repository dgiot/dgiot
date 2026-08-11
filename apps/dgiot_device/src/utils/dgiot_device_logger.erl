%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_device_logger).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([
    save_log/3,
    save_log/4,
    get_url/1,
    log_device_operation/4
]).

%% @doc 保存设备日志 - 通过设备ID
-spec save_log(DeviceId :: binary(), Payload :: term(), Domain :: binary()) -> ok | pass.
save_log(DeviceId, Payload, Domain) ->
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"devaddr">> := Devaddr, <<"productid">> := ProductId}} ->
            ?MLOG(info, #{
                <<"deviceid">> => DeviceId,
                <<"devaddr">> => Devaddr,
                <<"productid">> => ProductId,
                <<"msg">> => Payload}, Domain);
        _ ->
            pass
    end.

%% @doc 保存设备日志 - 通过产品和设备地址
-spec save_log(ProductId :: binary(), DevAddr :: binary(), Data :: term(), Domain :: binary()) -> ok.
save_log(ProductId, DevAddr, Data, Domain) ->
    dgiot_parsex:create_object(<<"Log">>, #{
        <<"deviceid">> => dgiot_parse_id:get_deviceid(ProductId, DevAddr),
        <<"productid">> => ProductId,
        <<"msg">> => dgiot_json:encode(#{<<"data">> => Data}),
        <<"domain">> => [Domain],
        <<"devaddr">> => DevAddr,
        <<"time">> => dgiot_datetime:nowstamp() * 1000
    }).

%% @doc 获取设备URL
-spec get_url(AppName :: binary()) -> binary().
get_url(AppName) ->
    Roleid = dgiot_parse_id:get_roleid(AppName),
    case dgiot_parsex:get_object(<<"_Role">>, Roleid) of
        {ok, #{<<"tag">> := #{<<"appconfig">> := #{<<"file">> := Url}}}} ->
            <<Url/binary>>;
        _ -> <<"">>
    end.

%% @doc 记录设备操作日志
-spec log_device_operation(DeviceId :: binary(), Operation :: binary(), Details :: map(), User :: binary()) -> ok.
log_device_operation(DeviceId, Operation, Details, User) ->
    Timestamp = dgiot_datetime:nowstamp() * 1000,
    
    LogEntry = #{
        <<"deviceid">> => DeviceId,
        <<"operation">> => Operation,
        <<"details">> => Details,
        <<"user">> => User,
        <<"timestamp">> => Timestamp
    },
    
    case dgiot_parsex:create_object(<<"DeviceLog">>, LogEntry) of
        {ok, _} -> ok;
        {error, Reason} -> 
            ?LOG(error, "Failed to save device operation log: ~p", [Reason]),
            ok
    end.
