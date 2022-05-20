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

-module(dgiot_task_data).
-include("dgiot_task.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-export([get_userdata/6, get_datasource/2]).

get_userdata(ProductId, Identifier, _DataForm, #{<<"type">> := <<"geopoint">>}, Payload, Acc) ->
    case maps:find(Identifier, Payload) of
        {ok, Value} ->
            Addr = dgiot_gps:get_gpsaddr(Value),
            dgiot_data:insert({topogps, dgiot_parse_id:get_shapeid(ProductId, Identifier)}, Addr),
            Acc#{Identifier => Value};
        _ ->
            dgiot_data:insert({topogps, dgiot_parse_id:get_shapeid(ProductId, Identifier)}, <<"无GPS信息"/utf8>>),
            Acc
    end;

get_userdata(_ProductId, Identifier, #{<<"collection">> := Collection} = DataForm, #{<<"type">> := Type, <<"specs">> := Specs}, Payload, Acc) ->
    case maps:find(Identifier, Payload) of
        {ok, Value} ->
            Str = re:replace(Collection, dgiot_utils:to_list(<<"%%", Identifier/binary>>), "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
            Str1 = re:replace(Str, "%s", "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
            case dgiot_task:string2value(Str1, Type, Specs) of
                error ->
                    maps:without([Identifier], Acc);
                Value1 ->
                    Acc#{Identifier => Value1}
            end;
        _ ->
            Address = maps:get(<<"address">>, DataForm, <<"">>),
            case maps:find(Address, Payload) of
                {ok, Value} ->
                    Str = re:replace(Collection, dgiot_utils:to_list(<<"%%", Identifier/binary>>), "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
                    Str1 = re:replace(Str, "%s", "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
                    case dgiot_task:string2value(Str1, Type, Specs) of
                        error ->
                            maps:without([Identifier], Acc);
                        Value1 ->
                            Acc#{Identifier => Value1}
                    end;
                _ -> Acc
            end
    end.


get_datasource(Protocol, DataSource) ->
    case catch dgiot_hook:run_hook({datasource, Protocol}, DataSource) of
        {ok, [Rtn | _]} ->
            Rtn;
        _ ->
            DataSource
    end.
