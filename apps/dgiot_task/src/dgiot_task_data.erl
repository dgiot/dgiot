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
-export([get_userdata/7, get_datasource/2, get_ack/4]).

get_userdata(ProductId, Identifier, _DataForm, #{<<"type">> := <<"geopoint">>}, _DataSource, Payload, Acc) ->
    case maps:find(Identifier, Payload) of
        {ok, Value} ->
            Addr = dgiot_gps:get_gpsaddr(Value),
            dgiot_data:insert({topogps, dgiot_parse_id:get_shapeid(ProductId, Identifier)}, Addr),
            Acc#{Identifier => Value};
        _ ->
            dgiot_data:insert({topogps, dgiot_parse_id:get_shapeid(ProductId, Identifier)}, <<"无GPS信息"/utf8>>),
            Acc
    end;

get_userdata(_ProductId, Identifier, #{<<"collection">> := Collection}, DataType, DataSource, Payload, Acc) ->
    case maps:find(Identifier, Payload) of
        {ok, Value} ->
            calculate_value(Value, Collection, Identifier, DataType, Acc);
        _ ->
            Address = maps:get(<<"address">>, DataSource, <<"">>),
            case maps:find(Address, Payload) of
                {ok, Value} ->
                    NewAcc = maps:without([Address], Acc),
                    calculate_value(Value, Collection, Identifier, DataType, NewAcc);
                _ ->
%%                    电表
                    Di = maps:get(<<"di">>, DataSource, <<"">>),
                    case maps:find(Di, Payload) of
                        {ok, Value} ->
                            NewAcc = maps:without([Di], Acc),
                            calculate_value(Value, Collection, Identifier, DataType, NewAcc);
                        _ ->
                            Acc
                    end
            end
    end.

calculate_value(Value, Collection, Identifier, #{<<"type">> := Type, <<"specs">> := Specs}, Acc) ->
    Str = re:replace(Collection, dgiot_utils:to_list(<<"%%", Identifier/binary>>), "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
    Str1 = re:replace(Str, "%s", "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
    case dgiot_task:string2value(Str1, Type, Specs) of
        error ->
            maps:without([Identifier], Acc);
        Value1 ->
            Acc#{Identifier => Value1}
    end.

get_ack(ProductId, Payload, Dis, Ack) ->
    NewPayload =
        maps:fold(fun(K, V, Acc) ->
            case dgiot_data:get({protocol, K, ProductId}) of
                not_find ->
                    Acc#{K => V};
                Identifier ->
                    Acc#{Identifier => V}
            end
                  end, #{}, Payload),
    dgiot_task:get_collection(ProductId, Dis, NewPayload, maps:merge(Ack, NewPayload)).

get_datasource(Protocol, DataSource) ->
    case catch dgiot_hook:run_hook({datasource, Protocol}, DataSource) of
        {ok, [Rtn | _]} ->
            Rtn;
        _ ->
            DataSource
    end.
