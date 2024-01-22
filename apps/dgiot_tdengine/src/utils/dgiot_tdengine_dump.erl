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

-module(dgiot_tdengine_dump).
-author("jonhl").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_tdengine.hrl").

-export([export/2, import/2]).


%% dgiot_tdengine:export().
export(ChannelId, #{<<"deviceid">> := DeviceId} = Body) ->
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"productid">> := ProductId}} ->
            export_device_data(ChannelId, #{<<"objectId">> => DeviceId, <<"product">> => #{<<"objectId">> => ProductId}}, Body, []);
        _ ->
            []
    end;

export(ChannelId, #{<<"sessionToken">> := SessionToken} = _Body) ->
%%    io:format("~s ~p 111Body = ~p.~n", [?FILE, ?LINE, Body]),
    Query = #{
        <<"keys">> => [<<"objectId">>, <<"product">>]
    },
    TdQuery = #{<<"limit">> => 10000, <<"function">> => <<"last">>, <<"interval">> => <<"1m">>},
    case dgiot_parse:query_object(<<"Device">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Data}} ->
            lists:foldl(fun(Device, Acc) ->
                export_device_data(ChannelId, Device, TdQuery, Acc)
                        end, [], Data);
        _ ->
            []
    end.

%% dgiot_tdengine:import().
import(ChannelId, Result) ->
    lists:foldl(fun({Name, Bin}, _Acc) ->
        case catch jsx:decode(Bin, [{labels, binary}, return_maps]) of
            {'EXIT', _} ->
                pass;
            Data ->
                case binary:split(dgiot_utils:to_binary(Name), <<$/>>, [global, trim]) of
                    [ProductId, <<DeviceId:10/binary, _/binary>>] ->
                        import_device_data(ChannelId, ProductId, DeviceId, Data);
                    _ ->
                        pass
                end
        end
                end, #{}, Result).

export_device_data(ChannelId, #{<<"objectId">> := DeviceId, <<"product">> := #{<<"objectId">> := ProductId}}, Query, NewData) ->
%%    io:format("~s ~p Query = ~p.~n", [?FILE, ?LINE, Query]),
    TableName = ?Table(DeviceId),
    case dgiot_device_tdengine:get_history_data(ChannelId, ProductId, TableName, Query) of
        {_TdNames, {ok, #{<<"results">> := TdResults}}} when length(TdResults) > 0 ->
            NewTdResults =
                lists:foldl(fun(Result, Acc) ->
                    Acc ++ [Result]
                            end, [], TdResults),
            NewData ++ [{dgiot_utils:to_list(<<ProductId/binary, "/", DeviceId/binary, ".json">>), unicode:characters_to_binary(dgiot_json:encode(#{<<"results">> => NewTdResults}))}];
        _ ->
            NewData
    end.

%% INSERT INTO _010e2df351._da06aff7f0 using _010e2df351._010e2df351 TAGS ('_844425383144878') VALUES  (now,null,6756.5,null,null,null);
import_device_data(ChannelId, ProductId, DeviceId, TdData) ->
    case TdData of
        #{<<"results">> := TdResults} ->
            dgiot_tdengine:transaction(ChannelId,
                fun(_Context) ->
                    case dgiot_device:lookup(DeviceId) of
                        {ok, #{<<"devaddr">> := DevAddr}} ->
                            lists:foldl(fun
                                            (#{<<"createdat">> := V} = Data, _Acc) ->
                                                NewV =
                                                    case binary:split(V, <<$.>>, [global, trim]) of
                                                        [NewV1, _] ->
                                                            NewV1;
                                                        _ ->
                                                            V
                                                    end,
                                                Createdat = dgiot_datetime:localtime_to_unixtime(dgiot_datetime:to_localtime(NewV)) * 1000,
                                                dgiot_tdengine_adapter:save(ProductId, DevAddr, Data#{<<"createdat">> => Createdat});
                                            (Data, _Acc) ->
                                                dgiot_tdengine_adapter:save(ProductId, DevAddr, Data)
                                        end, [], TdResults);
                        _ ->
                            pass
                    end
                end);
        _ ->
            pass
    end.
