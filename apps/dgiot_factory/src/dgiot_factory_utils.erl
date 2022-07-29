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

-module(dgiot_factory_utils).
-author("jonhl").
-include_lib("dgiot/include/logger.hrl").
-export([store_all/4]).
-export([get_num/2, get_name/2,turn_name/2]).


store_all(_, DeviceId, _, <<"false">>) ->
dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"realstatus">> => 2});


store_all(Channel, DeviceId, Operator, <<"true">>) ->
    case dgiot_factory_data:get_work_sheet(<<"product">>, Channel, DeviceId, undefined, undefined, 0, <<"true">>) of
        {ok, {_, Res}} ->
            lists:foldl(
                fun(X, _) ->
                    case maps:get(<<"product_condition">>, X) of
                        2 ->
                            dgiot_factory_data:handle_data(DeviceId, <<"product">>, X#{<<"product_condition">> := 3, <<"product_storedperson">> => Operator});
                        _ ->
                            pass
                    end
                end, [], Res),
            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"detail">> := Detail}} ->

                    EndTime = dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"),
                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"realstatus">> => 8, <<"detail">> => Detail#{<<"taskend">> => EndTime}});
                _ ->
                    error
            end;
        _ ->
            error
    end;

store_all(_, _, _, _) ->
error.


get_num(K, V) ->
Id = dgiot_parse_id:get_dictid(K, K, K, K),
case dgiot_parse:get_object(<<"Dict">>, Id) of
    {ok, #{<<"data">> := #{<<"end">> := End} = Dict}} ->
        case maps:find(V, Dict) of
            {ok, Num} ->
                #{K => Num};
            _ ->
                case dgiot_parse:update_object(<<"Dict">>, Id, #{<<"data">> => Dict#{<<"end">> => End + 1, V => End}}) of
                    {ok, _} ->
%%                            io:format("~s ~p Dict= ~p ~n", [?FILE, ?LINE, Dict]),

                        #{K => End};
                    _ ->
                        error
                end
        end;
    _ ->
        Map = #{<<"class">> => K,
            <<"title">> => K,
            <<"type">> => K,
            <<"key">> => K,
            <<"data">> => #{<<"end">> => 1, V => 0}
        },
        case dgiot_parse:create_object(<<"Dict">>, Map) of
            {ok, _} ->
                #{K => 0};
            _ ->
                error
        end

end.
get_name(K, Num) ->
Id = dgiot_parse_id:get_dictid(K, K, K, K),
case dgiot_parse:get_object(<<"Dict">>, Id) of
    {ok, #{<<"data">> := Dict}} ->
        TupleList = maps:to_list(Dict),
        case lists:keytake(Num, 2, TupleList) of
            {value, {Name, _}, _} ->
                #{K => Name};
            _ ->
                error
        end;
    _ ->
        error
end.

turn_name(Data, ThingMap) when is_list(Data) ->
maps:fold(
    fun(K, V, Acc) ->
        case V of
            <<"enum">> ->
                case maps:find(K, Acc) of
                    {ok, Num} ->
                        case get_name(K, Num) of
                            error ->
                                Acc;
                            Map ->
                                maps:merge(Acc, Map)
                        end;
                    _ ->
                        Acc
                end;
            _ ->
                Acc
        end
    end, Data, ThingMap).
