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

-module(dgiot_device_card).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_tdengine/include/dgiot_tdengine.hrl").

-export([get_devcard/5, get_card/5, get_device_card/4]).

get_devcard(Channel, ProductId, DeviceId, Devaddr, Args) ->
    Chartdata =
        case dgiot_product:get_sub_tab(ProductId) of
            not_find ->
                get_device_card(Channel, ProductId, DeviceId, Args);
            Subs when length(Subs) > 1 ->
                TableNames =
                    lists:foldl(fun(SubId, Acc) ->
                        SubDevid = dgiot_parse_id:get_deviceid(SubId, Devaddr),
                        Acc ++ [SubDevid]
                                end, [], Subs),
                get_device_card(Channel, ProductId, TableNames, Args);
            _ ->
                get_device_card(Channel, ProductId, DeviceId, Args)
        end,
    {ok, #{<<"data">> => Chartdata}}.

get_device_card(Channel, ProductId, DeviceId, Args) ->
    Results =
        case dgiot_device_tdengine:get_realtime_data(Channel, ProductId, DeviceId, Args) of
            {ok, #{<<"results">> := TdResults}} when length(TdResults) > 0 ->
                TdResults;
            _ ->
                [#{}]
        end,
    get_card(ProductId, Results, DeviceId, Args, dgiot_data:get({shard_storage, ProductId})).


decode_shard_data(Data, Result) ->
    case binary:split(Data, <<$,>>, [global, trim]) of
        List when length(List) > 0 ->
            lists:foldl(fun(<<Len:1/binary, Rest/binary>>, Acc) ->
                IntLen = dgiot_utils:to_int(Len),
                case Rest of
                    <<Key:IntLen/binary, Value/binary>> ->
                        Acc#{Key => Value};
                    _ ->
                        Acc
                end
                        end, Result, List);
        _ ->
            Result
    end.

%% 分片存储 dgiot_data:get({shard_storage, <<"857ed41119">>}).
get_card(ProductId, Results, DeviceId, Args, true) ->
    [Result | _] = Results,
    Createdat = maps:get(<<"createdat">>, Result, dgiot_datetime:now_secs()),
    Buff =
        lists:foldl(fun(Index, Acc) ->
            BinIndex = dgiot_utils:to_binary(Index),
            case maps:find(<<"shard_", BinIndex/binary>>, Result) of
                {ok, Value} when Value =/= null ->
                    <<Acc/binary, Value/binary>>;
                _ ->
                    Acc
            end
                    end, <<>>, lists:seq(1, maps:size(Result))),
    NewResult = dgiot_dlink_proctol:parse_payload(ProductId, decode_shard_data(Buff, Result)),
%%    io:format("success NewResult = ~ts~n", [unicode:characters_to_list(dgiot_json:encode(NewResult))]),
    get_card(ProductId, [dgiot_map:merge(NewResult#{<<"createdat">> => Createdat}, Result)], DeviceId, Args, false);

get_card(ProductId, Results, DeviceId, Args, _) ->
    [Result | _] = Results,
    Keys = maps:get(<<"keys">>, Args, <<"*">>),
    Props = dgiot_product:get_props(ProductId, Keys),
    Time = maps:get(<<"createdat">>, Result, dgiot_datetime:now_secs()),
    NewTime = dgiot_tdengine_field:get_time(dgiot_utils:to_binary(Time), <<"111">>),
    lists:foldl(fun(X, Acc) ->
        case X of
            #{<<"dataSource">> := #{<<"api">> := _}, <<"name">> := Name, <<"identifier">> := Identifier, <<"dataForm">> := #{<<"protocol">> := Protocol}, <<"dataType">> := #{<<"type">> := Typea} = DataType} ->
                DataSource = maps:get(<<"dataSource">>, X, #{}),
                Devicetype =
                    case maps:find(<<"devicetype">>, X) of
                        {ok, <<"">>} ->
                            <<"others">>;
                        {ok, Data} when byte_size(Data) > 0 ->
                            Data;
                        _ ->
                            <<"others">>
                    end,
                Ico = maps:get(<<"ico">>, X, <<"">>),
                Specs = maps:get(<<"specs">>, DataType, #{}),
                Unit = maps:get(<<"unit">>, Specs, <<"">>),
%%                {Color, _, _} = dgiot_device:get_color(DeviceId, Identifier),
                case do_hook({Protocol, Identifier}, DataSource#{<<"deviceid">> => DeviceId}) of
                    ignore ->
                        {Value, NewV} =
                            case maps:find(Identifier, Result) of
                                error ->
                                    {<<>>, <<"--">>};
                                {ok, V} ->
                                    {V, dgiot_product_tdengine:check_field(Typea, V, #{<<"datatype">> => DataType, <<"specs">> => Specs, <<"deviceid">> => DeviceId})}
                            end,
                        Acc ++ [#{<<"identifier">> => Identifier, <<"name">> => Name,
                            <<"type">> => Typea, <<"number">> => NewV, <<"value">> => Value,
                            <<"time">> => NewTime, <<"unit">> => Unit,
                            <<"imgurl">> => Ico, <<"devicetype">> => Devicetype}];
                    {error, _Reason} ->
                        Acc ++ [#{<<"identifier">> => Identifier, <<"name">> => Name,
                            <<"type">> => Typea, <<"number">> => <<"--">>,
                            <<"time">> => NewTime, <<"unit">> => Unit,
                            <<"imgurl">> => Ico, <<"devicetype">> => Devicetype}];
                    V ->
                        NewV = dgiot_product_tdengine:check_field(Typea, V, #{<<"datatype">> => DataType, <<"specs">> => Specs, <<"deviceid">> => DeviceId}),
                        Acc ++ [#{<<"identifier">> => Identifier, <<"name">> => Name,
                            <<"type">> => Typea, <<"number">> => NewV,
                            <<"time">> => NewTime, <<"unit">> => Unit,
                            <<"imgurl">> => Ico, <<"devicetype">> => Devicetype}]
                end;
            #{<<"name">> := Name, <<"identifier">> := Identifier, <<"dataType">> := #{<<"type">> := Typea} = DataType} ->
                Devicetype =
                    case maps:find(<<"devicetype">>, X) of
                        {ok, <<"">>} ->
                            <<"others">>;
                        {ok, Data} when byte_size(Data) > 0 ->
                            Data;
                        _ ->
                            <<"others">>
                    end,
                Ico = maps:get(<<"ico">>, X, <<"">>),
                Specs = maps:get(<<"specs">>, DataType, #{}),
                Unit = maps:get(<<"unit">>, Specs, <<"">>),
%%                {Color, _, _} = dgiot_device:get_color(DeviceId, Identifier),
                {Value, Number} =
                    case maps:find(Identifier, Result) of
                        {ok, null} ->
                            {<<>>, <<"--">>};
                        {ok, V} ->
                            {V, dgiot_product_tdengine:check_field(Typea, V, #{<<"datatype">> => DataType, <<"specs">> => Specs, <<"deviceid">> => DeviceId})};
                        _ ->
                            {<<>>, <<"--">>}
                    end,
                Acc ++ [#{<<"identifier">> => Identifier, <<"name">> => Name,
                    <<"type">> => Typea, <<"number">> => Number, <<"value">> => Value,
                    <<"time">> => NewTime, <<"unit">> => Unit,
                    <<"imgurl">> => Ico, <<"devicetype">> => Devicetype}];
            _ ->
                Acc
        end
                end, [], Props).

do_hook(Key, Args) ->
    case catch dgiot_hook:run_hook(Key, Args) of
        {'EXIT', Reason} ->
            {error, Reason};
        {error, not_find} ->
            ignore;
        {ok, []} ->
            ignore;
        {ok, [{error, Reason} | _]} ->
            {error, Reason};
        {ok, [Rtn | _]} ->
            Rtn
    end.




