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

-module(dgiot_device_echart).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_tdengine/include/dgiot_tdengine.hrl").

-export([get_echart_data/4]).
-export([get_data_by_month/4,get_data_by_echart_category/4, get_keys/2,get_table/2]).

get_echart_data(Channel, ProductId, DeviceId, Args) ->
    Query = maps:without([<<"productid">>, <<"deviceid">>], Args),
    case dgiot_data:get({tdengine_os, Channel}) of
        <<"windows">> ->
            pass;
        _ ->
            Query = maps:without([<<"productid">>, <<"deviceid">>], Args),
            Interval = maps:get(<<"interval">>, Args),
            TableName = ?Table(DeviceId),
            case dgiot_device_tdengine:get_history_data(Channel, TableName, Query#{
                <<"db">> => ProductId
            }) of
                {Names, {ok, #{<<"results">> := Results}}} ->
                    Chartdata = get_echart(ProductId, Results, Names, Interval),
                    {ok, #{<<"chartData">> => Chartdata}};
                _ ->
                    {ok, #{<<"code">> => 400, <<"msg">> => <<"no data">>}}
            end
    end.

get_echart(ProductId, Results, Names, Interval) ->
    Maps = dgiot_product:get_prop(ProductId),
    Units = dgiot_product:get_unit(ProductId),
    NewMaps = maps:merge(#{<<"createdat">> => <<"日期"/utf8>>}, Maps),
    Columns = [<<"日期"/utf8>>] ++ Names,
    Rows =
        lists:foldl(fun(Line, Lines) ->
            NewLine =
                maps:fold(fun(K, V, Acc) ->
                    case maps:find(K, NewMaps) of
                        error ->
                            Acc;
                        {ok, Name} ->
                            case Name of
                                <<"日期"/utf8>> ->
                                    NewV = dgiot_tdengine_field:get_time(V, Interval),
                                    Acc#{Name => NewV};
                                _ ->
                                    Acc#{Name => V}
                            end
                    end
                          end, #{}, Line),
            Lines ++ [NewLine]
                    end, [], Results),
    ?LOG(debug, "Rows ~p", [Rows]),
    ChildRows = lists:foldl(fun(X, Acc1) ->
        Date = maps:get(<<"日期"/utf8>>, X),
        maps:fold(fun(K1, V1, Acc) ->
            case maps:find(K1, Acc) of
                error ->
                    Acc#{K1 => [#{<<"日期"/utf8>> => Date, K1 => V1}]};
                {ok, V2} ->
                    Acc#{K1 => V2 ++ [#{<<"日期"/utf8>> => Date, K1 => V1}]}
            end
                  end, Acc1, maps:without([<<"日期"/utf8>>], X))
                            end, #{}, Rows),
    ?LOG(debug, "ChildRows ~p", [ChildRows]),
    Child =
        maps:fold(fun(K, V, Acc) ->
            Unit =
                case maps:find(K, Units) of
                    error -> <<"">>;
                    {ok, Unit1} -> Unit1
                end,
            Acc ++ [#{<<"columns">> => [<<"日期"/utf8>>, K], <<"rows">> => V, <<"unit">> => Unit}]
                  end, [], ChildRows),
    ?LOG(debug, "Child ~p", [Child]),
    #{<<"columns">> => Columns, <<"rows">> => Rows, <<"child">> => Child}.


%%判断目标keys是否累计并根据结果设置Function
%% 遍历产品keys和目标keys得到目标keys是否累计
%%根据是否累计将keys分到sum和last
%%针对sum和last分别配置参数并调用history函数
%%调用history
%%get_data_by_month(Channel, ProductId, DeviceId,Args)
get_data_by_month(Channel, ProductId, DeviceId, Args) ->
%%    io:format("~s ~p Channel = ~p , ProductId = ~p, DeviceId = ~p    ~n",[?FILE,?LINE,Channel, ProductId, DeviceId]),
    case dgiot_data:get({tdengine_os, Channel}) of
        <<"windows">> ->
            pass;
        _ ->
%%            io:format("~s ~p here ~n",[?FILE,?LINE]),
%%           由月份获得起止时间
            {ok,Count} = maps:find(<<"month_count">>,Args),
            {StartTime, EndTime} = dgiot_datetime:last_month(Count),
%%            取得key并分割转为list
            {ok, K}  = maps:find(<<"keys">>,Args),
            Keys = re:split(K,","),
%%            由key提取其accu属性并生成sql命令
            Res = case dgiot_product:lookup_prod(ProductId) of
                        {ok, Product} ->
%%                            io:format("~s ~p Product =~p , Keys = ~p ~n",[?FILE,?LINE,Product,Keys]),
                            get_keys(Product, Keys);
                        _ ->
                            error
                    end,
%%            io:format("~s ~p Res = ~p~n",[?FILE,?LINE,Res]),
            {ok,Sql} = maps:find(<<"sql">>,Res),
            {ok,Names} = maps:find(<<"names">>,Res),
%%            配置参数
            TableName = ?Table(DeviceId),
            Interval = <<"1d">>,
            %%传入参数获得结果
            case dgiot_device_tdengine:get_history_data2(Sql, Channel, TableName, Interval, ProductId, StartTime, EndTime) of
%%                判断结果并转换格式
                {ok, #{<<"results">> := Results}} ->
%%                    io:format("~s ~p Results = ~p ~n",[?FILE,?LINE,Results]),
                    Tabledata = get_table(Results,Names),
%%                    io:format("~s ~p Tabledata = ~p ~n",[?FILE,?LINE,Tabledata]),
                    {ok,Tabledata};
                _ ->
                    {ok, #{<<"code">> => 400, <<"msg">> => <<"no data">>}}
            end


    end.

get_keys(#{<<"thing">> := #{<<"properties">> := Properties}}, [<<"*">>]) ->
    lists:foldl(fun(X, Acc) ->
%%        io:format("~s ~p Acc = ~p ~n",[?FILE,?LINE,Acc]),
        case X of
            #{<<"identifier">> := Identifier, <<"isaccumulate">> := true,<<"name">> := Name,<<"identifier">> := Key} ->
                case Acc of
                    #{<<"sql">> := Sql,<<"names">> :=Names } ->
%%                        io:format("~s ~p Key = ~p ~n",[?FILE,?LINE,Key]),
                        #{<<"sql">> => <<Sql/binary, ", sum(", Identifier/binary, ")">>,<<"names">> => Names#{<<Key/binary>> => <<Name/binary>>}};
                    _ ->
%%                        io:format("~s ~p Key = ~p ~n",[?FILE,?LINE,Key]),
                        #{<<"sql">> => <<" sum(", Identifier/binary, ")">>,<<"names">> => #{<<Key/binary>> => Name/binary}}
                end;
            #{<<"identifier">> := Identifier, <<"isaccumulate">> := false,<<"name">> := Name,<<"identifier">> := Key} ->
                case Acc of
                    #{<<"sql">> := Sql,<<"names">> :=Names } ->
%%                        io:format("~s ~p Key = ~p ~n",[?FILE,?LINE,Key]),
                        #{<<"sql">> => <<Sql/binary, ", last(", Identifier/binary, ")">>,<<"names">> => Names#{<<Key/binary>> => <<Name/binary>>}};
                    _ ->
%%                        io:format("~s ~p Key = ~p ~n",[?FILE,?LINE,Key]),
%%                        io:format("~s ~p Name = ~p ~n",[?FILE,?LINE,Name]),
                        #{<<"sql">> => <<" last(", Identifier/binary, ")">>,<<"names">> => #{<<Key/binary>> => <<Name/binary>>}}
                end
        end
                end, #{}, Properties);

get_keys(#{<<"thing">> := #{<<"properties">> := Properties}}, Keys) ->
    lists:foldl(fun(X, Acc) ->
        case X of
            #{<<"identifier">> := Identifier, <<"isaccumulate">> := true, <<"name">> := Name, <<"identifier">> := Key} ->
                case lists:member(Identifier, Keys) of
                    true ->
                        case Acc of
                            #{<<"sql">> := Sql,<<"names">> :=Names } ->
                                {ok, Sql} = maps:find(<<"sql">>, Acc),
                                {ok, Names} = maps:find(<<"names">>, Acc),
                                #{<<"sql">> => <<Sql/binary, ", sum(", Identifier/binary, ")">>, <<"names">> => Names#{<<Key/binary>> => <<Name/binary>>}};
                            _ ->
                                #{<<"sql">> => <<" sum(", Identifier/binary, ")">>, <<"names">> => #{<<Key/binary>> => <<Name/binary>>}}
                        end;
                    false ->
                        Acc
                end;

            #{<<"identifier">> := Identifier, <<"isaccumulate">> := false, <<"name">> := Name, <<"identifier">> := Key} ->

                case lists:member(Identifier, Keys) of
                    true ->
                        case Acc of
                            #{<<"sql">> := Sql,<<"names">> :=Names } ->
                                {ok, Sql} = maps:find(<<"sql">>, Acc),
                                {ok, Names} = maps:find(<<"names">>, Acc),
                                #{<<"sql">> => <<Sql/binary, " last(", Identifier/binary, ")">>, <<"names">> => Names#{<<Key/binary>> => <<Name/binary>>}};
                            _ ->

                                #{<<"sql">> => <<" last(", Identifier/binary, ")">>, <<"names">> => #{<<Key/binary>> => <<Name/binary>>}}
                        end;
                    false -> Acc
                end

        end
                end, #{}, Properties).

get_table(Results,Names) ->
    Count = string:len(Results),

    TableData = lists:foldl(fun(X, Acc) ->
        Res = maps:fold(fun(K, V, Init) ->
            case K of
                <<"createdat">> ->
                    Init#{<<"时间"/utf8>> => V};
                _ ->
                    case binary:match(K, <<"last">>) of
                        {0, 4} ->
                            Last_Key = binary:part(K, 5, byte_size(K) - 6),
                            case maps:find(<<Last_Key/binary>>, Names) of
                                {ok, Name} ->
                                    Init#{<<Name/binary>> => V};
                                error ->
                                    Init#{<<K/binary>> => V}
                            end;
                        _ ->
                            case binary:match(K, <<"sum">>) of
                                {0, 3} ->
                                    Sum_Key = binary:part(K, 4, byte_size(K) - 5),
                                    case maps:find(<<Sum_Key/binary>>, Names) of
                                        {ok, Name1} ->
                                            Init#{<<Name1/binary>> => V};
                                        error ->
                                            Init#{<<K/binary>> => V}
                                    end;
                                _ ->
                                    Init#{<<K/binary>> => V}
                            end
                    end
            end
                        end, #{}, X),
        Acc ++ [Res]
                            end, [], Results),
    #{<<"status">> => 0, <<"msg">> => <<"ok">>, <<"data">> => #{<<"counts">> => Count ,<<"rows">> => TableData}}.


get_data_by_echart_category(Channel, ProductId, DeviceId, Args) ->
    %%    io:format("~s ~p Channel = ~p , ProductId = ~p, DeviceId = ~p    ~n",[?FILE,?LINE,Channel, ProductId, DeviceId]),
    case dgiot_data:get({tdengine_os, Channel}) of
        <<"windows">> ->
            pass;
        _ ->
%%            io:format("~s ~p here ~n",[?FILE,?LINE]),
%%           由月份获得起止时间
            {ok,Count} = maps:find(<<"month_count">>,Args),
            {StartTime, EndTime} = dgiot_datetime:last_month(Count),
%%            取得key并分割转为list
            {ok, K}  = maps:find(<<"keys">>,Args),
            Keys = re:split(K,","),
%%            由key提取其accu属性并生成sql命令
            Res = case dgiot_product:lookup_prod(ProductId) of
                      {ok, Product} ->
%%                            io:format("~s ~p Product =~p , Keys = ~p ~n",[?FILE,?LINE,Product,Keys]),
                          get_keys(Product, Keys);
                      _ ->
                          error
                  end,
%%            io:format("~s ~p Res = ~p~n",[?FILE,?LINE,Res]),
            {ok,Sql} = maps:find(<<"sql">>,Res),
            {ok,Names} = maps:find(<<"names">>,Res),
%%            配置参数
            TableName = ?Table(DeviceId),
            Interval = <<"1d">>,
            %%传入参数获得结果
            case dgiot_device_tdengine:get_history_data2(Sql, Channel, TableName, Interval, ProductId, StartTime, EndTime) of
%%                判断结果并转换格式
                {ok, #{<<"results">> := Results}} ->
%%                    io:format("~s ~p Results = ~p ~n",[?FILE,?LINE,Results]),
                    Tabledata = get_table(Results,Names),
%%                    io:format("~s ~p Tabledata = ~p ~n",[?FILE,?LINE,Tabledata]),
                    {ok,Tabledata};
                _ ->
                    {ok, #{<<"code">> => 400, <<"msg">> => <<"no data">>}}
            end


    end.
