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

-module(dgiot_factory_data).
-author("jonhl").
-include_lib("dgiot/include/logger.hrl").
-define(PRE, <<"_">>).
-define(Database(Name), <<?PRE/binary, Name/binary>>).
-define(Table(Name), <<?PRE/binary, Name/binary>>).
-define(SHEETID(SHEET), <<SHEET/binary, "_id">>).
-define(PRODUCTID, <<"ec71804a3d">>).
-define(TYPE, <<"TD">>).

-export([handel_data/1, save_data/1, get_work_sheet/7]).
-export([update_progress/0, get_ThingMap/2, thinglist2binary/1, get_history/8, get_device_list/0, add_data/4]).
handel_data([ProductId, DeviceId, Type, FlatMap]) ->
%%EnumData = case dgiot_factory_data:get_ThingMap(Type, ProductId) of
%%               {ok, ThingMap} ->
%%                   maps:fold(
%%                       fun(K, V, Acc) ->
%%                           case V of
%%                               <<"enum">> ->
%%                                   case maps:find(K, Acc) of
%%                                       {ok, Data} ->
%%                                           case dgiot_factory_utils:get_num(K, Data) of
%%                                               error ->
%%                                                   Acc;
%%                                               Map ->
%%                                                   maps:merge(Acc, Map)
%%                                           end;
%%                                       _ ->
%%                                           Acc
%%                                   end;
%%                               _ ->
%%                                   Acc
%%                           end
%%
%%                       end, FlatMap, ThingMap);
%%               _ ->
%%                   FlatMap
%%           end,
    save_data([ProductId, DeviceId, Type, FlatMap]).

save_data([ProductId, DeviceId, <<"product">> = Type, Payload]) ->
    case dgiot_data:lookup({ProductId, ?TYPE}) of
        {ok, Channel} ->
            case dgiot_device_cache:lookup(DeviceId) of
                {ok, #{<<"devaddr">> := DevAddr}} ->
                    handle_product_condition(Channel, ProductId, DeviceId, DevAddr, Type, Payload);

                _ ->
                    {error, not_find_toal_num}
            end;
        _ ->
            {error, not_find_channel}
    end;


save_data([ProductId, DeviceId, <<"semiproduct">> = Type, Payload]) ->
%%    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
%%        {ok, #{<<"devaddr">> := DevAddr}} ->
    case dgiot_device_cache:lookup(DeviceId) of
        {ok, #{<<"devaddr">> := DevAddr}} ->
            Id = get_id(DevAddr, Type),
            dgiot_task:save_td_no_match(ProductId, DevAddr, Payload#{?SHEETID(Type) => Id}, #{});

        _ ->
            {error, <<"not_fin_device">>}
    end;

save_data([ProductId, DeviceId, <<"quality">> = Type, Payload]) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"devaddr">> := DevAddr}} ->
            Id = get_id(DevAddr, Type),

            dgiot_task:save_td_no_match(ProductId, DevAddr, Payload#{?SHEETID(Type) => Id}, #{}),
            handle_quality(Payload, DeviceId);
        _ ->
            {error, <<"not_find_device">>}
    end;

save_data([ProductId, DeviceId, <<"material">>, Payload]) ->
    case handle_material(DeviceId, Payload) of
        {ok, Res} ->
            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"devaddr">> := DevAddr}} ->
                    dgiot_task:save_td_no_match(ProductId, DevAddr, Res, #{});

                _ ->
                    {error, <<"not_find_device">>}
            end;
        _ ->
            error
    end;

save_data(_) ->
    {error, error}.


handle_product_condition(_Channel, ProductId, DeviceId, DevAddr, Type, #{<<"product_pnumber">> := Pnumber, <<"product_subtime">> := SubTime, <<"product_condition">> := 1} = Payload) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"startTime">> := LastSubTime, <<"progress">> := Progress, <<"content">> := #{<<"baseInfo">> := #{<<"Number">> := Total}}}} ->
            WorkTime = (SubTime - LastSubTime) div 1000,
            Id = get_id(DevAddr, Type),
            UpId = string:to_upper(dgiot_utils:to_list(Id)),
            io:format("~s ~pPayload = ~p ~n", [?FILE, ?LINE, Payload]),
            dgiot_task:save_td_no_match(ProductId, DevAddr, Payload#{<<"product_mhour">> => abs(WorkTime), <<"product_id">> => UpId}, #{}),

            save_progress(DeviceId, Progress, Total, Pnumber, SubTime);

        _ ->
            error
    end;
handle_product_condition(_Channel, ProductId, DeviceId, DevAddr, _Type, #{<<"product_condition">> := 2, <<"product_id">> := Product_id, <<"product_pnumber">> := Pnumber} = Payload) ->
    dgiot_task:save_td_no_match(ProductId, DevAddr, Payload#{<<"product_id">> => dgiot_utils:to_list(Product_id)}, #{}),

    handle_storehouse(2, Pnumber, DeviceId);

handle_product_condition(_Channel, ProductId, DeviceId, DevAddr, _Type, #{<<"product_condition">> := 3, <<"product_id">> := Product_id, <<"product_pnumber">> := Pnumber} = Payload) ->
    dgiot_task:save_td_no_match(ProductId, DevAddr, Payload#{<<"product_id">> => dgiot_utils:to_list(Product_id)}, #{}),

    handle_storehouse(3, Pnumber, DeviceId),
%%            handle_dingdan(DeviceId),
    dgiot_factory_meter:test(Payload, DeviceId);

handle_product_condition(_, _, _, _, _, _) ->
    error.


handle_material(DeviceId, #{<<"material_type">> := <<"picking">>, <<"material_name">> := MaterialName, <<"material_date">> := Date} = Payload) ->
    Id = get_id(DeviceId, [MaterialName, Date]),
    {ok, Payload#{<<"material_id">> => Id}};

%%    case maps:find(<<"material_name">>, dgiot_factory_utils:get_num(<<"material_name">>, MaterialName)) of
%%        {ok, Num} ->
%%            {ok, Payload#{<<"material_id">> => Id, <<"material_name">> := Num}};
%%        _ ->
%%            error
%%    end;

handle_material(DeviceId, #{<<"material_type">> := <<"retriving">>, <<"material_name">> := MaterialName, <<"material_date">> := Date, <<"material_loss">> := Loss} = Payload) ->
    case maps:find(<<"persion_sessiontoken">>, Payload) of
        {ok, Token} ->
            case dgiot_product_tdengine:get_channel(Token) of
                {ok, Channel} ->
                    case get_work_sheet(<<"material">>, Channel, DeviceId, #{<<"material_name">> => MaterialName, <<"material_date">> => Date}, 1, 0, <<"true">>) of
                        {ok, {_, Res}} ->
                            case length(Res) of
                                1 ->
                                    F = lists:nth(1, Res),
                                    {ok, F#{<<"material_loss">> => Loss}};
                                _ ->
                                    error
                            end;
                        _ ->
                            error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end;
handle_material(_, _) ->
    io:format("~s ~p here~n", [?FILE, ?LINE]),
    error.


%%handle_dingdan(DeviceId) ->
%%    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
%%        {ok, #{<<"progress">> := Progress, <<"detail">> := Detail, <<"content">> := #{<<"baseInfo">> := #{<<"Number">> := Total}}}} ->
%%            case Progress >= Total of
%%                true ->
%%                    EndTime = dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"),
%%                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"realstate">> => 8, <<"detail">> => Detail#{<<"taskend">> => EndTime}});
%%                _ ->
%%                    pass
%%            end
%%    end.


handle_storehouse(2, Pnumber, DeviceId) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"storehouse">> := #{<<"unstored">> := Unstored, <<"stored">> := Stored}}} ->
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"storehouse">> => #{<<"unstored">> => Unstored + Pnumber, <<"stored">> => Stored}});
        _ ->
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"storehouse">> => #{<<"unstored">> => Pnumber, <<"stored">> => 0}})

    end;

handle_storehouse(3, Pnumber, DeviceId) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"storehouse">> := #{<<"unstored">> := Unstored, <<"stored">> := Stored}}} ->
            case Unstored - Pnumber < 0 of
                true ->
                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"storehouse">> => #{<<"unstored">> => 0, <<"stored">> => Stored + Pnumber}});
                _ ->
                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"storehouse">> => #{<<"unstored">> => Unstored - Pnumber, <<"stored">> => Stored + Pnumber}})
            end;
        _ ->
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"storehouse">> => #{<<"unstored">> => 0, <<"stored">> => 1}})
    end;

handle_storehouse(_, _, _) ->
    pass.



get_id(DevAddr, Type) ->
    Time = dgiot_utils:to_binary(dgiot_datetime:timestamp()),
    Bin = dgiot_utils:to_binary(Type),
    <<ObjID:10/binary, _/binary>> = dgiot_utils:to_md5(<<Bin/binary, DevAddr/binary, Time/binary>>),
    Res = string:to_upper(dgiot_utils:to_list(ObjID)),
    dgiot_utils:to_binary(Res).




handle_quality(#{<<"quality_people">> := Operator, <<"quality_status">> := Status, <<"quality_quality">> := Quality, <<"quality_alarmid">> := NotificationId,
    <<"quality_pnumber">> := Pnumber}, DeviceId) ->
    case {Status, Quality} of
        {3, 1} ->
            handle_alert(NotificationId, Operator, Quality),
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"realstatus">> => 4});
        {3, 0} ->
            handle_alert(NotificationId, Operator, Quality),
            handle_progress(DeviceId, -Pnumber),
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"realstatus">> => 2});
        {5, 1} ->
            handle_alert(NotificationId, Operator, Quality),
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"realstatus">> => 6});
        {5, 0} ->
            handle_alert(NotificationId, Operator, Quality),
            handle_progress(DeviceId, -Pnumber),
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"realstatus">> => 2});
        {6, 1} ->
            handle_alert(NotificationId, Operator, Quality);
        _ ->
            {ok, ok}
    end;
handle_quality(_, _) ->
    error.


handle_progress(DeviceId, Pnumber) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"progress">> := Progress}} ->
            NewProgress = case Progress + Pnumber < 0 of
                              true ->
                                  0;
                              false ->
                                  Progress + Pnumber
                          end,
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"progress">> => NewProgress});
        _ ->
            pass
    end.

handle_alert(NotificationId, Operator, 1) ->
    case dgiot_parse:get_object(<<"Notification">>, NotificationId) of
        {ok, #{<<"content">> := #{<<"alarm">> := Alarm}}} ->
            dgiot_parse:update_object(<<"Notification">>, NotificationId, #{<<"status">> => 1, <<"content">> => #{<<"alarm">> => Alarm#{<<"operator">> => Operator}, <<"alertstatus">> => 2}});
        _ ->
            pass
    end;
handle_alert(NotificationId, Operator, 0) ->
    case dgiot_parse:get_object(<<"Notification">>, NotificationId) of
        {ok, #{<<"content">> := #{<<"alarm">> := Alarm}}} ->
            dgiot_parse:update_object(<<"Notification">>, NotificationId, #{<<"status">> => 1, <<"content">> => #{<<"alarm">> => Alarm#{<<"operator">> => Operator}}});
        _ ->
            pass
    end.

save_progress(DeviceId, Progress, Total, Pnumber, SubTime) ->
    NewProgress = case Progress + Pnumber > Total of
                      true ->
                          Total;
                      false ->
                          Progress + Pnumber
                  end,
    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"progress">> => NewProgress, <<"startTime">> => SubTime}).



get_work_sheet(Type, Channel, DeviceId, Where, Limit, Skip, New) ->
    case filter_where(Where, ?PRODUCTID, Type) of
        {Parse, Td, ThingMap} ->
            case search_parse(DeviceId, Parse, Type) of
                {ok, ParseData} ->
                    case get_history(Channel, ?PRODUCTID, DeviceId, ThingMap, Td, Limit, Skip, Type) of
                        {ok, #{<<"results">> := HistoryData}} ->
                            NewestHistoryData = case New of
                                                    <<"true">> ->
                                                        get_newest_data(HistoryData, Type, Td);
                                                    _ ->
                                                        HistoryData
                                                end,
                            {Total, Res} = filter_data(Limit, Skip, NewestHistoryData),
                            MergeData = merge_data(ParseData, Res, DeviceId, ThingMap),
                            {ok, {Total, MergeData}};
                        _ ->
                            error
                    end;
                _ ->
                    {ok, <<"nodata">>}
            end;
        _ ->
            {error, not_find_thing}
    end.

get_newest_data(HistoryData, Type, #{<<"product_condition">> := Condition}) ->
    [_, Res] = lists:foldl(
        fun(X, [AddrList, Acc]) ->
            case maps:find(?SHEETID(Type), X) of
                {ok, Id} ->
                    case lists:member(Id, AddrList) of
                        true ->
                            [AddrList, Acc];
                        _ ->
                            case maps:find(<<"product_condition">>, X) of
                                {ok, Condition} ->
                                    [AddrList ++ [Id], Acc ++ [X]];
                                _ ->
                                    [AddrList ++ [Id], Acc]
                            end
                    end;
                _ ->

                    [AddrList, Acc]
            end
        end, [[], []], HistoryData),
    Res;


get_newest_data(HistoryData, Type, _) ->
    [_, Res] = lists:foldl(
        fun(X, [AddrList, Acc]) ->
            case maps:find(?SHEETID(Type), X) of
                {ok, Id} ->
                    case lists:member(Id, AddrList) of
                        true ->
                            [AddrList, Acc];
                        _ ->
                            [AddrList ++ [Id], Acc ++ [X]]

                    end;
                _ ->

                    [AddrList, Acc]
            end
        end, [[], []], HistoryData),
    Res.

merge_data(ParseData, HistoryData, DeviceList, _ThingMap) when is_list(DeviceList) ->
%%    HistoryData = dgiot_factory_utils:turn_name(History, ThingMap),
    lists:foldl(
        fun(X, Acc) ->
            Addr = maps:get(<<"devaddr">>, X, <<"">>),
            case maps:find(Addr, ParseData) of
                {ok, Parse} ->
                    Acc ++ [maps:merge(Parse, X)];
                _ ->

                    Acc
            end
        end, [], HistoryData);

merge_data(ParseData, HistoryData, _, _ThingMap) ->
%%    HistoryData = dgiot_factory_utils:turn_name(History, ThingMap),
    lists:foldl(
        fun(X, Acc) ->
            Acc ++ [maps:merge(ParseData, X)]
        end, [], HistoryData).




search_parse(DeviceList, Parse, Type) when is_list(DeviceList) ->
    Res = lists:foldl(
        fun(X, Acc) ->
            maps:fold(
                fun(K, V, ACC) ->
                    case search_parse(K, Parse, Type) of
                        {ok, Res} ->
                            ACC#{V => Res};
                        _ ->
                            ACC
                    end
                end, Acc, X)
        end, #{}, DeviceList),
    {ok, Res};

search_parse(DeviceId, undefined, Type) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"content">> := #{Type := Data}}} ->

            FlatternMap = dgiot_map:flatten(#{Type => Data}),
            {ok, FlatternMap#{<<"objectId">> => DeviceId}};
        _ ->
            error
    end;

search_parse(DeviceId, Parse, Type) ->
    case maps:size(Parse) of
        0 ->
            search_parse(DeviceId, undefined, Type);
        Num ->

            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"content">> := Content}} ->
                    FlatMap = dgiot_map:flatten(Content),
                    MatchNum = maps:fold(
                        fun(K, V, Acc) ->

                            case maps:find(K, FlatMap) of
                                {ok, V} ->
                                    Acc + 1;
                                _ ->
                                    Acc
                            end

                        end, 0, Parse),
                    case MatchNum of
                        Num ->
                            Data = maps:get(<<Type/binary>>, Content),
                            FlatternMap = dgiot_map:flatten(#{Type => Data}),
                            {ok, FlatternMap#{<<"objectId">> => DeviceId}};
                        _ ->
                            error
                    end
            end
    end.

filter_where(undefined, ProductId, Type) ->
    case get_ThingMap(Type, ProductId) of
        {ok, ThingMap} ->
            {undefined, undefined, ThingMap};
        _ ->
            error
    end;
filter_where(Where, ProductId, Type) ->
    MapWhere = case is_map(Where) of
                   true ->
                       Where;
                   _ ->
                       jsx:decode(Where)
               end,
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := PropertiesList}}} ->
            {Parse, Td} = lists:foldl(
                fun(X, {Parse, Td}) ->
                    Identifier = maps:get(<<"identifier">>, X, <<"">>),
                    case lists:member(Identifier, maps:keys(MapWhere)) of
                        true ->
                            case X of
                                #{<<"isstorage">> := false, <<"devicetype">> := Type} ->

                                    {Parse#{Identifier => maps:get(Identifier, MapWhere)}, Td};
                                #{<<"isstorage">> := true, <<"devicetype">> := Type} ->
                                    {Parse, Td#{Identifier => maps:get(Identifier, MapWhere)}};
                                _ ->
                                    {Parse, Td}

                            end;
                        false ->
                            {Parse, Td}
                    end
                end, {#{}, #{}}, PropertiesList),
            TdWithPerson = case lists:member(<<"person">>, maps:keys(MapWhere)) of
                               true ->
                                   Td#{<<"person">> => maps:get(<<"person">>, MapWhere)};
                               false ->
                                   Td
                           end,
            case get_ThingMap(Type, ProductId) of
                {ok, ThingMap} ->
                    {Parse, TdWithPerson, ThingMap};
                _ ->
                    error
            end;


        _ ->
            error
    end.



get_ThingMap(Type, ProductId) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Properties}}} ->
            ThingList = lists:foldl(
                fun(X, Acc) ->
                    case X of
                        #{<<"devicetype">> := Type, <<"isstorage">> := true, <<"identifier">> := Identifier, <<"dataType">> := #{<<"type">> := DateType}} ->
                            Acc#{Identifier => DateType};
                        _ ->
                            Acc
                    end
                end, #{}, Properties),
            case maps:size(ThingList) of
                0 ->
                    {error, not_find_thing};
                _ ->
                    {ok, ThingList}
            end;
        _ ->
            {error, not_find_thing}
    end.

thinglist2binary(ThingList) ->
    Str = lists:foldl(
        fun(K, Acc) ->
            Acc ++ lists:foldl(
                fun(X, ACC) ->
                    ACC ++ " ," ++ dgiot_utils:to_list(X)
                end, [], K)
        end, [], ThingList),

    dgiot_utils:to_binary(lists:nthtail(2, Str ++ " , devaddr , createdat  ")).

get_history(Channel, ProductId, DeviceId, ThingMap, Where, _Limit, _Skip, Type) ->
    case dgiot_data:get({tdengine_os, Channel}) of
        <<"windows">> ->
            {error, wrong_td_platform};
        _ ->
            TableName = case is_list(DeviceId) of
                            true ->
                                ?Table(ProductId);
                            _ ->
                                ?Table(DeviceId)
                        end,
            ThingList = maps:keys(ThingMap),
            DetectThing = <<Type/binary, "_id">>,

            ColumnStr = case get_ThingMap(<<"person">>, ProductId) of
                            {ok, PersonMap} ->
                                PersonList = maps:keys(PersonMap),
                                thinglist2binary([ThingList, PersonList]);
                            _ ->
                                <<" * ">>
                        end,
            dgiot_tdengine:transaction(Channel,
                fun(Context) ->
                    Database = ProductId,
                    DB = dgiot_tdengine_select:format_db(?Database(Database)),
                    WHERE = get_where(Where, ThingMap, DetectThing, ProductId),
                    Order = <<" ORDER BY createdat DESC ">>,
                    Sql = <<"SELECT ", ColumnStr/binary, " FROM ", DB/binary, TableName/binary, WHERE/binary, Order/binary, ";">>,
                    ?LOG(error, "Sql ~s", [Sql]),
                    dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)
                end)
    end.


get_where(undefined, _, DetectThing, _) ->
    <<" where ", DetectThing/binary, " is not null ">>;

get_where(Where, ThingMap, DetectThing, ProductId) ->
    case is_map(Where) of
        true ->
            W = maps:fold(
                fun(K, V, Acc) ->
                    case K of
                        <<"person">> ->
                            case get_ThingMap(<<"person">>, ProductId) of
                                {ok, PersonMap} ->
                                    PersonList = maps:keys(PersonMap),
                                    Res = lists:foldl(
                                        fun(X, ACC) ->
                                            <<ACC/binary, "or ", X/binary, " like \"%", V/binary, "%\" ">>
                                        end, <<"">>, PersonList),
                                    FixedRes = binary:part(Res, 2, byte_size(Res) - 2),
                                    <<Acc/binary, "( ", FixedRes/binary, " ) and ">>;
                                _ ->
                                    Acc
                            end;
                        <<"product_condition">> ->
                            Acc;
                        _ ->
                            case maps:find(K, ThingMap) of
                                {ok, <<"text">>} ->
                                    <<Acc/binary, K/binary, " like \"%", V/binary, "%\" and ">>;
                                {ok, _} ->
                                    Bin = dgiot_utils:to_binary(V),
                                    <<Acc/binary, K/binary, " = ", Bin/binary, " and ">>;
                                _ ->
                                    Acc
                            end
                    end
                end, <<" ">>, maps:remove(<<"product">>, Where)),
            <<" where ", W/binary, DetectThing/binary, " is not null ">>;
        _ ->
            <<" where ", DetectThing/binary, " is not null ">>
    end.

filter_data(undefined, _, HistoryData) ->
    Total = length(HistoryData),
    {Total, HistoryData};

filter_data(Limit, undefined, HistoryData) ->
    filter_data(Limit, 0, HistoryData);

filter_data(Limit, Skip, HistoryData) ->
    Total = length(HistoryData),
    Res = case Limit + Skip > Total of
              true ->
                  lists:sublist(HistoryData, Skip + 1, Total);
              false ->
                  lists:sublist(HistoryData, Skip + 1, Limit + Skip)
          end,
    {Total, Res}.

update_progress() ->
    {ok, #{<<"results">> := List}} = dgiot_parse:query_object(<<"Device">>, #{}),
    lists:foldl(
        fun(X, _Acc) ->
            case X of
                #{<<"objectId">> := DeviceId} ->
                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"progress">> => 0})
            end
        end, [], List
    ).

get_device_list() ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"product">> => <<"ec71804a3d">>}}) of
        {ok, #{<<"results">> := Results}} ->
            Res = lists:foldl(
                fun(X, Acc) ->
                    case X of #{<<"devaddr">> := Addr, <<"objectId">> := DevId} ->
                        Acc ++ [#{DevId => ?Table(Addr)}];
                        _ ->
                            Acc
                    end
                end, [], Results),
            {ok, Res};
        _ ->
            error
    end.


add_data(Channel, ProductId, DevAddr, Data) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"thing">> := Properties}} ->
            case dgiot_tdengine:format_data(ProductId, DevAddr, Properties, Data) of
                #{<<"db">> := DB, <<"tableName">> := Table, <<"using">> := Using, <<"tags">> := Tags, <<"fields">> := _Fields, <<"values">> := Values} ->
                    io:format("~s ~p Values= ~ts ~n", [?FILE, ?LINE, Values]),
                    case dgiot_channelx:call(?TYPE, Channel, config) of
                        {ok, Context} ->
                            Tag = dgiot_utils:to_binary(Tags),
                            Sql = <<"INSERT INTO ", DB/binary, ".", Table/binary, " using ", Using/binary, ".", Using/binary, " TAGS (", Tag/binary, ") VALUES ", Values/binary, ";">>,
                            io:format("~s ~p Sql= ~ts ~n", [?FILE, ?LINE, Sql]),
%%                            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql);
                            Res = dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql),
                            io:format("~s ~p Res= ~p ~n", [?FILE, ?LINE, Res]),
                            Res;
                        _ ->
                            error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end.
