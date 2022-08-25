%%%-------------------------------------------------------------------
%%% @author wolong
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 8月 2022 16:29
%%%-------------------------------------------------------------------
-module(dgiot_factory_getdata).
-author("wolong").
-include("dgiot_factory.hrl").
-define(PRE, <<"_">>).
-define(Database(Name), <<?PRE/binary, Name/binary>>).
-define(Table(Name), <<?PRE/binary, Name/binary>>).
-define(SHEETID(SHEET), <<SHEET/binary, "_id">>).
-define(TYPE, <<"TD">>).

%% API
-export([get_work_sheet/10]).
-export([get_ThingMap/2, thinglist2binary/1, get_history/9, get_device_list/1, get_example/2, filter_data/3]).
-export([get_all_sheet/9]).
-export([merge_data/4]).





get_work_sheet(ProductId, <<"person">>, Start, End, Channel, DeviceId, Where, Limit, Skip, New) ->
    get_all_sheet(ProductId, Start, End, Channel, DeviceId, Where, Limit, Skip, New);

get_work_sheet(ProductId, Type, Start, End, Channel, DeviceId, Where, Limit, Skip, New) ->
    case filter_where(Where, ProductId, Type) of
        {Parse, Td, ThingMap} ->
            case search_parse(DeviceId, Parse, Type) of
                {ok, ParseData} ->
                    case get_history(Channel, ProductId, DeviceId, ThingMap, Td, Start, End, Type, New) of
                        {ok, #{<<"results">> := HistoryData}} ->
                            {Total, Res} = filter_data(Limit, Skip, HistoryData),
                            MergeData = merge_data(ParseData, Res, DeviceId, ThingMap),
                            NamedData = dgiot_factory_utils:turn_name(MergeData, ThingMap),
                            {ok, {Total, NamedData}};
                        _ ->
                            error
                    end;
                _ ->
                    {ok, {0, []}}
            end;
        _ ->
            {error, not_find_thing}
    end.


merge_data(ParseData, HistoryData, DeviceList, _ThingMap) when is_list(DeviceList) ->
    lists:foldl(
        fun(X, Acc) ->
            DeviceId = maps:get(<<"person_deviceid">>, X, <<"">>),
            case maps:find(DeviceId, ParseData) of
                {ok, Parse} ->
                    Acc ++ [maps:merge(Parse, X)];
                _ ->
                    Acc
            end
        end, [], HistoryData);

merge_data(ParseData, HistoryData, _, _ThingMap) ->
    lists:foldl(
        fun(X, Acc) ->
            Acc ++ [maps:merge(ParseData, X)]
        end, [], HistoryData).


search_parse(DeviceList, Parse, Type) when is_list(DeviceList) ->
    Res = lists:foldl(
        fun(X, Acc) ->
            case search_parse(X, Parse, Type) of
                {ok, Res} ->
                    Acc#{X => Res};
                _ ->
                    Acc
            end
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
                       dgiot_factory_utils:turn_num(Where, ProductId, Type);
                   _ ->
                       dgiot_factory_utils:turn_num(jsx:decode(Where), ProductId, Type)
               end,
    case get_ThingMap(Type, ProductId) of
        {ok, ThingMap} ->
            {Parse, Td} = maps:fold(
                fun(K, V, {Parse, Td}) ->
                    case maps:is_key(K, ThingMap) of
                        true ->
                            {Parse, Td#{K => V}};
                        _ ->
                            {Parse#{K => V}, Td}

                    end
                end, {#{}, #{}}, MapWhere),

            TdWithPerson = case maps:is_key(<<"person">>, MapWhere) of
                               true ->
                                   Td#{<<"person">> => maps:get(<<"person">>, MapWhere)};
                               false ->
                                   Td
                           end,
            {Parse, TdWithPerson, ThingMap};
        _ ->
            error
    end.


get_ThingMap(Type, ProductId) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Properties}}} ->
            ThingList = lists:foldl(
                fun(X, Acc) ->
                    case X of
                        #{<<"devicetype">> := ?PERSON, <<"isstorage">> := true, <<"identifier">> := Identifier, <<"dataType">> := #{<<"type">> := DateType}} ->
                            Acc#{Identifier => DateType};
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
        fun(X, Acc) ->
            Acc ++ " ," ++ dgiot_utils:to_list(X)
        end, [], ThingList),
    dgiot_utils:to_binary(lists:nthtail(2, Str)).

get_history(Channel, ProductId, DeviceId, ThingMap, Where, Start, End, Type, New) ->
    case dgiot_data:get({tdengine_os, Channel}) of
        <<"windows">> ->
            {error, wrong_td_platform};
        _ ->
            DB = dgiot_tdengine_select:format_db(?Database(ProductId)),

            TableName = case is_list(DeviceId) of
                            true ->
                                ?Table(ProductId);
                            _ ->
                                ?Table(DeviceId)
                        end,
            List = case get_ThingMap(<<"person">>, ProductId) of
                       {ok, PersonMap} ->
                           PersonList = maps:keys(PersonMap),
                           ThingList = maps:keys(ThingMap),
                           PersonList ++ ThingList ++ [<<"createdat">>];
                       _ ->
                           maps:keys(ThingMap) ++ [<<"createdat">>]
                   end,

            dgiot_tdengine:transaction(Channel,
                fun(Context) ->
                    Select = thinglist2binary(List),
                    From = get_from(New, DB, TableName, Type, List),
                    WHERE = get_where(Where, ThingMap, Type, ProductId, Start, End),
                    Order = <<" ORDER BY createdat DESC ">>,
                    Sql = <<"SELECT ", Select/binary, " FROM ", From/binary, WHERE/binary, Order/binary, ";">>,
%%                    ?LOG(error, "Sql ~s", [Sql]),
                    dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)
                end)
    end.


get_where(undefined, _, Type, _, Start, End) ->
    Time = get_time(Start, End),
    DetectThing = ?SHEETID(Type),
    <<" where ", DetectThing/binary, " is not null ", Time/binary>>;

get_where(Where, ThingMap, Type, _ProductId, Start, End) ->
    DetectThing = ?SHEETID(Type),
    case is_map(Where) of
        true ->
            W = maps:fold(
                fun(K, V, Acc) ->
                    case K of
                        <<"person">> ->
                            P = <<Type/binary, "_people">>,
                            <<Acc/binary, P/binary, " like \"%", V/binary, "%\" and ">>;
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
            Time = get_time(Start, End),
            <<" where ", W/binary, DetectThing/binary, " is not null ", Time/binary>>;
        _ ->
            Time = get_time(Start, End),
            <<" where ", DetectThing/binary, " is not null ", Time/binary>>
    end.
get_time(undefined, undefined) ->
    <<" ">>;
get_time(undefined, End) ->
    BinEnd = dgiot_utils:to_binary(End * 1000),
    <<" and createdat < ", BinEnd/binary, " ">>;
get_time(Start, undefined) ->
    BinStart = dgiot_utils:to_binary(Start * 1000),
    <<" and createdat > ", BinStart/binary, " ">>;
get_time(Start, End) ->
    BinStart = dgiot_utils:to_binary(Start * 1000),
    BinEnd = dgiot_utils:to_binary(End * 1000),
    <<" and createdat > ", BinStart/binary, " and createdat < ", BinEnd/binary, " ">>.

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

get_device_list(ProductId) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"product">> => ProductId}}) of
        {ok, #{<<"results">> := Results}} ->
            Res = lists:foldl(
                fun(X, Acc) ->
                    case X of #{<<"objectId">> := DevId} ->
                        Acc ++ [DevId];
                        _ ->
                            Acc
                    end
                end, [], Results),
            {ok, Res};
        _ ->
            error
    end.




get_from(<<"true">>, DB, TableName, Type, List) ->
    Last = lists:foldl(
        fun(X, Acc) ->
            L = dgiot_utils:to_list(X),
            Acc ++ " ,last(" ++ L ++ " ) as " ++ L
        end, [], List),
    Select = dgiot_utils:to_binary(lists:nthtail(2, Last)),
    SheetId = ?SHEETID(Type),
    <<"( select ", Select/binary, " from ", DB/binary, TableName/binary, " group by ", SheetId/binary, " ) ">>;

get_from(_, DB, TableName, _, _) ->
    <<DB/binary, TableName/binary>>.




get_example(Type, ProductId) ->
    Map = case get_ThingMap(Type, ProductId) of
              {ok, Res} ->
                  maps:fold(
                      fun(K, V, Acc) ->
                          case V of
                              <<"text">> ->
                                  Acc#{K => <<"text">>};
                              <<"enum">> ->
                                  Acc#{K => <<"text">>};
                              _ ->
                                  Acc#{K => 2.55}
                          end
                      end, #{}, Res)
          end,
    io:format("~ts ~n", [unicode:characters_to_list(jsx:encode(Map))]).







get_all_sheet(ProductId, Start, End, Channel, DeviceId, Where, Limit, Skip, _) ->
    UnflattenWhere = unflatten_where(Where),
    case dgiot_hook:run_hook({factory, get_sheet_list}, [ProductId]) of
        {ok, [SheetList]} ->
            case length(SheetList) > 1 of
                true ->
                    FirstSheet = lists:nth(1, SheetList),
                    SheetWhere = get_sheet_where(FirstSheet, UnflattenWhere),
                    case dgiot_factory_getdata:get_work_sheet(ProductId, FirstSheet, Start, End, Channel, DeviceId, SheetWhere, undefined, undefined, <<"true">>) of
                        {ok, {_, FirstData}} ->
                            Data = get_other_sheet(FirstSheet, FirstData, lists:nthtail(1, SheetList), ProductId, Channel, DeviceId, UnflattenWhere),
                            {Total, Res} = filter_data(Limit, Skip, Data),
                            {ok, {Total, Res}};
                        _ ->
                            error
                    end;
                _ ->
                    error
            end;
        _ ->
            error

    end.

get_other_sheet(FirstSheet, FirstData, SheetList, ProductId, Channel, DeviceId, UnflattenWhere) ->
    lists:foldl(
        fun(Data, Acc) ->
            case maps:find(?SHEETID(FirstSheet), Data) of
                {ok, Id} ->
                    case get_left_data(FirstSheet, Data, SheetList, UnflattenWhere, ProductId, Channel, DeviceId, Id) of
                        {ok, AllData} ->
                            Acc ++ [AllData];
                        _ ->
                            Acc
                    end;
                _ ->
                    Acc
            end

        end, [], FirstData).


get_left_data(FirstSheet, Data, SheetList, UnflattenWhere, ProductId, Channel, DeviceId, Id) ->
    Res = lists:foldl(
        fun(NextSheet, Acc) ->
            case maps:size(Acc) > 0 of
                true ->
                    NextSheetWhere = case get_sheet_where(FirstSheet, UnflattenWhere) of
                                         undefined ->
                                             #{?SHEETID(NextSheet) => Id};
                                         Where ->
                                             maps:merge(Where, #{?SHEETID(NextSheet) => Id})
                                     end,
                    case dgiot_factory_getdata:get_work_sheet(ProductId, NextSheet, undefined, undefined, Channel, DeviceId, NextSheetWhere, 1, 0, <<"true">>) of
                        {ok, {0, _}} ->
                            Acc;
                        {ok, {1, [NextSheetData]}} ->
                            maps:merge(Acc, NextSheetData);
                        _ ->
                            Acc
                    end;
                _ ->
                    #{}

            end
        end, Data, SheetList),
    case maps:size(Res) > 0 of
        true ->
            {ok, Res};
        _ ->
            error
    end.

unflatten_where(undefined) ->
    undefined;
unflatten_where(Where) ->
    case is_map(Where) of
        true ->
            dgiot_map:unflatten(Where);
        _ ->
            dgiot_map:unflatten(jsx:decode(Where))
    end.

get_sheet_where(_, undefined) ->
    undefined;
get_sheet_where(X, UnflattenWhere) ->
    PersonWhere = maps:get(?PERSON, UnflattenWhere, #{}),
    SheetWhere = maps:get(X, UnflattenWhere, #{}),
    dgiot_map:flatten(maps:merge(#{?PERSON => PersonWhere}, #{X => SheetWhere})).
