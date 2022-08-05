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

-define(PRE, <<"_">>).
-define(Database(Name), <<?PRE/binary, Name/binary>>).
-define(Table(Name), <<?PRE/binary, Name/binary>>).
-define(SHEETID(SHEET), <<SHEET/binary, "_id">>).
-define(TYPE, <<"TD">>).

%% API
-export([get_work_sheet/8]).
-export([get_ThingMap/2, thinglist2binary/1, get_history/9, get_device_list/1,get_example/2, filter_data/3]).

get_work_sheet(ProductId, Type, Channel, DeviceId, Where, Limit, Skip, New) ->
    case filter_where(Where, ProductId, Type) of
        {Parse, Td, ThingMap} ->
            case search_parse(DeviceId, Parse, Type) of
                {ok, ParseData} ->
                    case get_history(Channel, ProductId, DeviceId, ThingMap, Td, Limit, Skip, Type, New) of
                        {ok, #{<<"results">> := HistoryData}} ->
                            io:format("~s ~p DeviceId= ~p ~n",[?FILE,?LINE,DeviceId]),
                            {Total, Res} = filter_data(Limit, Skip, HistoryData),
                            MergeData = merge_data(ParseData, Res, DeviceId, ThingMap),
                            NamedData = dgiot_factory_utils:turn_name(MergeData, ThingMap),
                            {ok, {Total, NamedData}};
                        _ ->
                            error
                    end;
                _ ->
                    {ok, {0,[]}}
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
                       Where;
                   _ ->
                       jsx:decode(Where)
               end,

    case get_ThingMap(Type, ProductId) of
        {ok, ThingMap} ->
            {Parse, Td} = maps:fold(
                fun(K,V,{Parse, Td})->
                    case maps:is_key(K,ThingMap) of
                        true ->
                            {Parse, Td#{K => V}};
                        _ ->
                            {Parse#{K => V}, Td}

                    end
            end,{#{},#{}},MapWhere),
            TdWithPerson = case maps:is_key(<<"person">>,ThingMap) of
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

get_history(Channel, ProductId, DeviceId, ThingMap, Where, _Limit, _Skip, Type, New) ->
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
            DetectThing = <<Type/binary, "_id">>,

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
                    WHERE = get_where(Where, ThingMap, DetectThing, ProductId),
                    Order = <<" ORDER BY createdat DESC ">>,
                    Sql = <<"SELECT ", Select/binary, " FROM ", From/binary, WHERE/binary, Order/binary, ";">>,
%%                    ?LOG(error, "Sql ~s", [Sql]),
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
                                  Acc#{K =><<"text">>};
                              <<"enum">> ->
                                  Acc#{K =><<"text">>};
                              _ ->
                                  Acc#{K =>2.55}
                          end
                      end, #{}, Res)
          end ,
    io:format("~ts ~n",[unicode:characters_to_list(jsx:encode(Map))]).
