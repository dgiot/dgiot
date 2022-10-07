%%%-------------------------------------------------------------------
%%% @author wolong
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 8月 2022 16:29
%%%-------------------------------------------------------------------
-module(dgiot_factory_data).
-author("wolong").
-include("dgiot_factory.hrl").
-define(PRE, <<"_">>).
-define(Database(Name), <<?PRE/binary, Name/binary>>).
-define(Table(Name), <<?PRE/binary, Name/binary>>).
-define(SHEETID(SHEET), <<SHEET/binary, "_id">>).
-define(TYPE, <<"TD">>).
-export([get_history_data/10,filter_data/1,filter_data/3]).
%% API
%%-export([
%%    get_card_data/2,
%%    get_card_data/3,
%%    get_td_sheet/10
%%    get_cache_data/3
%%]).
%%-export([get_ThingMap/2, thinglist2binary/1, get_history/9, get_device_list/1, get_example/2, filter_data/3, filter_data/1]).
%%-export([merge_data/1]).

%%get_card_data(BatchProductId, BatchDeviceId) ->
%%    DevcieTypeList = dgiot_product:get_devicetype(BatchProductId),
%%    lists:foldl(
%%        fun(DeviceType, Acc) ->
%%            case dgiot_data:get(?FACTORY_ORDER, {BatchProductId, BatchDeviceId, DeviceType}) of
%%                not_find ->
%%                    Acc;
%%                Res ->
%%                    maps:merge(Acc, Res)
%%            end
%%        end, #{}, DevcieTypeList).

%%get_card_data(ProductId, RollId, DeviceType) ->
%%    case dgiot_data:get(?FACTORY_ORDER, {ProductId, RollId, DeviceType}) of
%%        not_find ->
%%            get_td_last(ProductId, RollId, DeviceType);
%%        Data ->
%%            Data
%%    end.


%%get_td_last(ProductId, Type,  DeviceId) ->
%%    case dgiot_product:get_device_thing(ProductId, Type) of
%%        not_find ->
%%            error;
%%        ThingMap ->
%%            case get_history(Channel, ProductId, DeviceId, ThingMap, Type) of
%%                {ok, #{<<"results">> := HistoryData}} ->
%%                    NamedData = dgiot_factory_utils:turn_name(HistoryData, ThingMap),
%%%%                    {Total, Res} = filter_data(Limit, Skip, NamedData),
%%                    {ok, {Total, filter_data(Res)}};
%%
%%                _ ->
%%                    error
%%            end
%%    end.

%%
%%
%%get_td_sheet(ProductId, Type, Start, End, Channel, DeviceId, Where, Limit, Skip, New) ->
%%    case dgiot_product:get_device_thing(ProductId, Type) of
%%        not_find ->
%%            error;
%%        ThingMap ->
%%            case get_history(Channel, ProductId, DeviceId, ThingMap, Where, Start, End, Type, New) of
%%                {ok, #{<<"results">> := HistoryData}} ->
%%                    NamedData = dgiot_factory_utils:turn_name(HistoryData, ThingMap),
%%                    {Total, Res} = filter_data(Limit, Skip, NamedData),
%%                    {ok, {Total, filter_data(Res)}};
%%
%%                _ ->
%%                    error
%%            end
%%    end.

%%get_td_sheet(ProductId, Type, Start, End, Channel, DeviceId, Where, Limit, Skip, New) ->
%%    case dgiot_product:get_device_thing(ProductId, Type) of
%%        not_find ->
%%            error;
%%        ThingMap ->
%%            case get_history(Channel, ProductId, DeviceId, ThingMap, Where, Start, End, Type, New) of
%%                {ok, #{<<"results">> := HistoryData}} ->
%%                    NamedData = dgiot_factory_utils:turn_name(HistoryData, ThingMap),
%%                    {Total, Res} = filter_data(Limit, Skip, NamedData),
%%                    {ok, {Total, filter_data(Res)}};
%%
%%                _ ->
%%                    error
%%            end
%%    end.
%%
%%merge_data(Data) ->
%%    ReversList = lists:reverse(Data),
%%    {Order, MapData} = lists:foldl(
%%        fun(X, {List, Map}) ->
%%            case maps:find(<<"person_sheetsid">>, X) of
%%                {ok, Id} ->
%%                    case lists:member(Id, List) of
%%                        true ->
%%%%                            io:format("~s ~p List=~p ~n", [?FILE, ?LINE,List]),
%%%%                            io:format("~s ~p Map=~p ~n", [?FILE, ?LINE,Map]),
%%                            Old = maps:get(Id, Map, #{}),
%%                            New = maps:merge(Old, filter_data(X)),
%%                            {List, Map#{Id => New}};
%%                        _ ->
%%%%                            io:format("~s ~p List=~p ~n", [?FILE, ?LINE,List]),
%%%%                            io:format("~s ~p Map=~p ~n", [?FILE, ?LINE,Map]),
%%                            {List ++ [Id], Map#{Id => filter_data(X)}}
%%                    end;
%%                _ ->
%%                    {List, Map}
%%            end
%%        end, {[], #{}}, ReversList),
%%    lists:foldl(
%%        fun(X, Acc) ->
%%            case maps:find(X, MapData) of
%%                {ok, Res} ->
%%                    Acc ++ [Res];
%%                _ ->
%%                    Acc
%%            end
%%        end, [], lists:reverse(Order)).
%%
%%get_ThingMap(<<"all">>, ProductId) ->
%%    case dgiot_product:get(<<"Product">>, ProductId) of
%%        {ok, #{<<"thing">> := #{<<"properties">> := Properties}}} ->
%%            ThingList = lists:foldl(
%%                fun(X, Acc) ->
%%                    case X of
%%                        #{<<"isstorage">> := true, <<"identifier">> := Identifier, <<"dataType">> := #{<<"type">> := DateType}} ->
%%                            Acc#{Identifier => DateType};
%%                        _ ->
%%                            Acc
%%                    end
%%                end, #{}, Properties),
%%
%%            case maps:size(ThingList) of
%%                0 ->
%%                    {error, not_find_thing};
%%                _ ->
%%                    {ok, ThingList}
%%            end;
%%        _ ->
%%            {error, not_find_thing}
%%    end;
%%get_ThingMap(Type, ProductId) ->
%%    case dgiot_parse:get_object(<<"Product">>, ProductId) of
%%        {ok, #{<<"thing">> := #{<<"properties">> := Properties}}} ->
%%            ThingList = lists:foldl(
%%                fun(X, Acc) ->
%%                    case X of
%%                        #{<<"devicetype">> := ?PERSON, <<"isstorage">> := true, <<"identifier">> := Identifier, <<"dataType">> := #{<<"type">> := DateType}} ->
%%                            Acc#{Identifier => DateType};
%%                        #{<<"devicetype">> := Type, <<"isstorage">> := true, <<"identifier">> := Identifier, <<"dataType">> := #{<<"type">> := DateType}} ->
%%                            Acc#{Identifier => DateType};
%%                        _ ->
%%                            Acc
%%                    end
%%                end, #{}, Properties),
%%
%%            case maps:size(ThingList) of
%%                0 ->
%%                    {error, not_find_thing};
%%                _ ->
%%                    {ok, ThingList}
%%            end;
%%        _ ->
%%            {error, not_find_thing}
%%    end.
%%
%%thinglist2binary(_ThingList) ->
%%%%    Str = lists:foldl(
%%%%        fun(X, Acc) ->
%%%%            Acc ++ " ," ++ dgiot_utils:to_list(X)
%%%%        end, [], ThingList),
%%%%    dgiot_utils:to_binary(lists:nthtail(2, Str))
%%    <<" * ">>.
%%
%%get_history(Channel, ProductId, DeviceId, ThingMap, Where, Start, End, Type, New) ->
%%    case dgiot_data:get({tdengine_os, Channel}) of
%%        <<"windows">> ->
%%            {error, wrong_td_platform};
%%        _ ->
%%            DB = dgiot_tdengine_select:format_db(?Database(ProductId)),
%%            TableName = case DeviceId of
%%                            undefined ->
%%                                ?Table(ProductId);
%%                            _ ->
%%                                ?Table(DeviceId)
%%                        end,
%%
%%            Quality = case dgiot_product:get_device_thing(ProductId, <<"quality">>) of
%%                          not_find ->
%%                              #{};
%%                          Res ->
%%                              Res
%%                      end,
%%            Person = case dgiot_product:get_device_thing(ProductId, <<"person">>) of
%%                         not_find ->
%%                             #{};
%%                         Res1 ->
%%                             Res1
%%                     end,
%%            AllThngMap = maps:merge(ThingMap, maps:merge(Quality, Person)),
%%            List = maps:keys(AllThngMap) ++ [<<"createdat">>],
%%            Select = thinglist2binary(List),
%%            From = get_from(New, List, DB, TableName),
%%%%            io:format("~s ~p From = ~ts ~n", [?FILE, ?LINE, From]),
%%            WHERE = get_where(Where, AllThngMap, Type, ProductId, Start, End),
%%%%            io:format("~s ~p WHERE = ~ts ~n", [?FILE, ?LINE, WHERE]),
%%%%            LimitSkip = get_limit_skip(Limit, Skip),
%%            Order = <<" ORDER BY createdat DESC ">>,
%%            dgiot_tdengine:transaction(Channel,
%%                fun(Context) ->
%%                    Sql = <<"SELECT ", Select/binary, " FROM ", From/binary, WHERE/binary, Order/binary, ";">>,
%%                    dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)
%%                end)
%%
%%    end.

%%get_limit_skip(undefined, undefined) ->
%%    <<" limit 10  ">>;
%%get_limit_skip(Limit, Skip) ->
%%    BinLimit = dgiot_utils:to_binary(Limit),
%%    BinSkip = dgiot_utils:to_binary(Skip),
%%    <<" limit ", BinLimit/binary, " offset ", BinSkip/binary>>.
%%(Where, ThingMap, Type, ProductId, Start, End)
%%
%%get_where(Where, _ProductId) when is_map(Where) ->
%%    case maps:size(Where) /= 0 of
%%        true ->
%%            <<" and ", Where1/binary>> = maps:fold(
%%                fun(K, Map, Acc) ->
%%                    BinOption = dgiot_utils:to_binary(lists:nth(1, maps:keys(Map))),
%%                    V = lists:nth(1, maps:values(Map)),
%%                    Type = maps:get(K, ThingMap),
%%                    BinV = format_value(Type, V),
%%                    <<Acc/binary, " and ", K/binary, " ", BinOption/binary, " ", BinV/binary>>
%%                end, <<"">>, maps:remove(<<"product">>, Where)),
%%            case get_time(Start, End) of
%%                error ->
%%                    <<"where ", Where1/binary>>;
%%                Time ->
%%                    <<" where ", Where1/binary, " and ", Time/binary>>
%%            end;
%%        _ ->
%%            case get_time(Start, End) of
%%                error ->
%%                    <<" ">>;
%%                Time ->
%%                    <<" where ", Time/binary>>
%%            end
%%    end.
%%get_time(undefined, undefined) ->
%%    error;
%%get_time(undefined, End) ->
%%    BinEnd = dgiot_utils:to_binary(End * 1000),
%%    <<"  createdat < ", BinEnd/binary, " ">>;
%%get_time(Start, undefined) ->
%%    BinStart = dgiot_utils:to_binary(Start * 1000),
%%    <<"  createdat > ", BinStart/binary, " ">>;
%%get_time(Start, End) ->
%%    BinStart = dgiot_utils:to_binary(Start * 1000),
%%    BinEnd = dgiot_utils:to_binary(End * 1000),
%%    <<"  createdat > ", BinStart/binary, " and createdat < ", BinEnd/binary, " ">>.

filter_data(undefined, _, HistoryData) ->
    Total = length(HistoryData),
    {Total, HistoryData};

filter_data(Limit, undefined, HistoryData) ->
    filter_data(Limit, 0, HistoryData);

filter_data(Limit, Skip, HistoryData) ->
    Total = length(HistoryData),
    Min = max(Skip + 1, 0),
    Max = min(Total, Limit + Skip),
    Res = case Max > Min of
              true ->
                  lists:sublist(HistoryData, Min, Max);
              _ ->
                  HistoryData
          end,
    {Total, Res}.

filter_data(Data) when is_list(Data) ->
    lists:foldl(
        fun(X, Acc) ->
            Filtered = maps:filter(fun(_, V) -> (V /= <<"null">>) and (V /= null) end, X),
            Acc ++ [Filtered]
        end, [], Data);

filter_data(Data) when is_map(Data) ->
    maps:filter(fun(_, V) -> (V /= <<"null">>) and (V /= null) end, Data).
%%
%%get_device_list(ProductId) ->
%%    case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"product">> => ProductId}}) of
%%        {ok, #{<<"results">> := Results}} ->
%%            Res = lists:foldl(
%%                fun(X, Acc) ->
%%                    case X of #{<<"objectId">> := DevId} ->
%%                        Acc ++ [DevId];
%%                        _ ->
%%                            Acc
%%                    end
%%                end, [], Results),
%%            {ok, Res};
%%        _ ->
%%            error
%%    end.
%%


%%get_from(<<"true">>, List, DB, TableName) ->
%%    DevcieTypeList = dgiot_product:get_devicetype(ProductId),
%%    List = lists:foldl(
%%        fun(DeviceType, Acc) ->
%%            case dgiot_product:get_device_thing(ProductId, DeviceType) of
%%                not_find ->
%%                    Acc;
%%                ThingMap ->
%%                    Acc ++ maps:keys(ThingMap)
%%            end
%%        end, [], DevcieTypeList),
%%    io:format("~s ~p List = ~p  ~n", [?FILE, ?LINE, length(List)]),
%%    Last = lists:foldl(
%%        fun(X, Acc) ->
%%            L = dgiot_utils:to_list(X),
%%            Acc ++ " ,last(" ++ L ++ " ) as " ++ L
%%        end, [], List),
%%    Select = dgiot_utils:to_binary(lists:nthtail(2, Last)),
%%    Group = <<"person_sheetsid">>,
%%    <<"( select ", Select/binary, " from ", DB/binary, TableName/binary, " group by ", Group/binary, " ) ">>;
%%
%%get_from(_, _, DB, TableName) ->
%%    <<DB/binary, TableName/binary>>.

%%
%%
%%
%%get_example(Type, ProductId) ->
%%    Map = case get_ThingMap(Type, ProductId) of
%%              {ok, Res} ->
%%                  maps:fold(
%%                      fun(K, V, Acc) ->
%%                          case V of
%%                              <<"text">> ->
%%                                  Acc#{K => <<"text">>};
%%                              <<"enum">> ->
%%                                  Acc#{K => <<"text">>};
%%                              _ ->
%%                                  Acc#{K => 2.55}
%%                          end
%%                      end, #{}, Res)
%%          end,
%%    io:format("~ts ~n", [unicode:characters_to_list(jsx:encode(Map))]).
%%
%%
%%
%%format_value(<<"text">>, V) ->
%%    <<"\"", V/binary, "\"">>;
%%format_value(_, V) ->
%%    dgiot_utils:to_binary(V).


%%get_work_sheet(ProductId, ProcessType, Start, End, Channel, DeviceId, Where, Limit, Skip, New) ->
%%    case get_cache_data(ProductId, DeviceId, ProcessType) of
%%        {ok, Res} ->
%%            {ok, Res};
%%        _ ->
%%            get_td_sheet(ProductId, ProcessType, Start, End, Channel, DeviceId, Where, Limit, Skip, New)
%%    end.
%%
%%get_cache_data(ProductId, DeviceId, <<"quality">>) ->
%%    case dgiot_data:get(?FACTORY_QUALITY, {ProductId, DeviceId, '-'}) of
%%        {ok, Res} ->
%%            Res;
%%        _ ->
%%            error
%%    end;
%%get_cache_data(ProductId, DeviceId, Type) ->
%%    case dgiot_data:get(?FACTORY_QUALITY, {ProductId, DeviceId, Type}) of
%%        {ok, Res} ->
%%            Res;
%%        _ ->
%%            error
%%    end.
%%
%%get_cache_data(<<"all">>, ProductId, Start, End, _, DeviceId, Where, Limit, Skip, _) ->
%%    Dev = case DeviceId of
%%              undefined ->
%%                  '_';
%%              _ ->
%%                  DeviceId
%%
%%          end,
%%    DeviceTypeList = dgiot_product:get_devicetype(ProductId),
%%    AllData = lists:foldl(
%%        fun(DevType, Acc) ->
%%            case dgiot_data:match(?FACTORY_ORDER, {{ProductId, Dev, '_', DevType}, '$1'}) of
%%                {ok, Res} ->
%%                    Acc ++ Res;
%%                _ ->
%%                    Acc
%%            end
%%        end, [], DeviceTypeList),
%%    MergedData = merge_data(AllData),
%%    FiltedData = filter_cache_data(MergedData, Start, End, Where),
%%    {Total, Result} = filter_data(Limit, Skip, FiltedData),
%%    {ok, {Total, Result}};
%%
%%get_cache_data(ProductId, Type, Start, End, _, DeviceId, Where, Limit, Skip, <<"true">>) ->
%%    io:format("~s ~p Type = ~p  ~n", [?FILE, ?LINE, Type]),
%%    Dev = case DeviceId of
%%              undefined ->
%%                  '_';
%%              _ ->
%%                  DeviceId
%%
%%          end,
%%    case dgiot_data:match(?FACTORY_ORDER, {{ProductId, Dev, '_', Type}, '$1'}) of
%%        {ok, [Res]} ->
%%%%            io:format("~s ~p length = ~p  ~n", [?FILE, ?LINE,length(Res)]),
%%            FiltedData = filter_cache_data(Res, Start, End, Where),
%%%%            io:format("~s ~p length = ~p  ~n", [?FILE, ?LINE,length(FiltedData)]),
%%            {Total, Result} = filter_data(Limit, Skip, FiltedData),
%%%%            io:format("~s ~p length = ~p ~n", [?FILE, ?LINE,length(Result)]),
%%            {ok, {Total, Result}};
%%        _ ->
%%            error
%%    end;
%%get_cache_data(_, _, _, _, _, _, _, _, _, _) ->
%%    error.
%%
%%filter_cache_data(Data, undefined, _, Where) ->
%%    lists:foldl(
%%        fun([X, _], Acc) ->
%%            case Where of
%%                undefined ->
%%                    Acc ++ [X];
%%                _ ->
%%                    case maps:merge(X, Where) == X of
%%                        true ->
%%                            Acc ++ [X];
%%                        _ ->
%%                            Acc
%%                    end
%%            end
%%        end, [], Data);
%%filter_cache_data(Data, Start, End, Where) ->
%%    lists:foldl(
%%        fun([X, Date], Acc) ->
%%            case (Date > End) and (Date < Start) of
%%                true ->
%%                    case maps:merge(X, Where) == X of
%%                        true ->
%%                            Acc ++ [X];
%%                        _ ->
%%                            Acc
%%                    end;
%%                _ ->
%%                    Acc
%%            end
%%        end, [], Data).


%%merge_data(ParseData, HistoryData, DeviceList, _ThingMap) when is_list(DeviceList) ->
%%    lists:foldl(
%%        fun(X, Acc) ->
%%            DeviceId = maps:get(<<"person_deviceid">>, X, <<"">>),
%%            case maps:find(DeviceId, ParseData) of
%%                {ok, Parse} ->
%%                    Acc ++ [maps:merge(Parse, X)];
%%                _ ->
%%                    Acc
%%            end
%%        end, [], HistoryData);
%%
%%merge_data(ParseData, HistoryData, _, _ThingMap) ->
%%    lists:foldl(
%%        fun(X, Acc) ->
%%            Acc ++ [maps:merge(ParseData, X)]
%%        end, [], HistoryData).
%%
%%
%%search_parse(DeviceList, Parse, Type) when is_list(DeviceList) ->
%%    Res = lists:foldl(
%%        fun(X, Acc) ->
%%            case search_parse(X, Parse, Type) of
%%                {ok, Res} ->
%%                    Acc#{X => Res};
%%                _ ->
%%                    Acc
%%            end
%%        end, #{}, DeviceList),
%%    {ok, Res};
%%
%%search_parse(DeviceId, undefined, Type) ->
%%    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
%%        {ok, #{<<"content">> := #{Type := Data}}} ->
%%            FlatternMap = dgiot_map:flatten(#{Type => Data}),
%%            {ok, FlatternMap#{<<"objectId">> => DeviceId}};
%%        _ ->
%%            error
%%    end;
%%
%%search_parse(DeviceId, Parse, Type) ->
%%    case maps:size(Parse) of
%%        0 ->
%%            search_parse(DeviceId, undefined, Type);
%%        Num ->
%%            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
%%                {ok, #{<<"content">> := Content}} ->
%%                    FlatMap = dgiot_map:flatten(Content),
%%                    MatchNum = maps:fold(
%%                        fun(K, V, Acc) ->
%%                            case maps:find(K, FlatMap) of
%%                                {ok, V} ->
%%                                    Acc + 1;
%%                                _ ->
%%                                    Acc
%%                            end
%%
%%                        end, 0, Parse),
%%                    case MatchNum of
%%                        Num ->
%%                            Data = maps:get(<<Type/binary>>, Content),
%%                            FlatternMap = dgiot_map:flatten(#{Type => Data}),
%%                            {ok, FlatternMap#{<<"objectId">> => DeviceId}};
%%                        _ ->
%%                            error
%%                    end
%%            end
%%    end.
%%
%%filter_where(undefined, ProductId, Type) ->
%%    case get_ThingMap(Type, ProductId) of
%%        {ok, ThingMap} ->
%%            {undefined, undefined, ThingMap};
%%        _ ->
%%            error
%%    end;
%%filter_where(Where, ProductId, Type) ->
%%    MapWhere = case is_map(Where) of
%%                   true ->
%%                       dgiot_factory_utils:turn_num(Where, ProductId, Type);
%%                   _ ->
%%                       dgiot_factory_utils:turn_num(jsx:decode(Where), ProductId, Type)
%%               end,
%%    case get_ThingMap(Type, ProductId) of
%%        {ok, ThingMap} ->
%%            {Parse, Td} = maps:fold(
%%                fun(K, V, {Parse, Td}) ->
%%                    case maps:is_key(K, ThingMap) of
%%                        true ->
%%                            {Parse, Td#{K => V}};
%%                        _ ->
%%                            {Parse#{K => V}, Td}
%%
%%                    end
%%                end, {#{}, #{}}, MapWhere),
%%
%%            TdWithPerson = case maps:is_key(<<"person">>, MapWhere) of
%%                               true ->
%%                                   Td#{<<"person">> => maps:get(<<"person">>, MapWhere)};
%%                               false ->
%%                                   Td
%%                           end,
%%            {Parse, TdWithPerson, ThingMap};
%%        _ ->
%%            error
%%    end.
%%get_td_sheet(ProductId, Type, Start, End, Channel, DeviceId, Where, Limit, Skip, New)

get_history_data(ProductId, DeviceId, Type, Function, Group, Where, Order, Channel, Limit, Skip) ->
    DB = dgiot_tdengine_select:format_db(?Database(ProductId)),
    TableName = case DeviceId of
                    undefined ->
                        ?Table(ProductId);
                    _ ->
                        ?Table(DeviceId)
                end,


    Select = select(ProductId, Type, Function),
    From = <<DB/binary, ".", TableName/binary>>,
    GROPU = group(Group),
    WHERE = where(Where),
    ORDER = order(Order),
    LimitAndSkip = limit_skip(Limit, Skip),
    {ok, #{<<"results">> := HistoryData}} = dgiot_tdengine:transaction(Channel,
        fun(Context) ->
            Sql = <<"SELECT , ", Select/binary, " FROM ", From/binary, GROPU/binary, WHERE/binary, ORDER/binary, LimitAndSkip/binary, ";">>,
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)
        end),

    {ok, #{<<"results">> := [#{<<"count">> := Total}]}} = dgiot_tdengine:transaction(Channel,
        fun(Context) ->
            Sql = <<"SELECT count(*) as count , FROM ", From/binary, GROPU/binary, WHERE/binary, ORDER/binary, ";">>,
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)
        end),
    {ok, {Total, filter_data(HistoryData)}}.


select(ProductId, Type, undefined) ->
    case dgiot_product:get_device_thing(ProductId, Type) of
        not_find ->
            <<"select * ">>;
        ThingMap ->
            Quality = case dgiot_product:get_device_thing(ProductId, <<"quality">>) of
                          not_find ->
                              #{};
                          Res ->
                              Res
                      end,
            Person = case dgiot_product:get_device_thing(ProductId, <<"person">>) of
                         not_find ->
                             #{};
                         Res1 ->
                             Res1
                     end,
            AllThngMap = maps:merge(ThingMap, maps:merge(Quality, Person)),
            List = maps:keys(AllThngMap) ++ [<<"createdat">>],

            <<" , ", Res2/binary>> = lists:foldl(
                fun(X, Acc) ->
                    <<Acc/binary, " , ", X/binary>>

                end, <<"">>, List),
            Res2

    end;
select(ProductId, Type, Function) ->
    case dgiot_product:get_device_thing(ProductId, Type) of
        not_find ->
            <<"select * ">>;
        ThingMap ->
            Quality = case dgiot_product:get_device_thing(ProductId, <<"quality">>) of
                          not_find ->
                              #{};
                          Res ->
                              Res
                      end,
            Person = case dgiot_product:get_device_thing(ProductId, <<"person">>) of
                         not_find ->
                             #{};
                         Res1 ->
                             Res1
                     end,
            AllThngMap = maps:merge(ThingMap, maps:merge(Quality, Person)),
            List = maps:keys(AllThngMap) ++ [<<"createdat">>],

            <<" , ", Res2/binary>> = lists:foldl(
                fun(X, Acc) ->
                    <<Acc/binary, " , ", Function/binary, "(", X/binary, ") as ", X/binary>>
                end, <<"">>, List),
            Res2
    end.


group(undefined) ->
    <<" ">>;
group(Group) ->
    <<" group by ", Group/binary, " ">>.
where(undefined) ->
    <<" ">>;
where(Where) ->
    <<" where ", Where/binary>>.
order(udnefined) ->
    <<" ">>;
order(Order) ->
    <<" ", Order/binary>>.
limit_skip(undefined, undefined) ->
    <<" limit 10  ">>;
limit_skip(Limit, Skip) ->
    BinLimit = dgiot_utils:to_binary(Limit),
    BinSkip = dgiot_utils:to_binary(Skip),
    <<" limit ", BinLimit/binary, " offset ", BinSkip/binary>>.
