%%%-------------------------------------------------------------------
%%% @doc 无人机测试项加载器（统一版本）
%%% 只负责加载和解析测试项，不包含执行逻辑
%%%-------------------------------------------------------------------
-module(dgiot_uav_test_loader).

-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    load/1,
    load_by_station/1,
    load_by_station_name/1,
    load_by_name/1,
    load_by_product/2,
    get_steps/1,
    validate/1,
    test/0
]).

%% 记录定义
-record(test_item, {
    id :: binary(),
    name :: binary(),
    station_id :: integer(),
    station_name :: binary(),
    steps = [] :: list(),
    order = 0 :: integer()
}).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 加载测试项
-spec load(binary()) -> {ok, #test_item{}} | {error, term()}.
load(DeviceId) ->
    ?LOG(debug, "[LOADER] 加载测试项: ~s", [DeviceId]),
    
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, Device} ->
            Content = maps:get(<<"content">>, Device, #{}),
            Steps = maps:get(<<"steps">>, Content, []),
            
            TestItem = #test_item{
                id = DeviceId,
                name = maps:get(<<"name">>, Device, <<>>),
                station_id = extract_station_id(Device),
                station_name = maps:get(<<"station_name">>, Device, <<>>),
                steps = Steps,
                order = maps:get(<<"order">>, Content, 0)
            },
            
            case validate(TestItem) of
                ok -> {ok, TestItem};
                Error -> Error
            end;
        {error, Reason} ->
            ?LOG(error, "[LOADER] 加载失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 根据工位ID加载测试项（使用前缀匹配，如 "磁航向_"）
-spec load_by_station(integer()) -> {ok, [#test_item{}]} | {error, term()}.
load_by_station(StationId) when is_integer(StationId) ->
    StationName = station_id_to_name(StationId),
    load_by_station_name(StationName).

%% @doc 根据工位名称加载测试项（使用前缀匹配，备用devaddr查询）
%% 测试项名称格式："磁航向校准测试项" 或 "磁航向_磁航向测试"
%% 查询策略：1) 先按名称前缀匹配 2) 找不到则按devaddr前缀查询
-spec load_by_station_name(binary()) -> {ok, [#test_item{}]} | {error, term()}.
load_by_station_name(StationName) ->
    ?LOG(error, "[LOADER] 从Parse库加载工位测试项: ~s (前缀匹配)", [StationName]),
    
    Query = #{
        <<"where">> => #{<<"content.is_test_item_device">> => true},
        <<"limit">> => 500
    },
    
    case dgiot_parse:query_object(<<"Device">>, Query) of
        {ok, #{<<"results">> := Results}} when is_list(Results) ->
            %% 尾递归筛选：按名称前缀匹配
            case filter_by_name_prefix(Results, StationName, []) of
                [] ->
                    %% 名称匹配失败，尝试策略2: 按devaddr前缀查询
                    ?LOG(error, "[LOADER] 名称匹配未找到，尝试devaddr查询"),
                    load_by_devaddr_prefix(StationName, Results);
                StationItems ->
                    ?LOG(error, "[LOADER] Parse库找到测试项数量: ~p (前缀: ~s)", 
                         [length(StationItems), StationName]),
                    %% 尾递归转换
                    TestItems = map_to_records(StationItems, []),
                    {ok, TestItems}
            end;
        {ok, #{<<"results">> := []}} ->
            ?LOG(error, "[LOADER] Parse库未找到测试项"),
            {ok, []};
        {error, Reason} ->
            ?LOG(error, "[LOADER] Parse库查询失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 根据devaddr前缀查询测试项（备用策略）
-spec load_by_devaddr_prefix(binary(), [map()]) -> {ok, [#test_item{}]}.
load_by_devaddr_prefix(StationName, AllTestItems) ->
    DevAddrPrefix = get_devaddr_prefix(StationName),
    
    case DevAddrPrefix of
        <<>> ->
            ?LOG(error, "[LOADER] 工位~ts无devaddr映射", [StationName]),
            {ok, []};
        _ ->
            %% 尾递归筛选：按devaddr前缀匹配
            DevAddrItems = filter_by_devaddr_prefix(AllTestItems, DevAddrPrefix, []),
            ?LOG(error, "[LOADER] devaddr查询找到测试项数量: ~p (前缀: ~s)", 
                 [length(DevAddrItems), DevAddrPrefix]),
            %% 尾递归转换
            {ok, map_to_records(DevAddrItems, [])}
    end.

%% @doc 根据测试项名称精确加载（如 "磁航向_磁航向测试"）
-spec load_by_name(binary()) -> {ok, #test_item{}} | {error, term()}.
load_by_name(TestItemName) when is_binary(TestItemName) ->
    ?LOG(error, "[LOADER] 按名称加载测试项: ~s", [TestItemName]),
    
    %% 从Parse库查询
    Query = #{
        <<"where">> => #{
            <<"name">> => TestItemName,
            <<"content.is_test_item_device">> => true
        },
        <<"limit">> => 1
    },
    
    case dgiot_parse:query_object(<<"Device">>, Query) of
        {ok, #{<<"results">> := [Item | _]}} ->
            ?LOG(error, "[LOADER] 找到测试项: ~s", [TestItemName]),
            {ok, map_to_record(Item)};
        {ok, #{<<"results">> := []}} ->
            ?LOG(error, "[LOADER] 未找到测试项: ~s", [TestItemName]),
            {error, not_found};
        {error, Reason} ->
            ?LOG(error, "[LOADER] 查询失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 获取设备的工位名称（优先从content.common_params获取）
get_station_name(Item) ->
    Content = maps:get(<<"content">>, Item, #{}),
    CommonParams = maps:get(<<"common_params">>, Content, #{}),
    
    %% 优先级：content.common_params.station_name > content.station_name > 顶层station_name
    case maps:get(<<"station_name">>, CommonParams, undefined) of
        undefined ->
            case maps:get(<<"station_name">>, Content, undefined) of
                undefined ->
                    maps:get(<<"station_name">>, Item, <<>>);
                Name -> Name
            end;
        Name -> Name
    end.

%% @doc 根据产品ID和名称加载测试项
-spec load_by_product(binary(), binary()) -> {ok, #test_item{}} | {error, term()}.
load_by_product(ProductId, TestName) ->
    case dgiot_parse:query_object(<<"Device">>, #{
        <<"where">> => #{<<"product">> => ProductId, <<"name">> => TestName},
        <<"limit">> => 1
    }) of
        {ok, #{<<"results">> := [Device | _]}} ->
            load(maps:get(<<"objectId">>, Device));
        {ok, #{<<"results">> := []}} -> 
            {error, not_found};
        {error, Reason} -> 
            {error, Reason}
    end.

%% @doc 获取测试项的步骤列表
-spec get_steps(binary()) -> {ok, list()} | {error, term()}.
get_steps(TestItemId) ->
    case load(TestItemId) of
        {ok, #test_item{steps = Steps}} -> 
            {ok, Steps};
        Error -> 
            Error
    end.

%% @doc 验证测试项格式
-spec validate(#test_item{}) -> ok | {error, term()}.
validate(#test_item{id = Id}) when Id =:= undefined; Id =:= <<>> ->
    {error, missing_id};
validate(#test_item{steps = Steps}) when not is_list(Steps) ->
    {error, invalid_steps};
validate(#test_item{steps = Steps}) ->
    validate_steps(Steps, 1).

%%====================================================================
%% 内部函数
%%====================================================================

%% 从设备信息中提取工位ID
extract_station_id(Device) ->
    %% 优先从 station_id 字段获取
    case maps:get(<<"station_id">>, Device, undefined) of
        Id when is_integer(Id) -> Id;
        _ ->
            %% 从名称中提取
            Name = maps:get(<<"name">>, Device, <<>>),
            extract_from_name(Name)
    end.

%% 从名称中提取工位ID
extract_from_name(Name) ->
    case binary:split(Name, <<"_">>, [global]) of
        [<<"磁航向"/utf8>> | _] -> 1700;
        [<<"总测1"/utf8>> | _] -> 1500;
        [<<"总测2"/utf8>> | _] -> 1600;
        [<<"拷机1"/utf8>> | _] -> 1200;
        [<<"拷机2"/utf8>> | _] -> 1300;
        [<<"桁架"/utf8>> | _] -> 1100;
        _ -> 0
    end.

%% 工位ID转名称 - 使用case表达式而不是函数子句
-spec station_id_to_name(integer()) -> binary().
station_id_to_name(StationId) ->
    case StationId of
        1700 -> <<"磁航向"/utf8>>;
        1500 -> <<"总测"/utf8>>;
        1600 -> <<"总测"/utf8>>;
        1200 -> <<"拷机"/utf8>>;
        1300 -> <<"拷机"/utf8>>;
        1100 -> <<"桁架"/utf8>>;
        _ -> <<"未知"/utf8>>
    end.

%% @doc 获取devaddr前缀（工位名称映射）
-spec get_devaddr_prefix(binary()) -> binary().
get_devaddr_prefix(StationName) ->
    case StationName of
        <<"磁航向"/utf8>> -> <<"120">>;
        <<"拷机"/utf8>> -> <<"13">>;
        <<"总测"/utf8>> -> <<"15">>;
        <<"桁架"/utf8>> -> <<"11">>;
        _ -> <<>>
    end.

%% @doc 尾递归：按名称前缀筛选测试项
-spec filter_by_name_prefix([map()], binary(), [map()]) -> [map()].
filter_by_name_prefix([], _StationName, Acc) ->
    lists:reverse(Acc);
filter_by_name_prefix([Item | Rest], StationName, Acc) ->
    ItemName = maps:get(<<"name">>, Item, <<>>),
    case binary:match(ItemName, StationName) of
        {0, _} ->
            filter_by_name_prefix(Rest, StationName, [Item | Acc]);
        _ ->
            filter_by_name_prefix(Rest, StationName, Acc)
    end.

%% @doc 尾递归：按devaddr前缀筛选测试项
-spec filter_by_devaddr_prefix([map()], binary(), [map()]) -> [map()].
filter_by_devaddr_prefix([], _DevAddrPrefix, Acc) ->
    lists:reverse(Acc);
filter_by_devaddr_prefix([Item | Rest], DevAddrPrefix, Acc) ->
    DevAddr = maps:get(<<"devaddr">>, Item, <<>>),
    case binary:match(DevAddr, DevAddrPrefix) of
        {0, _} ->
            filter_by_devaddr_prefix(Rest, DevAddrPrefix, [Item | Acc]);
        _ ->
            filter_by_devaddr_prefix(Rest, DevAddrPrefix, Acc)
    end.

%% @doc 尾递归：批量转换Map到记录
-spec map_to_records([map()], [#test_item{}]) -> [#test_item{}].
map_to_records([], Acc) ->
    lists:reverse(Acc);
map_to_records([Item | Rest], Acc) ->
    map_to_records(Rest, [map_to_record(Item) | Acc]).

%% Map转记录（适配Parse库Device表结构）
map_to_record(Map) ->
    Content = maps:get(<<"content">>, Map, #{}),
    
    %% steps 可能在 content.steps 或 content.steps（数组）
    Steps = case maps:get(<<"steps">>, Content, []) of
        S when is_list(S) -> S;
        _ -> []
    end,
    
    %% 工位编号可能在 content.common_params.station_number 或直接在 content
    CommonParams = maps:get(<<"common_params">>, Content, #{}),
    StationNumber = maps:get(<<"station_number">>, CommonParams, 
                           maps:get(<<"station_number">>, Content, 0)),
    
    %% 使用 get_station_name 获取工位名称
    StationName = get_station_name(Map),
    
    #test_item{
        id = maps:get(<<"objectId">>, Map, <<>>),
        name = maps:get(<<"name">>, Map, <<>>),
        station_id = StationNumber,
        station_name = StationName,
        steps = Steps,
        order = maps:get(<<"order">>, Content, 0)
    }.

%% 验证步骤列表
validate_steps([], _) -> ok;
validate_steps([Step | Rest], Index) ->
    case validate_step(Step) of
        ok -> validate_steps(Rest, Index + 1);
        {error, Reason} -> {error, {Index, Reason}}
    end.

%% 验证单个步骤
validate_step(Step) ->
    ActionType = maps:get(<<"action_type">>, Step, maps:get(<<"type">>, Step, undefined)),
    case ActionType of
        undefined -> {error, missing_action_type};
        _ -> ok
    end.

%%====================================================================
%% 测试函数
%%====================================================================
-spec test() -> ok.
test() ->
    io:format(<<"~n========== 测试加载器测试 =========="/utf8>>, []),
    
    %% 测试查询所有设备
    io:format(<<"~n1. 查询Parse库设备...~n"/utf8>>, []),
    Query = #{<<"limit">> => 100},
    case dgiot_parse:query_object(<<"Device">>, Query) of
        {ok, #{<<"results">> := Results}} ->
            io:format(<<"   总设备数: ~p~n"/utf8>>, [length(Results)]),
            
            %% 找出测试项设备
            TestItems = lists:filter(
                fun(Item) ->
                    Content = maps:get(<<"content">>, Item, #{}),
                    maps:get(<<"is_test_item_device">>, Content, false) =:= true
                end,
                Results
            ),
            io:format(<<"   测试项设备数: ~p~n"/utf8>>, [length(TestItems)]),
            
            %% 打印测试项设备的工位
            lists:foreach(
                fun(Item) ->
                    Name = maps:get(<<"name">>, Item, <<>>),
                    StationName = get_station_name(Item),
                    io:format(<<"   - ~s (工位: ~s)~n"/utf8>>, [Name, StationName])
                end,
                TestItems
            );
        {error, Reason} ->
            io:format(<<"   查询失败: ~p~n"/utf8>>, [Reason])
    end,
    
    %% 测试按工位ID加载（前缀匹配）
    io:format(<<"~n2. 按工位ID加载测试项（前缀匹配）...~n"/utf8>>, []),
    case load_by_station(1700) of
        {ok, Items} ->
            io:format(<<"   工位1700测试项数量: ~p~n"/utf8>>, [length(Items)]),
            lists:foreach(
                fun(#test_item{name = Name, steps = Steps}) ->
                    io:format(<<"   - ~s (步骤数: ~p)~n"/utf8>>, [Name, length(Steps)])
                end,
                Items
            );
        {error, LoadReason} ->
            io:format(<<"   加载失败: ~p~n"/utf8>>, [LoadReason])
    end,
    
    %% 测试按名称精确加载
    io:format(<<"~n3. 按名称精确加载测试项...~n"/utf8>>, []),
    case load_by_name(<<"磁航向_磁航向测试"/utf8>>) of
        {ok, #test_item{name = Name, steps = Steps}} ->
            io:format(<<"   找到测试项: ~s (步骤数: ~p)~n"/utf8>>, [Name, length(Steps)]);
        {error, not_found} ->
            io:format(<<"   未找到测试项~n"/utf8>>);
        {error, OtherReason} ->
            io:format(<<"   加载失败: ~p~n"/utf8>>, [OtherReason])
    end,
    
    io:format(<<"~n========== 测试完成 ==========~n"/utf8>>, []),
    ok.
