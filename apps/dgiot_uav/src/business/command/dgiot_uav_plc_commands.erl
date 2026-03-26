%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_plc_commands - PLC 指令管理模块（简洁版）
%%% 管理6工位PLC指令集，并提供更新到工位设备（Device）的功能。
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_plc_commands).

-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    update_all/0,
    update_station/1,
    get_station_commands/1,
    test/0
]).

%% 工位产品ID常量（工位设备所属产品）
-define(STATION_PRODUCT_ID, <<"2de1b3e1b8">>).

%% ====================================================================
%% 工位指令集定义（根据原头文件提炼）
%% ====================================================================

%% 1. 桁架机械手工位 (StationId: 1100)
-define(COMMAND_SET_GANTRY, #{
    station_id => 1100,
    station_name => <<"桁架机械手工位"/utf8>>,
    commands => [
        #{code => 2, name => <<"向右上旋转30度"/utf8>>, description => <<"桁行架向右上方向旋转30度"/utf8>>},
        #{code => 3, name => <<"向右下旋转30度"/utf8>>, description => <<"桁行架向右下方向旋转30度"/utf8>>},
        #{code => 4, name => <<"左上旋转30度"/utf8>>, description => <<"桁行架向左上方向旋转30度"/utf8>>},
        #{code => 5, name => <<"左下旋转30度"/utf8>>, description => <<"桁行架向左下方向旋转30度"/utf8>>},
        #{code => 1, name => <<"回正"/utf8>>, description => <<"桁行架回正动作"/utf8>>},
        #{code => 7, name => <<"下料送走"/utf8>>, description => <<"桁行架下料送走"/utf8>>}
    ]
}).

%% 2. 拷机工位 (StationId: 1200)
-define(COMMAND_SET_BURN_IN_1, #{
    station_id => 1200,
    station_name => <<"拷机"/utf8>>,
    commands => [
        #{code => 1, name => <<"下料"/utf8>>, description => <<"下料"/utf8>>}
    ]
}).

%% 3. 拷机工位 (StationId: 1300)
-define(COMMAND_SET_BURN_IN_2, #{
    station_id => 1300,
    station_name => <<"拷机"/utf8>>,
    commands => [
        #{code => 1, name => <<"拷机测试动作"/utf8>>, description => <<"拷机工位2测试动作"/utf8>>}
    ]
}).

%% 4. 总测工位 (StationId: 1500)
-define(COMMAND_SET_FINAL_TEST_1, #{
    station_id => 1500,
    station_name => <<"总测"/utf8>>,
    commands => [
        #{code => 14, name => <<"折翼"/utf8>>, description => <<"总测工位1折翼"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位1回正"/utf8>>},
        #{code => 2,  name => <<"右滚90"/utf8>>, description => <<"总测工位1右滚90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位1回正"/utf8>>},
        #{code => 3,  name => <<"抬头90"/utf8>>, description => <<"总测工位1抬头90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位1回正"/utf8>>},
        #{code => 4,  name => <<"上升"/utf8>>, description => <<"总测工位1上升"/utf8>>},
        #{code => 5,  name => <<"下降"/utf8>>, description => <<"总测工位1下降"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位1回正"/utf8>>},
        #{code => 6,  name => <<"逆90"/utf8>>, description => <<"总测工位1逆90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位1回正"/utf8>>},
        #{code => 7,  name => <<"慢抬45"/utf8>>, description => <<"总测工位1慢抬45"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位1回正"/utf8>>},
        #{code => 8,  name => <<"抬头90"/utf8>>, description => <<"总测工位1抬头90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位1回正"/utf8>>},
        #{code => 9,  name => <<"低头90"/utf8>>, description => <<"总测工位1低头90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位1回正"/utf8>>},
        #{code => 10, name => <<"顺90"/utf8>>, description => <<"总测工位1顺90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位1回正"/utf8>>},
        #{code => 11, name => <<"逆90"/utf8>>, description => <<"总测工位1逆90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位1回正"/utf8>>},
        #{code => 12, name => <<"右滚90"/utf8>>, description => <<"总测工位1右滚90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位1回正"/utf8>>},
        #{code => 13, name => <<"未知动作"/utf8>>, description => <<"总测工位1未知动作"/utf8>>},
        #{code => 1,  name => <<"测螺旋桨转向"/utf8>>, description => <<"总测工位1测螺旋桨转向"/utf8>>},
        #{code => 16, name => <<"装盖待测"/utf8>>, description => <<"总测工位1装盖待测"/utf8>>},
        #{code => 15, name => <<"开盖下料"/utf8>>, description => <<"总测工位1开盖下料"/utf8>>}
    ]
}).

%% 5. 总测工位 (StationId: 1600)
-define(COMMAND_SET_FINAL_TEST_2, #{
    station_id => 1600,
    station_name => <<"总测"/utf8>>,
    commands => [
        #{code => 14, name => <<"折翼"/utf8>>, description => <<"总测工位2折翼"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位2回正"/utf8>>},
        #{code => 2,  name => <<"右滚90"/utf8>>, description => <<"总测工位2右滚90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位2回正"/utf8>>},
        #{code => 3,  name => <<"抬头90"/utf8>>, description => <<"总测工位2抬头90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位2回正"/utf8>>},
        #{code => 4,  name => <<"上升"/utf8>>, description => <<"总测工位2上升"/utf8>>},
        #{code => 5,  name => <<"下降"/utf8>>, description => <<"总测工位2下降"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位2回正"/utf8>>},
        #{code => 6,  name => <<"逆90"/utf8>>, description => <<"总测工位2逆90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位2回正"/utf8>>},
        #{code => 7,  name => <<"慢抬45"/utf8>>, description => <<"总测工位2慢抬45"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位2回正"/utf8>>},
        #{code => 8,  name => <<"抬头90"/utf8>>, description => <<"总测工位2抬头90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位2回正"/utf8>>},
        #{code => 9,  name => <<"低头90"/utf8>>, description => <<"总测工位2低头90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位2回正"/utf8>>},
        #{code => 10, name => <<"顺90"/utf8>>, description => <<"总测工位2顺90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位2回正"/utf8>>},
        #{code => 11, name => <<"逆90"/utf8>>, description => <<"总测工位2逆90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位2回正"/utf8>>},
        #{code => 12, name => <<"右滚90"/utf8>>, description => <<"总测工位2右滚90"/utf8>>},
        #{code => 1,  name => <<"回正"/utf8>>, description => <<"总测工位2回正"/utf8>>},
        #{code => 13, name => <<"未知动作"/utf8>>, description => <<"总测工位2未知动作"/utf8>>},
        #{code => 1,  name => <<"测螺旋桨转向"/utf8>>, description => <<"总测工位2测螺旋桨转向"/utf8>>},
        #{code => 16, name => <<"装盖待测"/utf8>>, description => <<"总测工位2装盖待测"/utf8>>},
        #{code => 15, name => <<"开盖下料"/utf8>>, description => <<"总测工位2开盖下料"/utf8>>}
    ]
}).

%% 6. 磁航向工位 (StationId: 1700, BaseAddress: 1751)
-define(COMMAND_SET_MAGNETIC_HEADING, #{
    station_id => 1700,
    station_name => <<"磁航向工位"/utf8>>,
    commands => [
        #{code => 1, name => <<"左转"/utf8>>, description => <<"磁航向左转动作"/utf8>>},
        #{code => 2, name => <<"右转"/utf8>>, description => <<"磁航向右转动作"/utf8>>},
        #{code => 3, name => <<"倾斜"/utf8>>, description => <<"磁航向倾斜动作"/utf8>>},
        #{code => 1, name => <<"左转"/utf8>>, description => <<"磁航向左转动作"/utf8>>},
        #{code => 2, name => <<"右转"/utf8>>, description => <<"磁航向右转动作"/utf8>>},
        #{code => 4, name => <<"倾斜"/utf8>>, description => <<"磁航向倾斜动作"/utf8>>},
        #{code => 1, name => <<"左转"/utf8>>, description => <<"磁航向左转动作"/utf8>>},
        #{code => 2, name => <<"右转"/utf8>>, description => <<"磁航向右转动作"/utf8>>}
    ]
}).

%%%===================================================================
%%% 内部函数：根据工位ID获取指令集映射
%%%===================================================================
-spec get_command_set(integer()) -> map() | undefined.
get_command_set(1100) -> ?COMMAND_SET_GANTRY;
get_command_set(1200) -> ?COMMAND_SET_BURN_IN_1;
get_command_set(1300) -> ?COMMAND_SET_BURN_IN_2;
get_command_set(1500) -> ?COMMAND_SET_FINAL_TEST_1;
get_command_set(1600) -> ?COMMAND_SET_FINAL_TEST_2;
get_command_set(1700) -> ?COMMAND_SET_MAGNETIC_HEADING;
get_command_set(_) -> undefined.

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 获取指定工位的指令列表（指令码、名称、描述）
-spec get_station_commands(integer()) -> list(#{code := integer(), name := binary(), description := binary()}).
get_station_commands(StationId) ->
    case get_command_set(StationId) of
        undefined -> [];
        #{commands := Commands} -> Commands
    end.

%% @doc 更新所有工位设备的 PLC 指令集
-spec update_all() -> ok | {error, term()}.
update_all() ->
    Where = #{
        <<"product">> => #{<<"__type">> => <<"Pointer">>, <<"className">> => <<"Product">>, <<"objectId">> => ?STATION_PRODUCT_ID},
        <<"devaddr">> => #{<<"$regex">> => <<"^D">>}
    },
    case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => Where, <<"limit">> => 100}) of
        {ok, #{<<"results">> := Stations}} ->
            Results = lists:map(fun(Station) ->
                DevAddr = maps:get(<<"devaddr">>, Station),
                StationId = case DevAddr of
                    <<"D", Rest/binary>> -> binary_to_integer(Rest);
                    _ -> 0
                end,
                if StationId >= 1100, StationId =< 1700 ->
                    update_station(StationId);
                   true ->
                    ?LOG(warning, <<"跳过无效工位地址: ~s"/utf8>>, [DevAddr]),
                    ok
                end
            end, Stations),
            case lists:filter(fun(R) -> R =/= ok end, Results) of
                [] -> ok;
                Errors ->
                    ?LOG(error, <<"部分工位更新失败: ~p"/utf8>>, [Errors]),
                    {error, partial_failure}
            end;
        {error, Reason} ->
            ?LOG(error, <<"查询工位设备失败: ~p"/utf8>>, [Reason]),
            {error, Reason}
    end.

%% @doc 更新指定工位的 PLC 指令集
-spec update_station(integer()) -> ok | {error, term()}.
update_station(StationId) ->
    DevAddr = <<"D", (integer_to_binary(StationId))/binary>>,
    Where = #{<<"devaddr">> => DevAddr},
    case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => Where}) of
        {ok, #{<<"results">> := [Device | _]}} ->
            ObjectId = maps:get(<<"objectId">>, Device),
            Content = maps:get(<<"content">>, Device, #{}),
            Instructions = maps:get(<<"instructions">>, Content, #{}),
            Commands = get_station_commands(StationId),
            Meanings = maps:from_list([ {integer_to_binary(Code), Desc} || #{code := Code, description := Desc} <- Commands ]),
            NewPLC = #{
                <<"name">> => maps:get(station_name, get_command_set(StationId), <<"工位PLC指令集"/utf8>>),
                <<"meanings">> => Meanings
            },
            NewInstructions = Instructions#{51 => NewPLC},
            NewContent = Content#{<<"instructions">> => NewInstructions},
            case dgiot_parse:update_object(<<"Device">>, ObjectId, #{<<"content">> => NewContent}) of
                {ok, _} ->
                    ?LOG(info, <<"工位 ~s PLC 指令集更新成功"/utf8>>, [DevAddr]),
                    ok;
                {error, Reason} ->
                    ?LOG(error, <<"工位 ~s PLC 指令集更新失败: ~p"/utf8>>, [DevAddr, Reason]),
                    {error, Reason}
            end;
        {ok, #{<<"results">> := []}} ->
            ?LOG(error, <<"未找到工位设备: ~s"/utf8>>, [DevAddr]),
            {error, device_not_found};
        {error, Reason} ->
            ?LOG(error, <<"查询工位 ~s 失败: ~p"/utf8>>, [DevAddr, Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% 测试函数
%%%===================================================================

%% @doc 测试函数 - 验证PLC指令集功能
%% @spec test() -> ok | {error, Reason}
test() ->
    ?LOG(info, <<"【PLC指令集测试】开始测试 ~p"/utf8>>, [?MODULE]),
    
    %% 测试所有工位指令集获取
    StationIds = [1100, 1200, 1300, 1500, 1600, 1700],
    Results = lists:map(fun(StationId) ->
        Commands = get_station_commands(StationId),
        Count = length(Commands),
        ?LOG(info, "工位 ~p: 获取到 ~p 个指令", [StationId, Count]),
        {StationId, Count, Commands}
    end, StationIds),
    
    %% 验证每个工位至少有一个指令
    case lists:filter(fun({_Id, Count, _}) -> Count =:= 0 end, Results) of
        [] ->
            ?LOG(info, <<"【PLC指令集测试】所有工位指令集正常，共测试 ~p 个工位"/utf8>>, [length(Results)]),
            ok;
        EmptyStations ->
            ?LOG(warning, <<"【PLC指令集测试】发现空指令集工位: ~p"/utf8>>, [EmptyStations]),
            {error, {empty_stations, EmptyStations}}
    end.