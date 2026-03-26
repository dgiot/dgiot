%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_mes_utils - 无人机MES上报工具函数
%%%
%%% 提供时间戳生成、事务ID、数据验证等通用辅助函数。
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_mes_utils).

%% API
-export([
    get_current_timestamp/0,
    format_timestamp/1,
    generate_mes_trans_id/0,
    validate_mes_data/1,
    validate_production_data/1,
    generate_fault_code/1,
    get_line_proc_no/1,
    format_test_parameters/1
]).

-include_lib("dgiot/include/logger.hrl").

%%%===================================================================
%%% 时间相关
%%%===================================================================

-spec get_current_timestamp() -> binary().
get_current_timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    TimestampStr = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", 
                                [Year, Month, Day, Hour, Minute, Second]),
    list_to_binary(TimestampStr).

-spec format_timestamp(integer()) -> binary().
format_timestamp(Timestamp) when is_integer(Timestamp) andalso Timestamp > 10000000000 ->
    %% 处理 YYYYMMDDHHMMSS 格式的时间戳
    Year = Timestamp div 10000000000,
    Month = (Timestamp div 100000000) rem 100,
    Day = (Timestamp div 1000000) rem 100,
    Hour = (Timestamp div 10000) rem 100,
    Minute = (Timestamp div 100) rem 100,
    Second = Timestamp rem 100,
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", 
                  [Year, Month, Day, Hour, Minute, Second]);
format_timestamp(Timestamp) when is_integer(Timestamp) ->
    %% 处理 Unix 时间戳（秒）
    DateTime = calendar:gregorian_seconds_to_datetime(Timestamp + 62167219200),
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", 
                  [Year, Month, Day, Hour, Minute, Second]).

-spec generate_mes_trans_id() -> binary().
generate_mes_trans_id() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    BaseTimestamp = Year * 10000000000 + Month * 100000000 + Day * 1000000 + 
                    Hour * 10000 + Minute * 100 + Second,
    RandomPart = rand:uniform(999999),
    TransId = io_lib:format("~14..0B~6..0B", [BaseTimestamp, RandomPart]),
    list_to_binary(TransId).

%%%===================================================================
%%% 数据验证
%%%===================================================================

-spec validate_mes_data(map()) -> boolean().
validate_mes_data(#{<<"func_id">> := FuncId, <<"line_no">> := LineNo, 
                    <<"line_sta">> := LineSta, <<"date_time">> := DateTime}) ->
    is_binary(FuncId) andalso 
    is_binary(LineNo) andalso
    is_integer(LineSta) andalso LineSta >= 1 andalso LineSta =< 5 andalso
    is_binary(DateTime);
validate_mes_data(_) ->
    false.

-spec validate_production_data(map()) -> boolean().
validate_production_data(DataRecord) ->
    RequiredFields = [<<"drone_no">>, <<"drone_type">>, <<"line_proc_no">>, <<"eqp_action_list">>],
    lists:all(fun(Field) -> maps:is_key(Field, DataRecord) end, RequiredFields).

%%%===================================================================
%%% 工具函数
%%%===================================================================

-spec generate_fault_code(integer()) -> binary().
generate_fault_code(FaultLevel) when FaultLevel >= 1, FaultLevel =< 3 ->
    RandomCode = rand:uniform(999),
    CodeStr = io_lib:format("GZ-~p-~3..0B", [FaultLevel, RandomCode]),
    list_to_binary(CodeStr).

-spec get_line_proc_no(integer()) -> binary().
get_line_proc_no(LineNo) ->
    case LineNo of
        1 -> <<"A-JC-03">>;
        2 -> <<"B-JC-03">>;
        _ -> <<"未知工位">>
    end.

-spec format_test_parameters(map()) -> list().
format_test_parameters(TestResults) ->
    Parameters = maps:get(<<"parameters">>, TestResults, []),
    lists:map(fun(Param) ->
        #{
            <<"p_name">> => maps:get(<<"name">>, Param, <<"未知参数">>),
            <<"p_upper">> => maps:get(<<"upper">>, Param, 0),
            <<"p_lower">> => maps:get(<<"lower">>, Param, 0),
            <<"p_stan">> => maps:get(<<"standard">>, Param, 0),
            <<"p_value">> => maps:get(<<"value">>, Param, 0),
            <<"p_result">> => maps:get(<<"result">>, Param, <<"未知">>)
        }
    end, Parameters).