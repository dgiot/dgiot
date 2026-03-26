%%--------------------------------------------------------------------
%% @doc 治具指标数据转换模块
%%--------------------------------------------------------------------
-module(dgiot_fixture_metrics).
-author("johnliu").

-export([
    registers_to_resistance/1,
    register_to_voltage/1,
    resistance_to_registers/1,
    voltage_to_register/1,
    parse_metrics/1,
    format_metrics/1
]).

%%====================================================================
%% 转换函数
%%====================================================================
registers_to_resistance([High, Low]) -> (High bsl 16) bor Low.
register_to_voltage([Value]) -> Value.

resistance_to_registers(Res) ->
    [(Res bsr 16) band 16#FFFF, Res band 16#FFFF].

voltage_to_register(Vol) ->
    [Vol band 16#FFFF].

%%====================================================================
%% 解析与格式化
%%====================================================================
parse_metrics(Registers) ->
    case Registers of
        [H, L] -> #{type => resistance, value => registers_to_resistance([H, L]), raw => Registers};
        [V] -> #{type => voltage, value => register_to_voltage([V]), raw => Registers};
        _ -> #{type => unknown, raw => Registers}
    end.

format_metrics(#{type := resistance, value := V}) ->
    io_lib:format("电阻: ~.2f Ω", [V / 1000]);
format_metrics(#{type := voltage, value := V}) ->
    io_lib:format("电压: ~.2f V", [V / 1000]);
format_metrics(_) ->
    "未知指标".
