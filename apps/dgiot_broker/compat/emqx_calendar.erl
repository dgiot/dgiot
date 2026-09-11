%% @doc 同名承接：emqx_calendar（时间工具）→ 自包含实现。
%% format/3,4 是规则引擎 format_date 的底层（strftime 子集），此处按 EMQX
%% 语义实现，日期换算用 OTP calendar 模块（比手写格里历算术更稳）。
-module(emqx_calendar).

-export([offset_second/1, parse/3, now_to_second/0,
         format/3, format/4]).

%% ---- 时区偏移秒（EMQX 语义，对齐 offset_second_/1）----
offset_second(Zone) -> offset_second_(Zone).

offset_second_(OffsetSecond) when is_integer(OffsetSecond) -> OffsetSecond;
offset_second_(undefined) -> 0;
offset_second_("local") -> offset_second_(local);
offset_second_(<<"local">>) -> offset_second_(local);
offset_second_(utc) -> 0;
offset_second_(<<"UTC">>) -> 0;
offset_second_(local) ->
    UniversalTime = calendar:system_time_to_universal_time(erlang:system_time(second), second),
    LocalTime = erlang:universaltime_to_localtime(UniversalTime),
    LocalSecs = calendar:datetime_to_gregorian_seconds(LocalTime),
    UniversalSecs = calendar:datetime_to_gregorian_seconds(UniversalTime),
    LocalSecs - UniversalSecs;
offset_second_(Offset) when is_binary(Offset) ->
    offset_second_(erlang:binary_to_list(Offset));
offset_second_("Z") -> 0;
offset_second_("z") -> 0;
offset_second_(Offset) when is_list(Offset) ->
    Sign = hd(Offset),
    ((Sign == $+) orelse (Sign == $-)) orelse erlang:error({bad_time_offset, Offset}),
    Signs = #{$+ => 1, $- => -1},
    PosNeg = maps:get(Sign, Signs),
    [Sign | HM] = Offset,
    {HourStr, MinuteStr, SecondStr} =
        case string:tokens(HM, ":") of
            [H, M] -> {H, M, "0"};
            [H, M, S] -> {H, M, S};
            [HHMM] when length(HHMM) == 4 ->
                {string:sub_string(HHMM, 1, 2), string:sub_string(HHMM, 3, 4), "0"};
            _ -> erlang:error({bad_time_offset, Offset})
        end,
    Hour = list_to_integer(HourStr),
    Minute = list_to_integer(MinuteStr),
    Second = list_to_integer(SecondStr),
    (Hour =< 23) orelse erlang:error({bad_time_offset_hour, Hour}),
    (Minute =< 59) orelse erlang:error({bad_time_offset_minute, Minute}),
    (Second =< 59) orelse erlang:error({bad_time_offset_second, Second}),
    PosNeg * (Hour * 3600 + Minute * 60 + Second).

%% ---- format/3,4：strftime 子集（%Y %m %d %H %M %S %N %3N %6N %z %:z %::z）----
format(Time, Unit, Formatter) ->
    format(Time, Unit, undefined, Formatter).

format(Time, Unit, Offset, FormatterBin) when is_binary(FormatterBin) ->
    format(Time, Unit, Offset, formatter(FormatterBin));
format(Time, Unit, Offset, Formatter) ->
    do_format(Time, time_unit(Unit), offset_second_(Offset), Formatter).

formatter(FormatterStr) when is_list(FormatterStr) ->
    formatter(list_to_binary(FormatterStr));
formatter(FormatterBin) when is_binary(FormatterBin) ->
    do_formatter(FormatterBin, []).

do_formatter(<<>>, Formatter) -> lists:reverse(Formatter);
do_formatter(<<"%Y", Tail/binary>>, Formatter) -> do_formatter(Tail, [year | Formatter]);
do_formatter(<<"%m", Tail/binary>>, Formatter) -> do_formatter(Tail, [month | Formatter]);
do_formatter(<<"%d", Tail/binary>>, Formatter) -> do_formatter(Tail, [day | Formatter]);
do_formatter(<<"%H", Tail/binary>>, Formatter) -> do_formatter(Tail, [hour | Formatter]);
do_formatter(<<"%M", Tail/binary>>, Formatter) -> do_formatter(Tail, [minute | Formatter]);
do_formatter(<<"%S", Tail/binary>>, Formatter) -> do_formatter(Tail, [second | Formatter]);
do_formatter(<<"%N", Tail/binary>>, Formatter) -> do_formatter(Tail, [nanosecond | Formatter]);
do_formatter(<<"%3N", Tail/binary>>, Formatter) -> do_formatter(Tail, [millisecond | Formatter]);
do_formatter(<<"%6N", Tail/binary>>, Formatter) -> do_formatter(Tail, [microsecond | Formatter]);
do_formatter(<<"%z", Tail/binary>>, Formatter) -> do_formatter(Tail, [timezone | Formatter]);
do_formatter(<<"%:z", Tail/binary>>, Formatter) -> do_formatter(Tail, [timezone1 | Formatter]);
do_formatter(<<"%::z", Tail/binary>>, Formatter) -> do_formatter(Tail, [timezone2 | Formatter]);
do_formatter(<<Char:8, Tail/binary>>, [Str | Formatter]) when is_list(Str) ->
    do_formatter(Tail, [lists:append(Str, [Char]) | Formatter]);
do_formatter(<<Char:8, Tail/binary>>, Formatter) ->
    do_formatter(Tail, [[Char] | Formatter]).

do_format(Time, Unit, Offset, Formatter) ->
    Adjustment = erlang:convert_time_unit(Offset, second, Unit),
    AdjustedTime = Time + Adjustment,
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:system_time_to_universal_time(AdjustedTime, Unit),
    Date = #{
        year => padding(Year, 4),
        month => padding(Month, 2),
        day => padding(Day, 2),
        hour => padding(Hour, 2),
        minute => padding(Min, 2),
        second => padding(Sec, 2),
        millisecond => trans_x_second(Unit, millisecond, Time),
        microsecond => trans_x_second(Unit, microsecond, Time),
        nanosecond => trans_x_second(Unit, nanosecond, Time)
    },
    Timezones = formatter_timezones(Offset, Formatter, #{}),
    DateWithZone = maps:merge(Date, Timezones),
    [maps:get(Key, DateWithZone, Key) || Key <- Formatter].

formatter_timezones(_Offset, [], Zones) -> Zones;
formatter_timezones(Offset, [Timezone | Formatter], Zones) ->
    case lists:member(Timezone, [timezone, timezone1, timezone2]) of
        true ->
            NZones = Zones#{Timezone => offset_to_timezone(Offset, Timezone)},
            formatter_timezones(Offset, Formatter, NZones);
        false ->
            formatter_timezones(Offset, Formatter, Zones)
    end.

offset_to_timezone(Offset, Timezone) ->
    Sign = case Offset >= 0 of true -> $+; false -> $- end,
    {H, M, S} = seconds_to_time(abs(Offset)),
    case Timezone of
        timezone -> io_lib:format("~c~2.10.0B~2.10.0B", [Sign, H, M]);
        timezone1 -> io_lib:format("~c~2.10.0B:~2.10.0B", [Sign, H, M]);
        timezone2 -> io_lib:format("~c~2.10.0B:~2.10.0B:~2.10.0B", [Sign, H, M, S])
    end.

seconds_to_time(Secs) ->
    Hour = Secs div 3600,
    Minute = (Secs rem 3600) div 60,
    Second = Secs rem 60,
    {Hour, Minute, Second}.

time_unit(second) -> second;
time_unit(millisecond) -> millisecond;
time_unit(microsecond) -> microsecond;
time_unit(nanosecond) -> nanosecond;
time_unit("second") -> second;
time_unit("millisecond") -> millisecond;
time_unit("microsecond") -> microsecond;
time_unit("nanosecond") -> nanosecond;
time_unit(<<"second">>) -> second;
time_unit(<<"millisecond">>) -> millisecond;
time_unit(<<"microsecond">>) -> microsecond;
time_unit(<<"nanosecond">>) -> nanosecond.

trans_x_second(FromUnit, ToUnit, Time) ->
    XSecond = do_trans_x_second(FromUnit, ToUnit, Time),
    Len = case ToUnit of
              millisecond -> 3;
              microsecond -> 6;
              nanosecond -> 9
          end,
    padding(XSecond, Len).

do_trans_x_second(second, _, _Time) -> 0;
do_trans_x_second(millisecond, millisecond, Time) -> Time rem 1000;
do_trans_x_second(millisecond, microsecond, Time) -> (Time rem 1000) * 1000;
do_trans_x_second(millisecond, nanosecond, Time) -> (Time rem 1000) * 1000000;
do_trans_x_second(microsecond, millisecond, Time) -> Time div 1000 rem 1000;
do_trans_x_second(microsecond, microsecond, Time) -> Time rem 1000000;
do_trans_x_second(microsecond, nanosecond, Time) -> (Time rem 1000000) * 1000;
do_trans_x_second(nanosecond, millisecond, Time) -> Time div 1000000 rem 1000;
do_trans_x_second(nanosecond, microsecond, Time) -> Time div 1000 rem 1000000;
do_trans_x_second(nanosecond, nanosecond, Time) -> Time rem 1000000000.

padding(Data, Len) when is_integer(Data) -> padding(integer_to_list(Data), Len);
padding(Data, Len) when Len > 0 andalso length(Data) < Len -> [$0 | padding(Data, Len - 1)];
padding(Data, _Len) -> Data.

%% ---- parse/3：占位（EMQX 里是 cron/窗口解析，本项目未用）----
parse(_Unit, _Field, _Value) ->
    {error, {not_implemented, cut7, calendar_parse}}.

now_to_second() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.
