
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

-module(dgiot_datetime).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-logger_header("[dgiot]").

-export([
    start_time/0,
    get_iso_8601/1,
    get_iso_8601_loacl/0,
    timezone/0,
    nowstamp/0,
    local_time/0,
    localtime_to_unixtime/1,
    unixtime_to_localtime/1,
    to_unixtime/1,
    to_localtime/1,
    format/1,
    format/2,
    timestamp_to_datetime/1
]).

-export([seed/0
    , utc/0
    , utc/1
    , now_secs/0
    , now_secs/1
    , now_ms/0
    , now_ms/1
    , now_microsecs/0
    , ts_from_ms/1
    , day_from/0
    , day_from/1
    , month_from/0
    , month_from/1
    , timestamp_from/1
    , hour_from/0
    , hour_from/1
    , timestamp/0
    , last_month/1
    , get_today_stamp/0
    , get_today_stamp/1
]).

-define(MS_ONE_DAY, 86400000).

-import(dgiot_utils, [to_int/1, to_list/1, to_binary/1, tokens/2]).
-define(TIMEZONE, + 8).


get_iso_8601_loacl() ->
    {{Y1, M1, D1}, {H1, Mm1, S1}} = calendar:local_time(),
    F =
        fun(Num) ->
            NumL = to_list(Num),
            case length(NumL) of
                1 ->
                    "0" ++ NumL;
                _ ->
                    NumL
            end
        end,
    [Y, M, D, H, Mn, S] = [F(Num) || Num <- [Y1, M1, D1, H1, Mm1, S1]],
    lists:concat([Y, "-", M, "-", D, "T", H, ":", Mn, ":", S]).



get_iso_8601(Expire_syncpoint) ->
    utc(Expire_syncpoint).

utc() ->
    utc(nowstamp()).

utc(TimeStamp) ->
    {{Y1, M1, D1}, {H1, Mm1, S1}} = unixtime_to_localtime(TimeStamp - ?TIMEZONE * 3600),
    F =
        fun(Num) ->
            NumL = to_list(Num),
            case length(NumL) of
                1 ->
                    "0" ++ NumL;
                _ ->
                    NumL
            end
        end,
    [Y, M, D, H, Mn, S] = [F(Num) || Num <- [Y1, M1, D1, H1, Mm1, S1]],
    lists:concat([Y, "-", M, "-", D, "T", H, ":", Mn, ":", S, "Z"]).


timestamp() ->
    {{Y1, M1, D1}, {H1, Mm1, S1}} = unixtime_to_localtime(nowstamp() - ?TIMEZONE * 3600),
    F =
        fun(Num) ->
            NumL = to_list(Num),
            case length(NumL) of
                1 ->
                    "0" ++ NumL;
                _ ->
                    NumL
            end
        end,
    [Y, M, D, H, Mn, S] = [F(Num) || Num <- [Y1, M1, D1, H1, Mm1, S1]],
    {_, _, Ms} = os:timestamp(),
    lists:concat([Y, "-", M, "-", D, " ", H, ":", Mn, ":", S, ".", Ms div 1000]).


seed() ->
    rand:seed(exsplus, erlang:timestamp()).

start_time() ->
    case application:get_all_env(dgiot) of
        Env when length(Env) == 0 ->
            0;
        _ ->
            case dgiot_data:get(dgiot_system_startime) of
                not_find ->
                    dgiot_data:insert(dgiot_system_startime, dgiot_datetime:now_secs()),
                    0;
                Value ->
                    dgiot_datetime:now_secs() - Value
            end
    end.

now_secs() ->
    erlang:system_time(second).

now_secs({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.

-spec now_ms() -> integer().
now_ms() ->
    erlang:system_time(millisecond).

now_ms({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000 + round(MicroSecs / 1000).

ts_from_ms(Ms) ->
    {Ms div 1000000, Ms rem 1000000, 0}.

% 获取时区 默认 +8区
timezone() ->
    dgiot:get_env(timezone, ?TIMEZONE).

% 当前Unix时间戳 1518057233
nowstamp() ->
    localtime_to_unixtime(local_time()).

local_time() ->
    calendar:local_time().

% 将本地时间转为Unix时间
localtime_to_unixtime(LocalTime) ->
    calendar:datetime_to_gregorian_seconds(LocalTime) - timezone() * 60 * 60 - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).

% Unix时间转本地时间 {{2018,2,8},{10,33,53}}
unixtime_to_localtime(NowStamp) ->
    calendar:gregorian_seconds_to_datetime(NowStamp + calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}) + timezone() * 60 * 60).

%%"2018-12-12" -> {2018,12,12}
%%"2018/12/12" -> {2018,12,12}
%%"2018-12-12 12:12:12" -> {{2018,12,12},{12,12,12}}
%%"2018/12/12 12:12:12" -> {{2018,12,12},{12,12,12}}
%%"12:12:12" -> {12,12,12}
%%2021-07-22T12:26:53.216Z
to_localtime(Time) when is_tuple(Time) ->
    Time;
to_localtime(NowStamp) when is_integer(NowStamp) ->
    unixtime_to_localtime(NowStamp);
to_localtime(<<Y:4/bytes, "-", M:2/bytes, "-", D:2/bytes, "T", H:2/bytes, ":", N:2/bytes, ":", S:2/bytes, ".", _/binary>>) ->
    Data = {{to_int(Y), to_int(M), to_int(D)}, {to_int(H), to_int(N), to_int(S)}},
    Ms = localtime_to_unixtime(Data) + timezone() * 60 * 60,
    unixtime_to_localtime(Ms);
to_localtime(DateTimeBin) when is_binary(DateTimeBin) ->
    to_localtime(to_list(DateTimeBin));
to_localtime(DateTimeStr) when is_list(DateTimeStr) ->
    case tokens(DateTimeStr, [" "]) of
        [DateOrTime] ->
            case tokens(DateOrTime, ["-", "/", "."]) of
                [Y, M, D] ->
                    {to_int(Y), to_int(M), to_int(D)};
                [UnixOrTime] ->
                    case tokens(UnixOrTime, [":"]) of
                        [H, N, S] ->
                            {to_int(H), to_int(N), to_int(S)};
                        [UnixT] ->
                            to_localtime(to_int(UnixT))
                    end
            end;
        [Date, Time | _] ->
            [Y, M, D] = tokens(Date, ["-", "/", "."]),
            [H, N, S] = tokens(Time, [":"]),
            {{to_int(Y), to_int(M), to_int(D)}, {to_int(H), to_int(N), to_int(S)}}
    end.

format(Format) ->
    format(local_time(), Format).
format(DateTime, Format) ->
    {{Y, M, D}, {H, N, S}} = to_localtime(DateTime),
    L = [{"Y", Y}, {"M", M}, {"D", D}, {"H", H}, {"N", N}, {"S", S}],
    format2(to_binary(Format), L, Format).

format2(Format, L, Result) when Format == <<>>; L == [] ->
    Result;
format2(<<"YY", Other/binary>>, L, Format) ->
    format2(<<"Y", Other/binary>>, L, Format);
format2(<<"Y", Other/binary>>, L, Format) ->
    V = proplists:get_value("Y", L),
    NewFormat = re:replace(Format, "Y{1,4}", to_list(V), [{return, binary}]),
    format2(Other, proplists:delete("Y", L), NewFormat);
format2(<<F:1/bytes, F:1/bytes, Other/binary>>, L, Format) ->
    case lists:member(binary_to_list(F), ["M", "D", "H", "N", "S"]) of
        true ->
            V = proplists:get_value(binary_to_list(F), L),
            New = io_lib:format("~2.10.0B", [V]),
            NewFormat = re:replace(Format, <<F/binary, "{1,2}">>, to_list(New), [{return, binary}]),
            format2(Other, proplists:delete(F, L), NewFormat);
        false ->
            format2(Other, L, Format)
    end;
format2(<<F:1/bytes, Other/binary>>, L, Format) ->
    case lists:member(binary_to_list(F), ["M", "D", "H", "N", "S"]) of
        true ->
            V = proplists:get_value(binary_to_list(F), L),
            NewFormat = re:replace(Format, <<F/binary, "{1,2}">>, to_list(V), [{return, binary}]),
            format2(Other, proplists:delete(F, L), NewFormat);
        false ->
            format2(Other, L, Format)
    end.

to_unixtime(Time) when is_integer(Time) ->
    Time;
to_unixtime(Time) when is_binary(Time) ->
    Size = byte_size(Time) - 4,
    <<DateTime:Size/binary, _/binary>> = Time,
    to_unixtime(to_localtime(DateTime));
to_unixtime(LocalTime) ->
    localtime_to_unixtime(LocalTime).

-spec now_microsecs() -> integer().
now_microsecs() ->
    now_microsecs(os:timestamp()).
-spec now_microsecs({integer(), integer(), integer()}) -> integer().
now_microsecs({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.

day_from() ->
    day_from(now_ms()).

-spec day_from(any()) -> integer().
day_from(Timestamp) when is_integer(Timestamp) ->
    {{Year, Month, Day}, _} = timestamp_to_datetime(Timestamp),
    day_from({Year, Month, Day});
day_from({Year, Month, Day}) ->
    [UniversalDatatime] = calendar:local_time_to_universal_time_dst({{Year, Month, Day}, {0, 0, 0}}),
    datetime_to_gregorian_ms(UniversalDatatime);
day_from({{Year, Month, Day}, _}) ->
    day_from({Year, Month, Day});
day_from(Arg) ->
    ?LOG(error, "dgiot_datatime:day_from - bad arg [~p]", [Arg]),
    0.

month_from() ->
    month_from(now_ms()).

-spec month_from(integer()) -> integer().
month_from(Timestamp) when is_integer(Timestamp) ->
    {{Year, Month, _}, _} = timestamp_to_datetime(Timestamp),
    [UniversalDatatime] = calendar:local_time_to_universal_time_dst({{Year, Month, 1}, {0, 0, 0}}),
    datetime_to_gregorian_ms(UniversalDatatime);
month_from({Year, Month, _}) ->
    [UniversalDatatime] = calendar:local_time_to_universal_time_dst({{Year, Month, 1}, {0, 0, 0}}),
    datetime_to_gregorian_ms(UniversalDatatime);
month_from({{Year, Month, _}, _}) ->
    month_from({Year, Month, 1});
month_from(Arg) ->
    ?LOG(error, "dgiot_datatime:month_from - bad arg [~p]", [Arg]),
    0.

hour_from() ->
    hour_from(now_ms()).

-spec hour_from(integer()) -> integer().
hour_from(Timestamp) when is_integer(Timestamp) ->
    {{Year, Month, Day}, {Hour, _, _}} = timestamp_to_datetime(Timestamp),
    [UniversalDatatime] = calendar:local_time_to_universal_time_dst({{Year, Month, Day}, {Hour, 0, 0}}),
    datetime_to_gregorian_ms(UniversalDatatime);
hour_from(Arg) ->
    ?LOG(error, "dgiot_datatime:month_from - bad arg [~p]", [Arg]),
    0.

-spec timestamp_from(tuple()) -> integer().
timestamp_from({day, Datetime}) ->
    day_from(Datetime);
timestamp_from({"day", Datetime}) ->
    day_from(Datetime);
timestamp_from({<<"day">>, Datetime}) ->
    day_from(Datetime);
timestamp_from({month, Datetime}) ->
    month_from(Datetime);
timestamp_from({"month", Datetime}) ->
    month_from(Datetime);
timestamp_from({<<"month">>, Datetime}) ->
    month_from(Datetime);
timestamp_from({hour, Datetime}) ->
    hour_from(Datetime);
timestamp_from({"hour", Datetime}) ->
    hour_from(Datetime);
timestamp_from({<<"hour">>, Datetime}) ->
    hour_from(Datetime);
timestamp_from(CycleDatetime) ->
    ?LOG(error, "dgiot_datatime:timestamp_from - bad arg [~p]", [CycleDatetime]),
    0.

-spec datetime_to_gregorian_ms(tuple()) -> integer().
datetime_to_gregorian_ms(Datetime) ->
    1000 * (calendar:datetime_to_gregorian_seconds(Datetime) -
        calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})).

-spec timestamp_to_datetime(integer()) -> {{integer(), integer(), integer()}, {integer(), integer(), integer()}}.
timestamp_to_datetime(Timestamp) ->
    calendar:universal_time_to_local_time(
        calendar:gregorian_seconds_to_datetime(Timestamp div 1000 +
            calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}))).

last_month(Count) ->
    {{Year, Month, Day}, {_Hour, _Minute, _Second}} = calendar:local_time(),
    EndTime = dgiot_datetime:localtime_to_unixtime({{Year, Month, Day}, {23, 59, 59}}),
    StartTime = dgiot_datetime:localtime_to_unixtime({{Year, Month, 1}, {0, 0, 0}}),
    last_month(StartTime, EndTime, Count - 1).

last_month(StartTime, EndTime, 0) ->
    {StartTime * 1000, EndTime * 1000};

last_month(StartTime, EndTime, Count) ->
    {{Year, Month, _Day}, {_Hour, _Minute, _Second}} = dgiot_datetime:unixtime_to_localtime(StartTime),
    {NewYear, NewMonth} =
        case Month of
            1 ->
                {Year - 1, 12};
            _ ->
                {Year, Month - 1}
        end,
    NewStartTime = dgiot_datetime:localtime_to_unixtime({{NewYear, NewMonth, 1}, {0, 0, 0}}),
    last_month(NewStartTime, EndTime, Count - 1).

get_today_stamp() ->
    {{Year, Month, Day}, _} = local_time(),
    dgiot_datetime:localtime_to_unixtime({{Year, Month, Day}, {0, 0, 0}}).
get_today_stamp(Date) when is_integer(Date) ->
    {{Year, Month, Day}, _} = to_localtime(Date),
    dgiot_datetime:localtime_to_unixtime({{Year, Month, Day}, {0, 0, 0}});
get_today_stamp(Date) ->
    Date.
