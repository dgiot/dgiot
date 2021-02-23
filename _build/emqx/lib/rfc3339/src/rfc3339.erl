% vim: sw=4 ts=4 et ft=erlang
-module(rfc3339).

-export([parse/1,parse_to_local_datetime/1]).
-export([format/1, format/2]).
-export([to_map/1]).
-export([to_time/1, to_time/2]).
-export_type([(error/0)]).

-type datetime() :: {date(), time()}.

-type date() :: {year(), month(), day()}.
-type year() :: 0..9999.
-type month() :: 1..12.
-type day() :: 1..31.

-type time() :: {hour(), min(), sec()}.
-type hour() :: 0..24.
-type min() :: 0..59.
-type sec() :: 0..60.

-type usec() :: 0..999999.

-type tz() :: tz_offset().
-type tz_offset() :: -1439..1439.

-type error() :: badarg | baddate | badtime | badyear | badday | badhour | badminute | badsecond | badusec | badtimezone.

-record(rfc3339, { year :: integer()
                 , month :: integer()
                 , day :: integer()
                 , hour :: integer()
                 , min :: integer()
                 , sec :: integer()
                 , usec :: integer()
                 , tz_offset :: integer()
                 }).
-type rfc3339() :: #rfc3339{}.

%% -spec parse_to_local_datetime(binary()) -> {date(), time()}
parse_to_local_datetime(Bin) ->
    {ok, {Date, Time, _,  TZ}} = parse(Bin),
    TZSecs = calendar:datetime_to_gregorian_seconds({Date, Time}),
    UTCDateTime = calendar:gregorian_seconds_to_datetime(case TZ of
                                                             _ when is_integer(TZ) ->
                                                                 TZSecs + (60*TZ);
                                                             _ ->
                                                                 TZSecs
                                                         end),
    calendar:universal_time_to_local_time(UTCDateTime).

-spec parse(binary()) -> {ok, {date(), time(), usec(), tz()}} | {error, error()}.
parse(Bin) when is_binary(Bin) -> date(Bin, {undefined, undefined, undefined, undefined});
parse(_) -> {error, badarg}.

-spec to_map(binary()) -> {ok, rfc3339()} | {error, error()}.
to_map(Bin) when is_binary(Bin) ->
  case parse(Bin) of
    {ok, {Date, Time, USec, Tz}} -> mapify(Date, Time, USec, Tz, #rfc3339{});
    {error, Error}               -> {error, Error}
  end;
to_map(_) -> {error, badarg}.

-spec to_time(binary()) -> {ok, integer()} | {error, error()}.
to_time(Bin) when is_binary(Bin) -> to_time(Bin, native).

-spec to_time(binary(), erlang:time_unit()) -> {ok, integer()} | {error, error()}.
to_time(Bin, Unit) when is_binary(Bin) ->
  case parse(Bin) of
    {ok, {Date, {Hour, Min, Sec}, USec, Tz}} ->
      Epoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
      Time = {Hour, Min + or_zero(Tz), Sec},
      GregorianSeconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
      U = convert_time_unit(or_zero(USec), micro_seconds, Unit),
      {ok, convert_time_unit(GregorianSeconds - Epoch, seconds, Unit) + U};
    {error, Error} -> {error, Error}
  end.


mapify({Year, Month, Day}, Time, USec, Tz, Result)
when is_integer(Year), is_integer(Month), is_integer(Day) ->
  mapify(Time, USec, Tz, Result#rfc3339{year = Year, month = Month, day = Day});
mapify(_, _, _, _, _) -> {error, badarg}.

mapify({Hour, Min, Sec}, USec, Tz, Result)
when is_integer(Hour), is_integer(Min), is_integer(Sec) ->
  mapify(USec, Tz, Result#rfc3339{hour = Hour, min = Min, sec = Sec});
mapify(_, _, _, _) -> {error, badarg}.

mapify(undefined, Tz, Result) -> mapify(Tz, Result);
mapify(USec, Tz, Result) when is_integer(USec) ->
  mapify(Tz, Result#rfc3339{usec = USec});
mapify(_, _, _) -> {error, badarg}.

mapify(undefined, Result) -> Result;
mapify(Tz, Result) when is_integer(Tz) -> Result#rfc3339{tz_offset = Tz};
mapify(_, _) -> {error, badarg}.

mapify(Time) when is_integer(Time) ->
  Epoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  GregorianSeconds = Time div 1000000 + Epoch,
  {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:gregorian_seconds_to_datetime(GregorianSeconds),
  USec = Time rem 1000000,
  #rfc3339{year = Year, month = Month, day = Day, hour = Hour, min = Min, sec = Sec, usec = USec};
mapify(_) -> {error, badarg}.

-spec format(rfc3339() | {date(), time(), usec(), tz()} | datetime() | integer()) -> {ok, binary()} | {error, error()}.
format({Date, Time, USec, Tz})
when is_tuple(Date), is_tuple(Time) ->
  format(mapify(Date, Time, USec, Tz, #rfc3339{}));
format({Date, Time})
when is_tuple(Date), is_tuple(Time) ->
  format(mapify(Date, Time, undefined, undefined, #rfc3339{}));
format(Time) when is_integer(Time) ->
  %% USec is the greatest fidelity supported. nano seconds are converted lossily
  USec = convert_time_unit(Time, native, micro_seconds),
  format(mapify(USec));
format(#rfc3339{} = Dt) ->
  Date = format_date(Dt),
  Time = format_time(Dt),
  {ok, format_(Date, Time)}.

-spec format(integer(), erlang:time_unit()) -> {ok, binary()} | {error, error()}.
format(Time, Unit) when is_integer(Time) ->
  USec = convert_time_unit(Time, Unit, micro_seconds),
  format(mapify(USec)).


date(<<Y1, Y2, Y3, Y4, $-, M1, M2, $-, D1, D2, Rest/binary>>, {undefined, undefined, undefined, undefined}) ->
  Year = to_year(Y1, Y2, Y3, Y4),
  Month = to_month(M1, M2),
  Day = to_day(D1, D2, Year, Month),
  Date = coalesce(Year, Month, Day),
  time(Rest, {Date, undefined, undefined, undefined});
date(_, _Result) ->
  {error, baddate}.

%% space
time(_, {{error, Error}, _, _, _}) -> {error, Error};
time(<<Sep, H1, H2, $:, M1, M2, $:, S1, S2, Rest/binary>>, {Date, undefined, undefined, undefined})
when Sep == 16#20 orelse Sep == $t orelse Sep == $T ->
  Hour = to_hour(H1, H2),
  Min = to_minute(M1, M2),
  Sec = to_second(S1, S2),
  Time = coalesce(Hour, Min, Sec),
  usec_or_tz(Rest, {Date, Time, undefined, undefined});
time(_, _) -> {error, badtime}.

usec_or_tz(_, {_, {error, Error}, _, _}) -> {error, Error};
usec_or_tz(<<$., Rest/binary>>, Result) ->
  usec(Rest, Result, 0, 100000);
usec_or_tz(Rest, Result) -> tz(Rest, Result).

%% next two clauses burn off fractional seconds beyond microsecond precision
usec(<<X, Rest/binary>>, Result, USec, 0)
when X >= $0 andalso X =< $9 ->
  usec(Rest, Result, USec, 0);
%% keep a running acc of usecs
usec(<<X, Rest/binary>>, Result, USec, Multiplier)
when X >= $0 andalso X =< $9 ->
  try list_to_integer([X]) of
    N -> usec(Rest, Result, USec + (N * Multiplier), Multiplier div 10)
  catch
    error:badarg -> {error, badusec}
  end;
%% not a digit, insert usecs into time and proceed to tz
usec(Bin, {Date, Time, undefined, undefined}, USec, _) ->
  tz(Bin, {Date, Time, USec, undefined});
usec(_, _, _, _) -> {error, badusec}.

tz(<<$+, H1, H2, $:, M1, M2>>, {Date, Time, USec, undefined}) ->
  Hour = to_hour(H1, H2),
  Min = to_minute(M1, M2),
  case calc_tz(positive, Hour, Min) of
    TZ when is_integer(TZ) -> {ok, {Date, Time, USec, TZ}};
    {error, Error}        -> {error, Error}
  end;
tz(<<$-, H1, H2, $:, M1, M2>>, {Date, Time, USec, undefined}) ->
  Hour = to_hour(H1, H2),
  Min = to_minute(M1, M2),
  case calc_tz(negative, Hour, Min) of
    TZ when is_integer(TZ) -> {ok, {Date, Time, USec, TZ}};
    {error, Error}        -> {error, Error}
  end;
tz(<<$Z>>, Result) ->
  {ok, Result};
tz(<<$z>>, Result) ->
  {ok, Result};
tz(_, _) -> {error, badtimezone}.

calc_tz(_, {error, Error}, _) -> {error, Error};
calc_tz(_, _, {error, Error}) -> {error, Error};
calc_tz(positive, Hour, Min) -> (Hour * 60) + Min;
calc_tz(negative, Hour, Min) -> (Hour * -60) - Min.

coalesce({error, Error}, _, _) -> {error, Error};
coalesce(_, {error, Error}, _) -> {error, Error};
coalesce(_, _, {error, Error}) -> {error, Error};
coalesce(X, Y, Z) -> {X, Y, Z}.

to_year(Y1, Y2, Y3, Y4) ->
  try list_to_integer([Y1, Y2, Y3, Y4]) of
    Year when Year >= 0 andalso Year =< 9999 -> Year;
    _ -> {error, badyear}
  catch
    error:badarg -> {error, badyear}
  end.

to_month(M1, M2) ->
  try list_to_integer([M1, M2]) of
    Month when Month >= 1 andalso Month =< 12 -> Month;
    _ -> {error, badmonth}
  catch
    error:badarg -> {error, badmonth}
  end.

to_day(D1, D2, Year, Month) ->
  try list_to_integer([D1, D2]) of
    Day when Day >= 0 andalso Day =< 28 -> Day;
    Day when Day >= 29 andalso Day =< 31 ->
      case day_in_month(Year, Month, Day) of
        true  -> Day;
        false -> {error, badday}
      end;
    _ -> {error, badday}
  catch
    error:badarg -> {error, badday}
  end.

day_in_month({error, badyear}, _, _) -> {error, badyear};
day_in_month(_, {error, badmonth}, _) -> {error, badmonth};
day_in_month(Year, Month, Day) ->
  case Day of
    29 when Month == 2 -> ((Year rem 4 == 0) andalso not (Year rem 100 == 0)) orelse (Year rem 400 == 0);
    30 when Month == 2 -> false;
    31 when Month == 2; Month == 4; Month == 6; Month == 9; Month == 11 -> false;
    _ -> true
  end.

to_hour(H1, H2) ->
  try list_to_integer([H1, H2]) of
    Hour when Hour >= 0 andalso Hour =< 23 -> Hour;
    _ -> {error, badhour}
  catch
    error:badarg -> {error, badhour}
  end.

to_minute(M1, M2) ->
  try list_to_integer([M1, M2]) of
    Min when Min >= 0 andalso Min =< 59 -> Min;
    _ -> {error, badminute}
  catch
    error:badarg -> {error, badminute}
  end.

to_second(S1, S2) ->
  try list_to_integer([S1, S2]) of
    Sec when Sec >= 0 andalso Sec =< 60 -> Sec;
    _ -> {error, badsecond}
  catch
    error:badarg -> {error, badsecond}
  end.

format_date(Dt) ->
  Year = g(year, Dt),
  Month = g(month, Dt),
  Day = g(day, Dt),
  format_date(Year, Month, Day).

format_date(Y, M, D) when is_integer(Y), is_integer(M), is_integer(D) ->
  io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B", [Y, M, D]);
format_date(_, _, _) -> {error, baddate}.

format_time(Dt) ->
  Hour = g(hour, Dt),
  Min = g(min, Dt),
  Sec = g(sec, Dt),
  USec = g(usec, Dt),
  Time = format_time(Hour, Min, Sec, USec),
  Offset = g(tz_offset, Dt),
  TZ = format_offset(Offset),
  format_time(Time, TZ).

format_time(H, M, S, 0) when is_integer(H), is_integer(M), is_integer(S) ->
  io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B", [H, M, S]);
format_time(H, M, S, U) when is_integer(H), is_integer(M), is_integer(S), is_integer(U) ->
  SU = (S / 1) + (U / 1000000),
  io_lib:format("~2.10.0B:~2.10.0B:~9.6.0f", [H, M, SU]);
format_time(_, _, _, _) -> {error, badtime}.

format_offset(undefined) -> "Z";
format_offset(nil) -> "Z";
format_offset(0) -> "Z";
format_offset(M) when is_integer(M) ->
  Sign = case M >= 0 of true -> "+"; false -> "-" end,
  Hour = abs(M) div 60,
  Min = abs(M) rem 60,
  Sign ++ io_lib:format("~2.10.0B:~2.10.0B", [Hour, Min]).

format_time({error, Error}, _) -> {error, Error};
format_time(Time, TZ) -> [Time, TZ].

format_({error, baddate}, _) -> {error, badarg};
format_(_, {error, badtime}) -> {error, badtime};
format_(Date, Time) -> unicode:characters_to_binary([Date, "T", Time]).

g(Key, Map) ->
  g(Key, Map, record_info(fields, rfc3339), 2).

g(Key, Map, [Key|_], I) ->
  element(I, Map);
g(Key, Map, [_|Keys], I) ->
  g(Key, Map, Keys, I + 1);
g(_, _, _, _) ->
  0.

or_zero(undefined) -> 0;
or_zero(N) when is_integer(N) -> N.

convert_time_unit(Time, FromUnit, ToUnit) ->
  FromMultiplier = integer_time_unit(FromUnit),
  ToMultiplier = integer_time_unit(ToUnit),
  case Time < 0 of
	true ->
      Time * ToMultiplier - (FromMultiplier - 1);
	false ->
      Time * ToMultiplier
  end div FromMultiplier.

integer_time_unit(second) ->
  1;
integer_time_unit(millisecond) ->
  1000;
integer_time_unit(microsecond) ->
  1000 * 1000;
integer_time_unit(nanosecond) ->
  1000 * 1000 * 1000;
integer_time_unit(native) ->
  integer_time_unit(microseconds);
integer_time_unit(Unit)
  when is_integer(Unit) andalso
       Unit > 0 ->
  Unit;
integer_time_unit(Unit) ->
  erlang:error(badarg, [Unit]).
