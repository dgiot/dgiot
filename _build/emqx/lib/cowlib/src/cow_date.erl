%% Copyright (c) 2013-2018, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(cow_date).

-export([parse_date/1]).
-export([rfc1123/1]).
-export([rfc2109/1]).
-export([rfc7231/1]).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-endif.

%% @doc Parse the HTTP date (IMF-fixdate, rfc850, asctime).

-define(DIGITS(A, B), ((A - $0) * 10 + (B - $0))).
-define(DIGITS(A, B, C, D), ((A - $0) * 1000 + (B - $0) * 100 + (C - $0) * 10 + (D - $0))).

-spec parse_date(binary()) -> calendar:datetime().
parse_date(DateBin) ->
	Date = {{_, _, D}, {H, M, S}} = http_date(DateBin),
	true = D >= 0 andalso D =< 31,
	true = H >= 0 andalso H =< 23,
	true = M >= 0 andalso M =< 59,
	true = S >= 0 andalso S =< 60, %% Leap second.
	Date.

http_date(<<"Mon, ", D1, D2, " ", R/bits >>) -> fixdate(R, ?DIGITS(D1, D2));
http_date(<<"Tue, ", D1, D2, " ", R/bits >>) -> fixdate(R, ?DIGITS(D1, D2));
http_date(<<"Wed, ", D1, D2, " ", R/bits >>) -> fixdate(R, ?DIGITS(D1, D2));
http_date(<<"Thu, ", D1, D2, " ", R/bits >>) -> fixdate(R, ?DIGITS(D1, D2));
http_date(<<"Fri, ", D1, D2, " ", R/bits >>) -> fixdate(R, ?DIGITS(D1, D2));
http_date(<<"Sat, ", D1, D2, " ", R/bits >>) -> fixdate(R, ?DIGITS(D1, D2));
http_date(<<"Sun, ", D1, D2, " ", R/bits >>) -> fixdate(R, ?DIGITS(D1, D2));
http_date(<<"Monday, ", D1, D2, "-", R/bits >>) -> rfc850_date(R, ?DIGITS(D1, D2));
http_date(<<"Tuesday, ", D1, D2, "-", R/bits >>) -> rfc850_date(R, ?DIGITS(D1, D2));
http_date(<<"Wednesday, ", D1, D2, "-", R/bits >>) -> rfc850_date(R, ?DIGITS(D1, D2));
http_date(<<"Thursday, ", D1, D2, "-", R/bits >>) -> rfc850_date(R, ?DIGITS(D1, D2));
http_date(<<"Friday, ", D1, D2, "-", R/bits >>) -> rfc850_date(R, ?DIGITS(D1, D2));
http_date(<<"Saturday, ", D1, D2, "-", R/bits >>) -> rfc850_date(R, ?DIGITS(D1, D2));
http_date(<<"Sunday, ", D1, D2, "-", R/bits >>) -> rfc850_date(R, ?DIGITS(D1, D2));
http_date(<<"Mon ", R/bits >>) -> asctime_date(R);
http_date(<<"Tue ", R/bits >>) -> asctime_date(R);
http_date(<<"Wed ", R/bits >>) -> asctime_date(R);
http_date(<<"Thu ", R/bits >>) -> asctime_date(R);
http_date(<<"Fri ", R/bits >>) -> asctime_date(R);
http_date(<<"Sat ", R/bits >>) -> asctime_date(R);
http_date(<<"Sun ", R/bits >>) -> asctime_date(R).

fixdate(<<"Jan ", Y1, Y2, Y3, Y4, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 1, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
fixdate(<<"Feb ", Y1, Y2, Y3, Y4, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 2, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
fixdate(<<"Mar ", Y1, Y2, Y3, Y4, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 3, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
fixdate(<<"Apr ", Y1, Y2, Y3, Y4, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 4, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
fixdate(<<"May ", Y1, Y2, Y3, Y4, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 5, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
fixdate(<<"Jun ", Y1, Y2, Y3, Y4, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 6, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
fixdate(<<"Jul ", Y1, Y2, Y3, Y4, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 7, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
fixdate(<<"Aug ", Y1, Y2, Y3, Y4, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 8, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
fixdate(<<"Sep ", Y1, Y2, Y3, Y4, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 9, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
fixdate(<<"Oct ", Y1, Y2, Y3, Y4, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 10, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
fixdate(<<"Nov ", Y1, Y2, Y3, Y4, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 11, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
fixdate(<<"Dec ", Y1, Y2, Y3, Y4, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 12, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}}.

rfc850_date(<<"Jan-", Y1, Y2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{rfc850_year(?DIGITS(Y1, Y2)), 1, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
rfc850_date(<<"Feb-", Y1, Y2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{rfc850_year(?DIGITS(Y1, Y2)), 2, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
rfc850_date(<<"Mar-", Y1, Y2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{rfc850_year(?DIGITS(Y1, Y2)), 3, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
rfc850_date(<<"Apr-", Y1, Y2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{rfc850_year(?DIGITS(Y1, Y2)), 4, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
rfc850_date(<<"May-", Y1, Y2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{rfc850_year(?DIGITS(Y1, Y2)), 5, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
rfc850_date(<<"Jun-", Y1, Y2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{rfc850_year(?DIGITS(Y1, Y2)), 6, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
rfc850_date(<<"Jul-", Y1, Y2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{rfc850_year(?DIGITS(Y1, Y2)), 7, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
rfc850_date(<<"Aug-", Y1, Y2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{rfc850_year(?DIGITS(Y1, Y2)), 8, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
rfc850_date(<<"Sep-", Y1, Y2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{rfc850_year(?DIGITS(Y1, Y2)), 9, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
rfc850_date(<<"Oct-", Y1, Y2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{rfc850_year(?DIGITS(Y1, Y2)), 10, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
rfc850_date(<<"Nov-", Y1, Y2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{rfc850_year(?DIGITS(Y1, Y2)), 11, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
rfc850_date(<<"Dec-", Y1, Y2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " GMT">>, Day) ->
	{{rfc850_year(?DIGITS(Y1, Y2)), 12, Day}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}}.

rfc850_year(Y) when Y > 50 -> Y + 1900;
rfc850_year(Y) -> Y + 2000.

asctime_date(<<"Jan ", D1, D2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " ", Y1, Y2, Y3, Y4 >>) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 1, asctime_day(D1, D2)}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
asctime_date(<<"Feb ", D1, D2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " ", Y1, Y2, Y3, Y4 >>) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 2, asctime_day(D1, D2)}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
asctime_date(<<"Mar ", D1, D2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " ", Y1, Y2, Y3, Y4 >>) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 3, asctime_day(D1, D2)}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
asctime_date(<<"Apr ", D1, D2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " ", Y1, Y2, Y3, Y4 >>) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 4, asctime_day(D1, D2)}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
asctime_date(<<"May ", D1, D2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " ", Y1, Y2, Y3, Y4 >>) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 5, asctime_day(D1, D2)}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
asctime_date(<<"Jun ", D1, D2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " ", Y1, Y2, Y3, Y4 >>) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 6, asctime_day(D1, D2)}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
asctime_date(<<"Jul ", D1, D2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " ", Y1, Y2, Y3, Y4 >>) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 7, asctime_day(D1, D2)}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
asctime_date(<<"Aug ", D1, D2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " ", Y1, Y2, Y3, Y4 >>) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 8, asctime_day(D1, D2)}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
asctime_date(<<"Sep ", D1, D2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " ", Y1, Y2, Y3, Y4 >>) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 9, asctime_day(D1, D2)}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
asctime_date(<<"Oct ", D1, D2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " ", Y1, Y2, Y3, Y4 >>) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 10, asctime_day(D1, D2)}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
asctime_date(<<"Nov ", D1, D2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " ", Y1, Y2, Y3, Y4 >>) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 11, asctime_day(D1, D2)}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}};
asctime_date(<<"Dec ", D1, D2, " ", H1, H2, ":", M1, M2, ":", S1, S2, " ", Y1, Y2, Y3, Y4 >>) ->
	{{?DIGITS(Y1, Y2, Y3, Y4), 12, asctime_day(D1, D2)}, {?DIGITS(H1, H2), ?DIGITS(M1, M2), ?DIGITS(S1, S2)}}.

asctime_day($\s, D2) -> (D2 - $0);
asctime_day(D1, D2) -> (D1 - $0) * 10 + (D2 - $0).

-ifdef(TEST).
day_name() -> oneof(["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]).
day_name_l() -> oneof(["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]).
year() -> integer(1951, 2050).
month() -> integer(1, 12).
day() -> integer(1, 31).
hour() -> integer(0, 23).
minute() -> integer(0, 59).
second() -> integer(0, 60).

fixdate_gen() ->
	?LET({DayName, Y, Mo, D, H, Mi, S},
		{day_name(), year(), month(), day(), hour(), minute(), second()},
		{{{Y, Mo, D}, {H, Mi, S}},
			list_to_binary([DayName, ", ", pad_int(D), " ", month(Mo), " ", integer_to_binary(Y),
			" ", pad_int(H), ":", pad_int(Mi), ":", pad_int(S), " GMT"])}).

rfc850_gen() ->
	?LET({DayName, Y, Mo, D, H, Mi, S},
		{day_name_l(), year(), month(), day(), hour(), minute(), second()},
		{{{Y, Mo, D}, {H, Mi, S}},
			list_to_binary([DayName, ", ", pad_int(D), "-", month(Mo), "-", pad_int(Y rem 100),
			" ", pad_int(H), ":", pad_int(Mi), ":", pad_int(S), " GMT"])}).

asctime_gen() ->
	?LET({DayName, Y, Mo, D, H, Mi, S},
		{day_name(), year(), month(), day(), hour(), minute(), second()},
		{{{Y, Mo, D}, {H, Mi, S}},
			list_to_binary([DayName, " ", month(Mo), " ",
			if D < 10 -> << $\s, (D + $0) >>; true -> integer_to_binary(D) end,
			" ", pad_int(H), ":", pad_int(Mi), ":", pad_int(S), " ", integer_to_binary(Y)])}).

prop_http_date() ->
	?FORALL({Date, DateBin},
		oneof([fixdate_gen(), rfc850_gen(), asctime_gen()]),
		Date =:= parse_date(DateBin)).

http_date_test_() ->
	Tests = [
		{<<"Sun, 06 Nov 1994 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}},
		{<<"Sunday, 06-Nov-94 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}},
		{<<"Sun Nov  6 08:49:37 1994">>, {{1994, 11, 6}, {8, 49, 37}}}
	],
	[{V, fun() -> R = http_date(V) end} || {V, R} <- Tests].

horse_http_date_fixdate() ->
	horse:repeat(200000,
		http_date(<<"Sun, 06 Nov 1994 08:49:37 GMT">>)
	).

horse_http_date_rfc850() ->
	horse:repeat(200000,
		http_date(<<"Sunday, 06-Nov-94 08:49:37 GMT">>)
	).

horse_http_date_asctime() ->
	horse:repeat(200000,
		http_date(<<"Sun Nov  6 08:49:37 1994">>)
	).
-endif.

%% @doc Return the date formatted according to RFC1123.

-spec rfc1123(calendar:datetime()) -> binary().
rfc1123(DateTime) ->
	rfc7231(DateTime).

%% @doc Return the date formatted according to RFC2109.

-spec rfc2109(calendar:datetime()) -> binary().
rfc2109({Date = {Y, Mo, D}, {H, Mi, S}}) ->
	Wday = calendar:day_of_the_week(Date),
	<<	(weekday(Wday))/binary, ", ",
		(pad_int(D))/binary, "-",
		(month(Mo))/binary, "-",
		(year(Y))/binary, " ",
		(pad_int(H))/binary, ":",
		(pad_int(Mi))/binary, ":",
		(pad_int(S))/binary, " GMT" >>.

-ifdef(TEST).
rfc2109_test_() ->
	Tests = [
		{<<"Sat, 14-May-2011 14:25:33 GMT">>, {{2011, 5, 14}, {14, 25, 33}}},
		{<<"Sun, 01-Jan-2012 00:00:00 GMT">>, {{2012, 1,  1}, { 0,  0,  0}}}
	],
	[{R, fun() -> R = rfc2109(D) end} || {R, D} <- Tests].

horse_rfc2109_20130101_000000() ->
	horse:repeat(100000,
		rfc2109({{2013, 1, 1}, {0, 0, 0}})
	).

horse_rfc2109_20131231_235959() ->
	horse:repeat(100000,
		rfc2109({{2013, 12, 31}, {23, 59, 59}})
	).

horse_rfc2109_12340506_070809() ->
	horse:repeat(100000,
		rfc2109({{1234, 5, 6}, {7, 8, 9}})
	).
-endif.

%% @doc Return the date formatted according to RFC7231.

-spec rfc7231(calendar:datetime()) -> binary().
rfc7231({Date = {Y, Mo, D}, {H, Mi, S}}) ->
	Wday = calendar:day_of_the_week(Date),
	<<	(weekday(Wday))/binary, ", ",
		(pad_int(D))/binary, " ",
		(month(Mo))/binary, " ",
		(year(Y))/binary, " ",
		(pad_int(H))/binary, ":",
		(pad_int(Mi))/binary, ":",
		(pad_int(S))/binary, " GMT" >>.

-ifdef(TEST).
rfc7231_test_() ->
	Tests = [
		{<<"Sat, 14 May 2011 14:25:33 GMT">>, {{2011, 5, 14}, {14, 25, 33}}},
		{<<"Sun, 01 Jan 2012 00:00:00 GMT">>, {{2012, 1,  1}, { 0,  0,  0}}}
	],
	[{R, fun() -> R = rfc7231(D) end} || {R, D} <- Tests].

horse_rfc7231_20130101_000000() ->
	horse:repeat(100000,
		rfc7231({{2013, 1, 1}, {0, 0, 0}})
	).

horse_rfc7231_20131231_235959() ->
	horse:repeat(100000,
		rfc7231({{2013, 12, 31}, {23, 59, 59}})
	).

horse_rfc7231_12340506_070809() ->
	horse:repeat(100000,
		rfc7231({{1234, 5, 6}, {7, 8, 9}})
	).
-endif.

%% Internal.

-spec pad_int(0..59) -> <<_:16>>.
pad_int( 0) -> <<"00">>;
pad_int( 1) -> <<"01">>;
pad_int( 2) -> <<"02">>;
pad_int( 3) -> <<"03">>;
pad_int( 4) -> <<"04">>;
pad_int( 5) -> <<"05">>;
pad_int( 6) -> <<"06">>;
pad_int( 7) -> <<"07">>;
pad_int( 8) -> <<"08">>;
pad_int( 9) -> <<"09">>;
pad_int(10) -> <<"10">>;
pad_int(11) -> <<"11">>;
pad_int(12) -> <<"12">>;
pad_int(13) -> <<"13">>;
pad_int(14) -> <<"14">>;
pad_int(15) -> <<"15">>;
pad_int(16) -> <<"16">>;
pad_int(17) -> <<"17">>;
pad_int(18) -> <<"18">>;
pad_int(19) -> <<"19">>;
pad_int(20) -> <<"20">>;
pad_int(21) -> <<"21">>;
pad_int(22) -> <<"22">>;
pad_int(23) -> <<"23">>;
pad_int(24) -> <<"24">>;
pad_int(25) -> <<"25">>;
pad_int(26) -> <<"26">>;
pad_int(27) -> <<"27">>;
pad_int(28) -> <<"28">>;
pad_int(29) -> <<"29">>;
pad_int(30) -> <<"30">>;
pad_int(31) -> <<"31">>;
pad_int(32) -> <<"32">>;
pad_int(33) -> <<"33">>;
pad_int(34) -> <<"34">>;
pad_int(35) -> <<"35">>;
pad_int(36) -> <<"36">>;
pad_int(37) -> <<"37">>;
pad_int(38) -> <<"38">>;
pad_int(39) -> <<"39">>;
pad_int(40) -> <<"40">>;
pad_int(41) -> <<"41">>;
pad_int(42) -> <<"42">>;
pad_int(43) -> <<"43">>;
pad_int(44) -> <<"44">>;
pad_int(45) -> <<"45">>;
pad_int(46) -> <<"46">>;
pad_int(47) -> <<"47">>;
pad_int(48) -> <<"48">>;
pad_int(49) -> <<"49">>;
pad_int(50) -> <<"50">>;
pad_int(51) -> <<"51">>;
pad_int(52) -> <<"52">>;
pad_int(53) -> <<"53">>;
pad_int(54) -> <<"54">>;
pad_int(55) -> <<"55">>;
pad_int(56) -> <<"56">>;
pad_int(57) -> <<"57">>;
pad_int(58) -> <<"58">>;
pad_int(59) -> <<"59">>;
pad_int(60) -> <<"60">>;
pad_int(Int) -> integer_to_binary(Int).

-spec weekday(1..7) -> <<_:24>>.
weekday(1) -> <<"Mon">>;
weekday(2) -> <<"Tue">>;
weekday(3) -> <<"Wed">>;
weekday(4) -> <<"Thu">>;
weekday(5) -> <<"Fri">>;
weekday(6) -> <<"Sat">>;
weekday(7) -> <<"Sun">>.

-spec month(1..12) -> <<_:24>>.
month( 1) -> <<"Jan">>;
month( 2) -> <<"Feb">>;
month( 3) -> <<"Mar">>;
month( 4) -> <<"Apr">>;
month( 5) -> <<"May">>;
month( 6) -> <<"Jun">>;
month( 7) -> <<"Jul">>;
month( 8) -> <<"Aug">>;
month( 9) -> <<"Sep">>;
month(10) -> <<"Oct">>;
month(11) -> <<"Nov">>;
month(12) -> <<"Dec">>.

-spec year(pos_integer()) -> <<_:32>>.
year(1970) -> <<"1970">>;
year(1971) -> <<"1971">>;
year(1972) -> <<"1972">>;
year(1973) -> <<"1973">>;
year(1974) -> <<"1974">>;
year(1975) -> <<"1975">>;
year(1976) -> <<"1976">>;
year(1977) -> <<"1977">>;
year(1978) -> <<"1978">>;
year(1979) -> <<"1979">>;
year(1980) -> <<"1980">>;
year(1981) -> <<"1981">>;
year(1982) -> <<"1982">>;
year(1983) -> <<"1983">>;
year(1984) -> <<"1984">>;
year(1985) -> <<"1985">>;
year(1986) -> <<"1986">>;
year(1987) -> <<"1987">>;
year(1988) -> <<"1988">>;
year(1989) -> <<"1989">>;
year(1990) -> <<"1990">>;
year(1991) -> <<"1991">>;
year(1992) -> <<"1992">>;
year(1993) -> <<"1993">>;
year(1994) -> <<"1994">>;
year(1995) -> <<"1995">>;
year(1996) -> <<"1996">>;
year(1997) -> <<"1997">>;
year(1998) -> <<"1998">>;
year(1999) -> <<"1999">>;
year(2000) -> <<"2000">>;
year(2001) -> <<"2001">>;
year(2002) -> <<"2002">>;
year(2003) -> <<"2003">>;
year(2004) -> <<"2004">>;
year(2005) -> <<"2005">>;
year(2006) -> <<"2006">>;
year(2007) -> <<"2007">>;
year(2008) -> <<"2008">>;
year(2009) -> <<"2009">>;
year(2010) -> <<"2010">>;
year(2011) -> <<"2011">>;
year(2012) -> <<"2012">>;
year(2013) -> <<"2013">>;
year(2014) -> <<"2014">>;
year(2015) -> <<"2015">>;
year(2016) -> <<"2016">>;
year(2017) -> <<"2017">>;
year(2018) -> <<"2018">>;
year(2019) -> <<"2019">>;
year(2020) -> <<"2020">>;
year(2021) -> <<"2021">>;
year(2022) -> <<"2022">>;
year(2023) -> <<"2023">>;
year(2024) -> <<"2024">>;
year(2025) -> <<"2025">>;
year(2026) -> <<"2026">>;
year(2027) -> <<"2027">>;
year(2028) -> <<"2028">>;
year(2029) -> <<"2029">>;
year(Year) -> integer_to_binary(Year).
