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

-module(twitter_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

all() ->
	[http, http2].

http(_) ->
	{ok, Pid} = gun:open("twitter.com", 443, #{protocols => [http]}),
	{ok, http} = gun:await_up(Pid),
	common(Pid).

http2(_) ->
	{ok, Pid} = gun:open("twitter.com", 443, #{protocols => [http2]}),
	{ok, http2} = gun:await_up(Pid),
	common(Pid).

common(Pid) ->
	Ref = gun:get(Pid, "/"),
	receive
		{gun_response, Pid, Ref, nofin, Status, Headers} ->
			ct:print("response ~p ~p", [Status, Headers]),
			data_loop(Pid, Ref)
	after 5000 ->
		error(timeout)
	end.

data_loop(Pid, Ref) ->
	receive
		{gun_data, Pid, Ref, nofin, Data} ->
			ct:print("data ~p", [Data]),
			data_loop(Pid, Ref);
		{gun_data, Pid, Ref, fin, Data} ->
			gun:close(Pid),
			ct:print("data ~p~nend", [Data])
	after 5000 ->
		error(timeout)
	end.
