%% Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(rfc7540_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).

all() ->
	ct_helper:all(?MODULE).

%% Server helpers.

do_origin_start(Fun) ->
	Self = self(),
	Pid = spawn_link(fun() -> do_origin_init_tcp(Self, Fun) end),
	Port = do_receive(Pid),
	{ok, Pid, Port}.

do_origin_init_tcp(Parent, Fun) ->
	{ok, ListenSocket} = gen_tcp:listen(0, [binary, {active, false}]),
	{ok, {_, Port}} = inet:sockname(ListenSocket),
	Parent ! {self(), Port},
	{ok, ClientSocket} = gen_tcp:accept(ListenSocket, 5000),
	do_handshake(ClientSocket, gen_tcp),
	Fun(Parent, ClientSocket, gen_tcp).

do_handshake(Socket, Transport) ->
	%% Send a valid preface.
	ok = Transport:send(Socket, cow_http2:settings(#{})),
	%% Receive the fixed sequence from the preface.
	Preface = <<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n">>,
	{ok, Preface} = Transport:recv(Socket, byte_size(Preface), 5000),
	%% Receive the SETTINGS from the preface.
	{ok, <<Len:24>>} = Transport:recv(Socket, 3, 1000),
	{ok, <<4:8, 0:40, _:Len/binary>>} = Transport:recv(Socket, 6 + Len, 1000),
	%% Send the SETTINGS ack.
	ok = Transport:send(Socket, cow_http2:settings_ack()),
	%% Receive the SETTINGS ack.
	{ok, <<0:24, 4:8, 1:8, 0:32>>} = Transport:recv(Socket, 9, 1000),
	ok.

do_receive(Pid) ->
	do_receive(Pid, 1000).

do_receive(Pid, Timeout) ->
	receive
		{Pid, Msg} ->
			Msg
	after Timeout ->
		error(timeout)
	end.

%% Tests.

headers_priority_flag(_) ->
	doc("HEADERS frames may include a PRIORITY flag indicating "
		"that stream dependency information is attached. (RFC7540 6.2)"),
	{ok, _, Port} = do_origin_start(fun(_, Socket, Transport) ->
		%% Receive a HEADERS frame.
		{ok, <<_:24, 1:8, _:8, 1:32>>} = Transport:recv(Socket, 9, 1000),
		%% Send a HEADERS frame with PRIORITY back.
		{HeadersBlock, _} = cow_hpack:encode([
			{<<":status">>, <<"200">>}
		]),
		Len = iolist_size(HeadersBlock) + 5,
		ok = Transport:send(Socket, [
			<<Len:24, 1:8,
				0:2, %% Undefined.
				1:1, %% PRIORITY.
				0:1, %% Undefined.
				0:1, %% PADDED.
				1:1, %% END_HEADERS.
				0:1, %% Undefined.
				1:1, %% END_STREAM.
				0:1, 1:31,
				1:1, %% Exclusive?
				3:31, %% Stream dependency.
				42:8 >>, %% Weight.
			HeadersBlock
		]),
		timer:sleep(1000)
	end),
	{ok, ConnPid} = gun:open("localhost", Port, #{protocols => [http2]}),
	{ok, http2} = gun:await_up(ConnPid),
	timer:sleep(100), %% Give enough time for the handshake to fully complete.
	StreamRef = gun:get(ConnPid, "/"),
	{response, fin, 200, _} = gun:await(ConnPid, StreamRef),
	gun:close(ConnPid).
