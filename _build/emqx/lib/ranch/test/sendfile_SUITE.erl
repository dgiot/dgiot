%% Copyright (c) 2013, James Fish <james@fishcakez.com>
%% Copyright (c) 2015-2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(sendfile_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).

all() ->
	[{group, tcp}, {group, ssl}].

suite() ->
	[{timetrap, {seconds, 60}}].

groups() ->
	Tests = [
		filename,
		rawfile,
		rawfile_bytes_large,
		rawfile_bytes_zero,
		rawfile_chunk_size_large,
		rawfile_offset_large,
		rawfile_range_large,
		rawfile_range_medium,
		rawfile_range_small
	],
	[{tcp, [parallel], Tests}, {ssl, [parallel], Tests ++ [ssl_chunk_size]}].

init_per_suite(Config) ->
	Filename = filename:join(config(priv_dir, Config), "sendfile"),
	Binary = crypto:strong_rand_bytes(20 * 1024 * 1024),
	ok = file:write_file(Filename, Binary),
	[{filename, Filename} | Config].

end_per_suite(Config) ->
	Filename = config(filename, Config),
	ok = file:delete(Filename),
	ok.

init_per_group(ssl, Config) ->
	SslOpts = ct_helper:get_certs_from_ets(),
	[{transport, ranch_ssl}, {transport_opts, SslOpts} | Config];
init_per_group(tcp, Config) ->
	[{transport, ranch_tcp}, {transport_opts, []} | Config].

end_per_group(_, _) ->
	ok.

filename(Config) ->
	doc("Use sendfile with a filename."),
	Transport = config(transport, Config),
	Filename = config(filename, Config),
	{ok, Binary} = file:read_file(Filename),
	Size = byte_size(Binary),
	{ok, {Server, Client}} = sockets(Config),
	Ref = recv(Transport, Server, Size),
	{ok, Size} = Transport:sendfile(Client, Filename),
	{ok, Binary} = result(Ref),
	{error, timeout} = Transport:recv(Server, 1, 100),
	ok = Transport:close(Client),
	ok = Transport:close(Server).

rawfile(Config) ->
	doc("Use sendfile with a file descriptor (raw file)."),
	Transport = config(transport, Config),
	Filename = config(filename, Config),
	{ok, Binary} = file:read_file(Filename),
	Size = byte_size(Binary),
	{ok, {Server, Client}} = sockets(Config),
	{ok, RawFile} = file:open(Filename, [read, raw, binary]),
	Ref = recv(Transport, Server, Size),
	{ok, Size} = Transport:sendfile(Client, RawFile, 0, Size),
	{ok, Binary} = result(Ref),
	{error, timeout} = Transport:recv(Server, 1, 100),
	{ok, 0} = file:position(RawFile, {cur, 0}),
	ok = file:close(RawFile),
	ok = Transport:close(Client),
	ok = Transport:close(Server).

rawfile_bytes_large(Config) ->
	doc("Use sendfile with a file descriptor. Try to send a size larger than file size."),
	Transport = config(transport, Config),
	Filename = config(filename, Config),
	{ok, Binary} = file:read_file(Filename),
	Size = byte_size(Binary),
	{ok, {Server, Client}} = sockets(Config),
	{ok, RawFile} = file:open(Filename, [read, raw, binary]),
	Ref = recv(Transport, Server, Size),
	%% Only send Size not Size * 2
	{ok, Size} = Transport:sendfile(Client, RawFile, 0, Size * 2),
	{ok, Binary} = result(Ref),
	{error, timeout} = Transport:recv(Server, 1, 100),
	{ok, 0} = file:position(RawFile, {cur, 0}),
	ok = file:close(RawFile),
	ok = Transport:close(Client),
	ok = Transport:close(Server).

rawfile_bytes_zero(Config) ->
	doc("Use sendfile with a file descriptor. Ensure using a size of 0 sends the whole file."),
	Transport = config(transport, Config),
	Filename = config(filename, Config),
	{ok, Binary} = file:read_file(Filename),
	Size = byte_size(Binary),
	{ok, {Server, Client}} = sockets(Config),
	{ok, RawFile} = file:open(Filename, [read, raw, binary]),
	Ref = recv(Transport, Server, Size),
	{ok, Size} = Transport:sendfile(Client, RawFile, 0, 0),
	{ok, Binary} = result(Ref),
	{error, timeout} = Transport:recv(Server, 1, 100),
	{ok, 0} = file:position(RawFile, {cur, 0}),
	ok = file:close(RawFile),
	ok = Transport:close(Client),
	ok = Transport:close(Server).

rawfile_chunk_size_large(Config) ->
	doc("Use sendfile with a file descriptor. Try to use a chunk size larger than file size."),
	Transport = config(transport, Config),
	Filename = config(filename, Config),
	{ok, Binary} = file:read_file(Filename),
	Size = byte_size(Binary),
	{ok, {Server, Client}} = sockets(Config),
	{ok, RawFile} = file:open(Filename, [read, raw, binary]),
	Ref = recv(Transport, Server, Size),
	{ok, Size} = Transport:sendfile(Client, RawFile, 0, Size, [{chunk_size, Size * 2}]),
	{ok, Binary} = result(Ref),
	{error, timeout} = Transport:recv(Server, 1, 100),
	{ok, 0} = file:position(RawFile, {cur, 0}),
	ok = file:close(RawFile),
	ok = Transport:close(Client),
	ok = Transport:close(Server).

rawfile_offset_large(Config) ->
	doc("Use sendfile with a file descriptor. Ensure using an offset larger than file size sends nothing."),
	Transport = config(transport, Config),
	Filename = config(filename, Config),
	{ok, Binary} = file:read_file(Filename),
	Size = byte_size(Binary),
	{ok, {Server, Client}} = sockets(Config),
	{ok, RawFile} = file:open(Filename, [read, raw, binary]),
	{ok, 0} = Transport:sendfile(Client, RawFile, Size, 1),
	{error, timeout} = Transport:recv(Server, 1, 100),
	ok = file:close(RawFile),
	ok = Transport:close(Client),
	ok = Transport:close(Server).

rawfile_range_large(Config) ->
	doc("Use sendfile with a file descriptor. "
		"Set an offset and try to send a size larger than remaining file size."),
	Transport = config(transport, Config),
	Filename = config(filename, Config),
	{ok, Binary} = file:read_file(Filename),
	Size = byte_size(Binary),
	{ok, {Server, Client}} = sockets(Config),
	{ok, RawFile} = file:open(Filename, [read, raw, binary]),
	Initial = 499,
	{ok, _} = file:position(RawFile, {bof, Initial}),
	Offset = 75,
	Bytes = Size * 2,
	Sent = Size - Offset,
	Ref = recv(Transport, Server, Sent),
	{ok, Sent} = Transport:sendfile(Client, RawFile, Offset, Bytes),
	Binary2 = binary:part(Binary, Offset, Sent),
	{ok, Binary2} = result(Ref),
	{error, timeout} = Transport:recv(Server, 1, 100),
	{ok, Initial} = file:position(RawFile, {cur, 0}),
	ok = file:close(RawFile),
	ok = Transport:close(Client),
	ok = Transport:close(Server).

rawfile_range_medium(Config) ->
	doc("Use sendfile with a file descriptor. "
		"Set an offset and try to send a size lower than remaining file size."),
	Transport = config(transport, Config),
	Filename = config(filename, Config),
	{ok, Binary} = file:read_file(Filename),
	Size = byte_size(Binary),
	{ok, {Server, Client}} = sockets(Config),
	{ok, RawFile} = file:open(Filename, [read, raw, binary]),
	Initial = 50,
	{ok, _} = file:position(RawFile, {bof, Initial}),
	Offset = 50,
	Bytes = Size - Offset - 50,
	Ref = recv(Transport, Server, Bytes),
	{ok, Bytes} = Transport:sendfile(Client, RawFile, Offset, Bytes),
	Binary2 = binary:part(Binary, Offset, Bytes),
	{ok, Binary2} = result(Ref),
	{error, timeout} = Transport:recv(Server, 1, 100),
	{ok, Initial} = file:position(RawFile, {cur, 0}),
	ok = file:close(RawFile),
	ok = Transport:close(Client),
	ok = Transport:close(Server).

rawfile_range_small(Config) ->
	doc("Use sendfile with a file descriptor. "
		"Set an offset and try to send a size lower than remaining file size, "
		"which is in turn lower than the chunk size."),
	Transport = config(transport, Config),
	Filename = config(filename, Config),
	{ok, Binary} = file:read_file(Filename),
	{ok, {Server, Client}} = sockets(Config),
	{ok, RawFile} = file:open(Filename, [read, raw, binary]),
	Initial = 3,
	{ok, _} = file:position(RawFile, {bof, Initial}),
	Offset = 7,
	Bytes = 19,
	Ref = recv(Transport, Server, Bytes),
	{ok, Bytes} = Transport:sendfile(Client, RawFile, Offset, Bytes, [{chunk_size, 16#FFFF}]),
	Binary2 = binary:part(Binary, Offset, Bytes),
	{ok, Binary2} = result(Ref),
	{error, timeout} = Transport:recv(Server, 1, 100),
	{ok, Initial} = file:position(RawFile, {cur, 0}),
	ok = file:close(RawFile),
	ok = Transport:close(Client),
	ok = Transport:close(Server).

ssl_chunk_size(Config) ->
	case code:is_module_native(?MODULE) of
		true -> doc("This test uses tracing and is not compatible with native code.");
		false -> do_ssl_chunk_size(Config)
	end.

do_ssl_chunk_size(Config) ->
	doc("Use sendfile with SSL. Ensure the sendfile fallback respects the chunk size."),
	Transport = config(transport, Config),
	Filename = config(filename, Config),
	{ok, Binary} = file:read_file(Filename),
	Size = byte_size(Binary),
	Self = self(),
	ChunkSize = 8 * 1024,
	Fun = fun() ->
		receive go -> ok after 1000 -> error(timeout) end,
		{ok, {Server, Client}} = sockets(Config),
		{ok, RawFile} = file:open(Filename, [read, raw, binary]),
		Ref = recv(Transport, Server, Size),
		{ok, Size} = Transport:sendfile(Client, RawFile, 0, Size, [{chunk_size, ChunkSize}]),
		{ok, Binary} = result(Ref),
		{error, timeout} = Transport:recv(Server, 1, 100),
		Self ! done,
		ok = file:close(RawFile),
		ok = Transport:close(Client),
		ok = Transport:close(Server)
	end,
	Pid = spawn_link(Fun),
	1 = erlang:trace(Pid, true, [call]),
	_ = erlang:trace_pattern({Transport, send, 2}, true, [global]),
	Pid ! go,
	receive done -> ok after 30000 -> error(timeout) end,
	Sizes = lists:duplicate(Size div ChunkSize, ChunkSize) ++
		[Size rem ChunkSize || (Size rem ChunkSize) =/= 0],
	ok = recv_send_trace(Sizes, Pid),
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces().

%% Internal.

sockets(Config) ->
	Transport = config(transport, Config),
	TransportOpts = config(transport_opts, Config),
	{ok, LSocket} = Transport:listen(TransportOpts),
	{ok, {_, Port}} = Transport:sockname(LSocket),
	Self = self(),
	Fun = fun() ->
		{ok, Client} = Transport:connect("localhost", Port, TransportOpts),
		ok = Transport:controlling_process(Client, Self),
		Self ! {ok, Client}
	end,
	_ = spawn_link(Fun),
	{ok, Server} = Transport:accept(LSocket, 500),
	{ok, _} = Transport:handshake(Server, [], 500),
	receive
		{ok, Client} ->
			ok = Transport:close(LSocket),
			{ok, {Server, Client}}
	after 1000 ->
		{error, timeout}
	end.

recv(Transport, Server, Size) ->
	Self = self(),
	Ref = make_ref(),
	spawn_link(fun() -> Self ! {Ref, Transport:recv(Server, Size, 20000)} end),
	Ref.

result(Ref) ->
	receive
		{Ref, Result} ->
			Result
	after
		30000 ->
			{error, result_timedout}
	end.

recv_send_trace([], _Pid) ->
	ok;
recv_send_trace([Size | Rest], Pid) ->
	receive
		{trace, Pid, call, {_, _, [_, Chunk]}} when byte_size(Chunk) == Size ->
			recv_send_trace(Rest, Pid);
		{trace, Pid, call, {_, _, [_, Chunk]}} ->
			{error, {invalid_chunk, Size, byte_size(Chunk)}}
	after 1000 ->
		{error, timeout}
	end.

clean_traces() ->
	receive
		{trace, _, _, _} ->
			clean_traces();
		{trace, _, _, _, _} ->
			clean_traces()
	after 0 ->
		ok
	end.
