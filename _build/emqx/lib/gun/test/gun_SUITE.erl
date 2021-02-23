%% Copyright (c) 2017-2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(gun_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).

all() ->
	ct_helper:all(?MODULE).

connect_timeout(_) ->
	doc("Ensure an integer value for connect_timeout is accepted."),
	{ok, Pid} = gun:open("localhost", 12345, #{connect_timeout => 1000, retry => 0}),
	Ref = monitor(process, Pid),
	receive
		{'DOWN', Ref, process, Pid, {shutdown, _}} ->
			ok
	after 5000 ->
		error(timeout)
	end.

connect_timeout_infinity(_) ->
	doc("Ensure infinity for connect_timeout is accepted."),
	{ok, Pid} = gun:open("localhost", 12345, #{connect_timeout => infinity, retry => 0}),
	Ref = monitor(process, Pid),
	receive
		{'DOWN', Ref, process, Pid, {shutdown, _}} ->
			ok
	after 5000 ->
		error(timeout)
	end.

detect_owner_gone(_) ->
	Self = self(),
	spawn(fun() ->
		{ok, ConnPid} = gun:open("google.com", 80),
		Self ! {conn, ConnPid},
		gun:await_up(ConnPid)
	end),
	Pid = receive
		{conn, C} ->
			C
	after 1000 ->
		error(timeout)
	end,
	Ref = monitor(process, Pid),
	receive
		{'DOWN', Ref, process, Pid, normal} ->
			ok
	after 1000 ->
		true = erlang:is_process_alive(Pid),
		error(timeout)
	end.

detect_owner_gone_ws(_) ->
	Self = self(),
	spawn(fun() ->
		{ok, ConnPid} = gun:open("echo.websocket.org", 80),
		Self ! {conn, ConnPid},
		gun:await_up(ConnPid),
		gun:ws_upgrade(ConnPid, "/", []),
		receive
			{gun_upgrade, ConnPid, _, [<<"websocket">>], _} ->
				ok
		after 1000 ->
			error(timeout)
		end
	end),
	Pid = receive
		{conn, C} ->
			C
	after 1000 ->
		error(timeout)
	end,
	Ref = monitor(process, Pid),
	receive
		{'DOWN', Ref, process, Pid, normal} ->
			ok
	after 1000 ->
		true = erlang:is_process_alive(Pid),
		error(timeout)
	end.

shutdown_reason(_) ->
	doc("The last connection failure must be propagated."),
	{ok, Pid} = gun:open("localhost", 12345, #{retry => 0}),
	Ref = monitor(process, Pid),
	receive
		{'DOWN', Ref, process, Pid, {shutdown, econnrefused}} ->
			ok
	after 200 ->
		error(timeout)
	end.

info(_) ->
	doc("Get info from the Gun connection."),
	{ok, Pid} = gun:open("google.com", 443),
	#{sock_ip := _, sock_port := _} = gun:info(Pid),
	ok.

keepalive_infinity(_) ->
	doc("Ensure infinity for keepalive is accepted by all protocols."),
	{ok, Pid} = gun:open("localhost", 12345, #{
		http_opts => #{keepalive => infinity},
		http2_opts => #{keepalive => infinity},
		retry => 0}),
	Ref = monitor(process, Pid),
	receive
		{'DOWN', Ref, process, Pid, {shutdown, _}} ->
			ok
	after 5000 ->
		error(timeout)
	end.

reply_to(_) ->
	doc("The reply_to option allows using a separate process for requests."),
	do_reply_to(http),
	do_reply_to(http2).

do_reply_to(Protocol) ->
	Self = self(),
	{ok, Pid} = gun:open("google.com", 443, #{protocols => [Protocol]}),
	{ok, Protocol} = gun:await_up(Pid),
	ReplyTo = spawn(fun() ->
		receive Ref ->
			Response = gun:await(Pid, Ref),
			Self ! Response
		after 1000 ->
			error(timeout)
		end
	end),
	Ref = gun:get(Pid, "/", [{<<"host">>, <<"google.com">>}], #{reply_to => ReplyTo}),
	ReplyTo ! Ref,
	receive
		{response, _, _, _} ->
			ok
	after 1000 ->
		error(timeout)
	end.

retry_0(_) ->
	doc("Ensure Gun gives up immediately with retry=0."),
	{ok, Pid} = gun:open("localhost", 12345, #{retry => 0, retry_timeout => 500}),
	Ref = monitor(process, Pid),
	receive
		{'DOWN', Ref, process, Pid, {shutdown, _}} ->
			ok
	after 200 ->
		error(timeout)
	end.

retry_1(_) ->
	doc("Ensure Gun gives up with retry=1."),
	{ok, Pid} = gun:open("localhost", 12345, #{retry => 1, retry_timeout => 500}),
	Ref = monitor(process, Pid),
	receive
		{'DOWN', Ref, process, Pid, {shutdown, _}} ->
			ok
	after 700 ->
		error(timeout)
	end.

retry_timeout(_) ->
	doc("Ensure the retry_timeout value is enforced."),
	{ok, Pid} = gun:open("localhost", 12345, #{retry => 1, retry_timeout => 1000}),
	Ref = monitor(process, Pid),
	receive
		{'DOWN', Ref, process, Pid, {shutdown, _}} ->
			error(shutdown_too_early)
	after 800 ->
		ok
	end,
	receive
		{'DOWN', Ref, process, Pid, {shutdown, _}} ->
			ok
	after 400 ->
		error(shutdown_too_late)
	end.

transform_header_name(_) ->
	doc("The reply_to option allows using a separate process for requests."),
	{ok, Pid} = gun:open("google.com", 443, #{
		protocols => [http],
		http_opts => #{
			transform_header_name => fun(<<"host">>) -> <<"HOST">>; (N) -> N end
		}
	}),
	{ok, http} = gun:await_up(Pid),
	Ref = gun:get(Pid, "/", [{<<"host">>, <<"google.com">>}]),
	{response, _, _, _} = gun:await(Pid, Ref),
	ok.

unix_socket_connect(_) ->
	case os:type() of
		{win32, _} ->
			doc("Unix Domain Sockets are not available on Windows.");
		_ ->
			do_unix_socket_connect()
	end.

do_unix_socket_connect() ->
	doc("Ensure we can send data via a unix domain socket."),
	DataDir = "/tmp/gun",
	SocketPath = filename:join(DataDir, "gun.sock"),
	ok = filelib:ensure_dir(SocketPath),
	_ = file:delete(SocketPath),
	TCPOpts = [
		{ifaddr, {local, SocketPath}},
		binary, {nodelay, true}, {active, false},
		{packet, raw}, {reuseaddr, true}
	],
	{ok, LSock} = gen_tcp:listen(0, TCPOpts),
	Tester = self(),
	Acceptor = fun() ->
		{ok, S} = gen_tcp:accept(LSock),
		{ok, R} = gen_tcp:recv(S, 0),
		Tester ! {recv, R},
		ok = gen_tcp:close(S),
		ok = gen_tcp:close(LSock)
	end,
	spawn(Acceptor),
	{ok, Pid} = gun:open_unix(SocketPath, #{}),
	_ = gun:get(Pid, "/", [{<<"host">>, <<"localhost">>}]),
	receive
		{recv, _} ->
			ok
	after 250 ->
		error(timeout)
	end.
