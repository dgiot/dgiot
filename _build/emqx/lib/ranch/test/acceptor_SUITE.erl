%% Copyright (c) 2011-2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(acceptor_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-dialyzer({nowarn_function, misc_wait_for_connections/1}).

-import(ct_helper, [doc/1]).
-import(ct_helper, [name/0]).

%% ct.

all() ->
	[{group, tcp}, {group, ssl}, {group, misc}, {group, supervisor}].

groups() ->
	[{tcp, [
		tcp_accept_socket,
		tcp_active_echo,
		tcp_echo,
		tcp_graceful,
		tcp_accept_ack,
		tcp_inherit_options,
		tcp_max_connections,
		tcp_max_connections_and_beyond,
		tcp_max_connections_infinity,
		tcp_remove_connections,
		tcp_set_max_connections,
		tcp_set_max_connections_clean,
		tcp_getopts_capability,
		tcp_getstat_capability,
		tcp_upgrade,
		tcp_error_eaddrinuse,
		tcp_error_eacces
	]}, {ssl, [
		ssl_accept_error,
		ssl_accept_socket,
		ssl_active_echo,
		ssl_echo,
		ssl_graceful,
		ssl_accept_ack,
		ssl_sni_echo,
		ssl_sni_fail,
		ssl_upgrade_from_tcp,
		ssl_getopts_capability,
		ssl_getstat_capability,
		ssl_error_eaddrinuse,
		ssl_error_no_cert,
		ssl_error_eacces
	]}, {misc, [
		misc_bad_transport,
		misc_bad_transport_options,
		misc_info,
		misc_info_embedded,
		misc_opts_logger,
		misc_wait_for_connections
	]}, {supervisor, [
		connection_type_supervisor,
		connection_type_supervisor_separate_from_connection,
		supervisor_changed_options_restart,
		supervisor_clean_child_restart,
		supervisor_clean_restart,
		supervisor_conns_alive,
		supervisor_protocol_start_link_crash,
		supervisor_server_recover_state,
		supervisor_unexpected_message
	]}].

%% misc.

misc_bad_transport(_) ->
	doc("Reject invalid transport modules."),
	{error, badarg} = ranch:start_listener(misc_bad_transport,
		bad_transport, #{},
		echo_protocol, []),
	ok.

misc_bad_transport_options(_) ->
	doc("Ignore invalid transport options."),
	{ok, _} = ranch:start_listener(misc_bad_transport_options,
		ranch_tcp, [binary, {packet, 4}, <<"garbage">>, raw, backlog],
		echo_protocol, []),
	ok.

misc_info(_) ->
	doc("Information about listeners."),
	%% Open a listener with a few connections.
	{ok, Pid1} = ranch:start_listener({misc_info, tcp},
		ranch_tcp, #{num_acceptors => 1},
		remove_conn_and_wait_protocol, [{remove, true, 2500}]),
	Port1 = ranch:get_port({misc_info, tcp}),
	%% Open a few more listeners with different arguments.
	{ok, Pid2} = ranch:start_listener({misc_info, act},
		ranch_tcp, #{num_acceptors => 2},
		active_echo_protocol, {}),
	Port2 = ranch:get_port({misc_info, act}),
	ranch:set_max_connections({misc_info, act}, infinity),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, Pid3} = ranch:start_listener({misc_info, ssl},
		ranch_ssl, #{num_acceptors => 3, socket_opts => Opts},
		echo_protocol, [{}]),
	Port3 = ranch:get_port({misc_info, ssl}),
	%% Open 5 connections, 3 removed from the count.
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	receive after 250 -> ok end,
	ranch:set_protocol_options({misc_info, tcp}, [{remove, false, 2500}]),
	receive after 250 -> ok end,
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	receive after 250 -> ok end,
	%% Confirm the info returned by Ranch is correct.
	[
		{{misc_info, act}, [
			{pid, Pid2},
			{status, _},
			{ip, _},
			{port, Port2},
			{max_connections, infinity}, %% Option was modified.
			{active_connections, 0},
			{all_connections, 0},
			{transport, ranch_tcp},
			{transport_options, #{num_acceptors := 2}},
			{protocol, active_echo_protocol},
			{protocol_options, {}}
		]},
		{{misc_info, ssl}, [
			{pid, Pid3},
			{status, _},
			{ip, _},
			{port, Port3},
			{max_connections, 1024},
			{active_connections, 0},
			{all_connections, 0},
			{transport, ranch_ssl},
			{transport_options, #{num_acceptors := 3, socket_opts := Opts}},
			{protocol, echo_protocol},
			{protocol_options, [{}]}
		]},
		{{misc_info, tcp}, [
			{pid, Pid1},
			{status, _},
			{ip, _},
			{port, Port1},
			{max_connections, 1024},
			{active_connections, 2},
			{all_connections, 5},
			{transport, ranch_tcp},
			{transport_options, #{num_acceptors := 1}},
			{protocol, remove_conn_and_wait_protocol},
			{protocol_options, [{remove, false, 2500}]} %% Option was modified.
		]}
	] = do_get_listener_info(misc_info),
	%% Get acceptors.
	[_] = ranch:procs({misc_info, tcp}, acceptors),
	[_, _] = ranch:procs({misc_info, act}, acceptors),
	[_, _, _] = ranch:procs({misc_info, ssl}, acceptors),
	%% Get connections.
	[_, _, _, _, _] = ranch:procs({misc_info, tcp}, connections),
	[] = ranch:procs({misc_info, act}, connections),
	[] = ranch:procs({misc_info, ssl}, connections),
	ok.

misc_info_embedded(_) ->
	doc("Information about listeners in embedded mode."),
	{ok, SupPid} = embedded_sup:start_link(),
	%% Open a listener with a few connections.
	{ok, Pid1} = embedded_sup:start_listener(SupPid, {misc_info_embedded, tcp},
		ranch_tcp, #{num_acceptors => 1},
		remove_conn_and_wait_protocol, [{remove, true, 2500}]),
	Port1 = ranch:get_port({misc_info_embedded, tcp}),
	%% Open a few more listeners with different arguments.
	{ok, Pid2} = embedded_sup:start_listener(SupPid, {misc_info_embedded, act},
		ranch_tcp, #{num_acceptors => 2},
		active_echo_protocol, {}),
	Port2 = ranch:get_port({misc_info_embedded, act}),
	ranch:set_max_connections({misc_info_embedded, act}, infinity),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, Pid3} = embedded_sup:start_listener(SupPid, {misc_info_embedded, ssl},
		ranch_ssl, #{num_acceptors => 3, socket_opts => Opts},
		echo_protocol, [{}]),
	Port3 = ranch:get_port({misc_info_embedded, ssl}),
	%% Open 5 connections, 3 removed from the count.
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	receive after 250 -> ok end,
	ranch:set_protocol_options({misc_info_embedded, tcp}, [{remove, false, 2500}]),
	receive after 250 -> ok end,
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	receive after 250 -> ok end,
	%% Confirm the info returned by Ranch is correct.
	[
		{{misc_info_embedded, act}, [
			{pid, Pid2},
			{status, _},
			{ip, _},
			{port, Port2},
			{max_connections, infinity}, %% Option was modified.
			{active_connections, 0},
			{all_connections, 0},
			{transport, ranch_tcp},
			{transport_options, #{num_acceptors := 2}},
			{protocol, active_echo_protocol},
			{protocol_options, {}}
		]},
		{{misc_info_embedded, ssl}, [
			{pid, Pid3},
			{status, _},
			{ip, _},
			{port, Port3},
			{max_connections, 1024},
			{active_connections, 0},
			{all_connections, 0},
			{transport, ranch_ssl},
			{transport_options, #{num_acceptors := 3, socket_opts := Opts}},
			{protocol, echo_protocol},
			{protocol_options, [{}]}
		]},
		{{misc_info_embedded, tcp}, [
			{pid, Pid1},
			{status, _},
			{ip, _},
			{port, Port1},
			{max_connections, 1024},
			{active_connections, 2},
			{all_connections, 5},
			{transport, ranch_tcp},
			{transport_options, #{num_acceptors := 1}},
			{protocol, remove_conn_and_wait_protocol},
			{protocol_options, [{remove, false, 2500}]} %% Option was modified.
		]}
	] = do_get_listener_info(misc_info_embedded),
	%% Get acceptors.
	[_] = ranch:procs({misc_info_embedded, tcp}, acceptors),
	[_, _] = ranch:procs({misc_info_embedded, act}, acceptors),
	[_, _, _] = ranch:procs({misc_info_embedded, ssl}, acceptors),
	%% Get connections.
	[_, _, _, _, _] = ranch:procs({misc_info_embedded, tcp}, connections),
	[] = ranch:procs({misc_info_embedded, act}, connections),
	[] = ranch:procs({misc_info_embedded, ssl}, connections),
	%% Stop embedded tcp listener and ensure it is gone.
	ok = embedded_sup:stop_listener(SupPid, {misc_info_embedded, tcp}),
	timer:sleep(500),
	[{{misc_info_embedded, act}, _}, {{misc_info_embedded, ssl}, _}] =
		do_get_listener_info(misc_info_embedded),
	%% Stop embedded act listener and ensure it is gone.
	ok = embedded_sup:stop_listener(SupPid, {misc_info_embedded, act}),
	timer:sleep(500),
	[{{misc_info_embedded, ssl}, _}] =
		do_get_listener_info(misc_info_embedded),
	%% Stop embedded ssl listener and ensure it is gone.
	ok = embedded_sup:stop_listener(SupPid, {misc_info_embedded, ssl}),
	timer:sleep(500),
	[] = do_get_listener_info(misc_info_embedded),
	%% Stop embedded supervisor.
	embedded_sup:stop(SupPid),
	ok.

do_get_listener_info(ListenerGroup) ->
	lists:sort([L || L={{G, _}, _} <- ranch:info(), G=:=ListenerGroup]).

misc_opts_logger(_) ->
	doc("Confirm that messages are sent via the configured logger module."),
	register(misc_opts_logger, self()),
	{ok, _} = ranch:start_listener(name(),
		ranch_tcp, #{logger => ?MODULE, socket_opts => [<<"garbage">>]},
		echo_protocol, []),
	receive
		{warning, "Transport option " ++ _, [<<"garbage">>]} ->
			ok
	after 1000 ->
		error(timeout)
	end.

warning(Format, Args) ->
	misc_opts_logger ! {warning, Format, Args}.

misc_wait_for_connections(_) ->
	doc("Ensure wait for connections works."),
	Name = name(),
	Self = self(),
	%% Ensure invalid arguments are rejected.
	{'EXIT', {badarg, _}} = begin catch ranch:wait_for_connections(Name, 'foo', 0) end,
	{'EXIT', {badarg, _}} = begin catch ranch:wait_for_connections(Name, '==', -1) end,
	{'EXIT', {badarg, _}} = begin catch ranch:wait_for_connections(Name, '==', 0, -1) end,
	{'EXIT', {badarg, _}} = begin catch ranch:wait_for_connections(Name, '<', 0) end,
	%% Create waiters for increasing number of connections.
	Pid1GT = do_create_waiter(Self, Name, '>', 0),
	Pid1GE = do_create_waiter(Self, Name, '>=', 1),
	Pid1EQ = do_create_waiter(Self, Name, '==', 1),
	Pid2GT = do_create_waiter(Self, Name, '>', 1),
	Pid2GE = do_create_waiter(Self, Name, '>=', 2),
	Pid2EQ = do_create_waiter(Self, Name, '==', 2),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{num_acceptors => 1},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	%% Create some connections, ensure that waiters respond.
	{ok, Sock1} = gen_tcp:connect("localhost", Port, []),
	ok = do_expect_waiter(Pid1GT),
	ok = do_expect_waiter(Pid1GE),
	ok = do_expect_waiter(Pid1EQ),
	ok = do_expect_waiter(undefined),
	{ok, Sock2} = gen_tcp:connect("localhost", Port, []),
	ok = do_expect_waiter(Pid2GT),
	ok = do_expect_waiter(Pid2GE),
	ok = do_expect_waiter(Pid2EQ),
	ok = do_expect_waiter(undefined),
	%% Create waiters for decreasing number of connections.
	Pid3LT = do_create_waiter(Self, Name, '<', 2),
	Pid3LE = do_create_waiter(Self, Name, '=<', 1),
	Pid3EQ = do_create_waiter(Self, Name, '==', 1),
	Pid4LT = do_create_waiter(Self, Name, '<', 1),
	Pid4LE = do_create_waiter(Self, Name, '=<', 0),
	Pid4EQ = do_create_waiter(Self, Name, '==', 0),
	%% Close connections, ensure that waiters respond.
	ok = gen_tcp:close(Sock1),
	ok = do_expect_waiter(Pid3LT),
	ok = do_expect_waiter(Pid3LE),
	ok = do_expect_waiter(Pid3EQ),
	ok = do_expect_waiter(undefined),
	ok = gen_tcp:close(Sock2),
	ok = do_expect_waiter(Pid4LT),
	ok = do_expect_waiter(Pid4LE),
	ok = do_expect_waiter(Pid4EQ),
	ok = do_expect_waiter(undefined),
	ok = ranch:stop_listener(Name),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

do_create_waiter(ReplyTo, Ref, Op, NumConns) ->
	spawn(fun () -> ok = ranch:wait_for_connections(Ref, Op, NumConns, 100),
		ReplyTo ! {wait_connections, self()} end).

do_expect_waiter(WaiterPid) ->
	receive
		{wait_connections, _} when WaiterPid=:=undefined ->
			error;
		{wait_connections, Pid} when Pid=:=WaiterPid ->
			ok
	after 1000 ->
			case WaiterPid of
				undefined ->
					ok;
				_ ->
					timeout
			end
	end.

%% ssl.

ssl_accept_error(_) ->
	doc("Acceptor must not crash if client disconnects in the middle of SSL handshake."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, ListenerSup} = ranch:start_listener(Name,
		ranch_ssl, #{num_acceptors => 1, socket_opts => Opts},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	ListenerSupChildren = supervisor:which_children(ListenerSup),
	{_, AcceptorsSup, _, _} = lists:keyfind(ranch_acceptors_sup, 1, ListenerSupChildren),
	[{{acceptor, _, _}, AcceptorPid, _, _}] = supervisor:which_children(AcceptorsSup),
	true = is_process_alive(AcceptorPid),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:close(Socket),
	receive after 500 -> ok end,
	true = is_process_alive(AcceptorPid),
	ok = ranch:stop_listener(Name).

ssl_accept_socket(_) ->
	doc("Ensure that listener can use an externally opened SSL listen socket."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, S} = ssl:listen(0, [binary, {active, false}, {packet, raw}, {reuseaddr, true}|Opts]),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, #{socket => S},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = ssl:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_active_echo(_) ->
	doc("Ensure that active mode works with SSL transport."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts,
		active_echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_echo(_) ->
	doc("Ensure that passive mode works with SSL transport."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts,
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_sni_echo(_) ->
	case application:get_key(ssl, vsn) of
		{ok, Vsn} when Vsn >= "7.0" ->
			do_ssl_sni_echo();
		_ ->
			{skip, "No SNI support."}
	end.

do_ssl_sni_echo() ->
	doc("Ensure that SNI works with SSL transport."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, [{sni_hosts, [{"localhost", Opts}]}],
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_sni_fail(_) ->
	case application:get_key(ssl, vsn) of
		{ok, Vsn} when Vsn >= "7.0" ->
			do_ssl_sni_fail();
		_ ->
			{skip, "No SNI support."}
	end.

do_ssl_sni_fail() ->
	doc("Ensure that connection fails when host is not in SNI list."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, [{sni_hosts, [{"pouet", Opts}]}],
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{error, _} = ssl:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = ranch:stop_listener(Name),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_upgrade_from_tcp(_) ->
	doc("Ensure a TCP socket can be upgraded to SSL"),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		ssl_upgrade_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"ECHO Before upgrading to SSL">>),
	{ok, <<"Before upgrading to SSL">>} = gen_tcp:recv(Socket, 23, 1000),
	ok = gen_tcp:send(Socket, <<"UPGRADE">>),
	{ok, <<"READY">>} = gen_tcp:recv(Socket, 5, 1000),
	{ok, SslSocket} = ssl:connect(Socket, [{verify, verify_none}], 5000),
	ok = ssl:send(SslSocket, <<"ECHO After upgrading to SSL">>),
	{ok, <<"After upgrading to SSL">>} = ssl:recv(SslSocket, 22, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(SslSocket, 0, 1000),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_graceful(_) ->
	doc("Ensure suspending and resuming of listeners does not kill active connections."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts,
		echo_protocol, []),
	Port = ranch:get_port(Name),
	%% Make sure connections with a fresh listener work.
	running = ranch:get_status(Name),
	{ok, Socket1} = ssl:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = ssl:send(Socket1, <<"SSL with fresh listener">>),
	{ok, <<"SSL with fresh listener">>} = ssl:recv(Socket1, 23, 1000),
	%% Make sure transport options cannot be changed on a running listener.
	{error, running} = ranch:set_transport_options(Name, #{socket_opts => [{port, Port}|Opts]}),
	%% Suspend listener, make sure established connections keep running.
	ok = ranch:suspend_listener(Name),
	suspended = ranch:get_status(Name),
	ok = ssl:send(Socket1, <<"SSL with suspended listener">>),
	{ok, <<"SSL with suspended listener">>} = ssl:recv(Socket1, 27, 1000),
	%% Make sure new connections are refused on the suspended listener.
	{error, econnrefused} = ssl:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	%% Make sure transport options can be changed when listener is suspended.
	ok = ranch:set_transport_options(Name, #{socket_opts => [{port, Port}|Opts]}),
	%% Resume listener, make sure connections can be established again.
	ok = ranch:resume_listener(Name),
	running = ranch:get_status(Name),
	{ok, Socket2} = ssl:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = ssl:send(Socket2, <<"SSL with resumed listener">>),
	{ok, <<"SSL with resumed listener">>} = ssl:recv(Socket2, 25, 1000),
	%% Make sure transport options cannot be changed on resumed listener.
	{error, running} = ranch:set_transport_options(Name, #{socket_opts => [{port, Port}|Opts]}),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket1, 0, 1000),
	{error, closed} = ssl:recv(Socket2, 0, 1000),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_accept_ack(_) ->
	doc("Ensure accept_ack works with SSL transport."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts,
		accept_ack_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = ssl:send(Socket, <<"SSL transport accept_ack is working!">>),
	{ok, <<"SSL transport accept_ack is working!">>} = ssl:recv(Socket, 36, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_getopts_capability(_) ->
	doc("Ensure getopts/2 capability."),
	Name=name(),
	Opts=ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts,
		transport_capabilities_protocol, []),
	Port=ranch:get_port(Name),
	{ok, Socket}=ssl:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok=ssl:send(Socket, <<"getopts/2">>),
	{ok, <<"OK">>}=ssl:recv(Socket, 0, 1000),
	ok=ranch:stop_listener(Name),
	{error, closed}=ssl:recv(Socket, 0, 1000),
	{'EXIT', _}=begin catch ranch:get_port(Name) end,
	ok.

ssl_getstat_capability(_) ->
	case application:get_key(ssl, vsn) of
		{ok, Vsn} when Vsn>="8.0" ->
			do_ssl_getstat_capability();
		_ ->
			{skip, "No getstat/1,2 support."}
	end.

do_ssl_getstat_capability() ->
	doc("Ensure getstat/1,2 capability."),
	Name=name(),
	Opts=ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts,
		transport_capabilities_protocol, []),
	Port=ranch:get_port(Name),
	{ok, Socket}=ssl:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok=ssl:send(Socket, <<"getstat/1">>),
	{ok, <<"OK">>}=ssl:recv(Socket, 0, 1000),
	ok=ssl:send(Socket, <<"getstat/2">>),
	{ok, <<"OK">>}=ssl:recv(Socket, 0, 1000),
	ok=ranch:stop_listener(Name),
	{error, closed}=ssl:recv(Socket, 0, 1000),
	{'EXIT', _}=begin catch ranch:get_port(Name) end,
	ok.

ssl_error_eaddrinuse(_) ->
	doc("Ensure that failure due to an eaddrinuse returns a compact readable error."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts,
		active_echo_protocol, []),
	Port = ranch:get_port(Name),
	{error, eaddrinuse} = ranch:start_listener({Name, fails},
		ranch_ssl, [{port, Port}|Opts],
		active_echo_protocol, []),
	ok = ranch:stop_listener(Name),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_error_no_cert(_) ->
	doc("Ensure that failure due to missing certificate returns a compact readable error."),
	{error, no_cert} = ranch:start_listener(name(),
		ranch_ssl, #{},
		active_echo_protocol, []),
	ok.

ssl_error_eacces(_) ->
	case os:type() of
		{win32, nt} ->
			doc("There are no privileged ports on Windows.");
		_ ->
			doc("Ensure that failure due to an eacces returns a compact readable error."),
			Name = name(),
			Opts = ct_helper:get_certs_from_ets(),
			{error, eacces} = ranch:start_listener(Name,
				ranch_ssl, [{port, 283}|Opts],
				active_echo_protocol, []),
			ok
	end.

%% tcp.

tcp_accept_socket(_) ->
	doc("Ensure that listener can use an externally opened TCP listen socket."),
	Name = name(),
	{ok, S} = gen_tcp:listen(0, [binary, {active, false}, {packet, raw}, {reuseaddr, true}]),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{socket => S},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_active_echo(_) ->
	doc("Ensure that active mode works with TCP transport."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		active_echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_echo(_) ->
	doc("Ensure that passive mode works with TCP transport."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_graceful(_) ->
	doc("Ensure suspending and resuming of listeners does not kill active connections."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	%% Make sure connections with a fresh listener work.
	running = ranch:get_status(Name),
	{ok, Socket1} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket1, <<"TCP with fresh listener">>),
	{ok, <<"TCP with fresh listener">>} = gen_tcp:recv(Socket1, 23, 1000),
	%% Make sure transport options cannot be changed on a running listener.
	{error, running} = ranch:set_transport_options(Name, [{port, Port}]),
	%% Suspend listener, make sure established connections keep running.
	ok = ranch:suspend_listener(Name),
	suspended = ranch:get_status(Name),
	ok = gen_tcp:send(Socket1, <<"TCP with suspended listener">>),
	{ok, <<"TCP with suspended listener">>} = gen_tcp:recv(Socket1, 27, 1000),
	%% Make sure new connections are refused on the suspended listener.
	{error, econnrefused} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	%% Make sure transport options can be changed when listener is suspended.
	ok = ranch:set_transport_options(Name, [{port, Port}]),
	%% Resume listener, make sure connections can be established again.
	ok = ranch:resume_listener(Name),
	running = ranch:get_status(Name),
	{ok, Socket2} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket2, <<"TCP with resumed listener">>),
	{ok, <<"TCP with resumed listener">>} = gen_tcp:recv(Socket2, 25, 1000),
	%% Make sure transport options cannot be changed on resumed listener.
	{error, running} = ranch:set_transport_options(Name, [{port, Port}]),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket1, 0, 1000),
	{error, closed} = gen_tcp:recv(Socket2, 0, 1000),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_accept_ack(_) ->
	doc("Ensure accept_ack works with TCP transport."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		accept_ack_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP transport accept_ack is working!">>),
	{ok, <<"TCP transport accept_ack is working!">>} = gen_tcp:recv(Socket, 36, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_inherit_options(_) ->
	doc("Ensure TCP options are inherited in the protocol."),
	Name = name(),
	Opts = [{nodelay, false}, {send_timeout_close, false}],
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, Opts,
		check_tcp_options, [{pid, self()} | Opts]),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, true}, {packet, raw}]),
	receive checked -> ok after 1000 -> error(timeout) end,
	ok = gen_tcp:close(Socket),
	ok = ranch:stop_listener(Name).

tcp_max_connections(_) ->
	doc("Ensure the max_connections option actually limits connections."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{max_connections => 10, num_acceptors => 1},
		notify_and_wait_protocol, [{msg, connected}, {pid, self()}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 11, 150),
	10 = ranch_server:count_connections(Name),
	10 = receive_loop(connected, 400),
	1 = receive_loop(connected, 1000),
	ok = ranch:stop_listener(Name).

tcp_max_connections_and_beyond(_) ->
	doc("Ensure the max_connections option works when connections are removed from the count."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{max_connections => 10, num_acceptors => 1},
		remove_conn_and_wait_protocol, [{remove, true, 2500}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 10, 0),
	receive after 250 -> ok end,
	0 = ranch_server:count_connections(Name),
	10 = length(supervisor:which_children(ranch_server:get_connections_sup(Name))),
	Counts = supervisor:count_children(ranch_server:get_connections_sup(Name)),
	{_, 1} = lists:keyfind(specs, 1, Counts),
	{_, 0} = lists:keyfind(supervisors, 1, Counts),
	{_, 10} = lists:keyfind(active, 1, Counts),
	{_, 10} = lists:keyfind(workers, 1, Counts),
	ranch:set_protocol_options(Name, [{remove, false, 2500}]),
	receive after 250 -> ok end,
	ok = connect_loop(Port, 10, 0),
	receive after 250 -> ok end,
	10 = ranch_server:count_connections(Name),
	20 = length(supervisor:which_children(ranch_server:get_connections_sup(Name))),
	Counts2 = supervisor:count_children(ranch_server:get_connections_sup(Name)),
	{_, 20} = lists:keyfind(active, 1, Counts2),
	{_, 20} = lists:keyfind(workers, 1, Counts2),
	ok = ranch:stop_listener(Name).

tcp_max_connections_infinity(_) ->
	doc("Set the max_connections option from 10 to infinity and back to 10."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{max_connections => 10, num_acceptors => 1},
		notify_and_wait_protocol, [{msg, connected}, {pid, self()}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 20, 0),
	10 = ranch_server:count_connections(Name),
	10 = receive_loop(connected, 1000),
	10 = ranch_server:count_connections(Name),
	10 = ranch:get_max_connections(Name),
	ranch:set_max_connections(Name, infinity),
	receive after 250 -> ok end,
	20 = ranch_server:count_connections(Name),
	infinity = ranch:get_max_connections(Name),
	ranch:set_max_connections(Name, 10),
	20 = ranch_server:count_connections(Name),
	10 = receive_loop(connected, 1000),
	ok = ranch:stop_listener(Name).

tcp_remove_connections(_) ->
	doc("Ensure that removed connections are only removed once."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		remove_conn_and_wait_protocol, [{remove, true, 0}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 10, 0),
	receive after 250 -> ok end,
	0 = ranch_server:count_connections(Name),
	ok = ranch:stop_listener(Name).

tcp_set_max_connections(_) ->
	doc("Ensure that changing the max_connections option to a larger value allows for more connections."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{max_connections => 10, num_acceptors => 1},
		notify_and_wait_protocol, [{msg, connected}, {pid, self()}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 20, 0),
	10 = ranch_server:count_connections(Name),
	10 = receive_loop(connected, 1000),
	10 = ranch:get_max_connections(Name),
	ranch:set_max_connections(Name, 20),
	10 = receive_loop(connected, 1000),
	20 = ranch:get_max_connections(Name),
	ok = ranch:stop_listener(Name).

tcp_set_max_connections_clean(Config) ->
	case code:is_module_native(?MODULE) of
		true -> doc("This test uses tracing and is not compatible with native code.");
		false -> do_tcp_set_max_connections_clean(Config)
	end.

do_tcp_set_max_connections_clean(_) ->
	doc("Ensure that setting max_connections does not crash any process."),
	Name = name(),
	{ok, ListSupPid} = ranch:start_listener(Name,
		ranch_tcp, #{max_connections => 4},
		notify_and_wait_protocol, [{msg, connected}, {pid, self()}]),
	Children = supervisor:which_children(ListSupPid),
	{_, AccSupPid, _, _} = lists:keyfind(ranch_acceptors_sup, 1, Children),
	1 = erlang:trace(ListSupPid, true, [procs]),
	1 = erlang:trace(AccSupPid, true, [procs]),
	Port = ranch:get_port(Name),
	N = 20,
	ok = connect_loop(Port, N*5, 0),
	%% Randomly set max_connections.
	[spawn(ranch, set_max_connections, [Name, Max]) ||
		Max <- lists:flatten(lists:duplicate(N, [6, 4, 8, infinity]))],
	receive
		{trace, _, spawn, _, _} ->
			error(dirty_set_max_connections)
	after
		2000 -> ok
	end,
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces(),
	ok = ranch:stop_listener(Name).

tcp_getopts_capability(_) ->
	doc("Ensure getopts/2 capability."),
	Name=name(),
	{ok, _}=ranch:start_listener(Name,
		ranch_tcp, #{},
		transport_capabilities_protocol, []),
	Port=ranch:get_port(Name),
	{ok, Socket}=gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok=gen_tcp:send(Socket, <<"getopts/2">>),
	{ok, <<"OK">>}=gen_tcp:recv(Socket, 0, 1000),
	ok=ranch:stop_listener(Name),
	{error, closed}=gen_tcp:recv(Socket, 0, 1000),
	{'EXIT', _}=begin catch ranch:get_port(Name) end,
	ok.

tcp_getstat_capability(_) ->
	doc("Ensure getstat/1,2 capability."),
	Name=name(),
	{ok, _}=ranch:start_listener(Name,
		ranch_tcp, #{},
		transport_capabilities_protocol, []),
	Port=ranch:get_port(Name),
	{ok, Socket}=gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok=gen_tcp:send(Socket, <<"getstat/1">>),
	{ok, <<"OK">>}=gen_tcp:recv(Socket, 0, 1000),
	ok=gen_tcp:send(Socket, <<"getstat/2">>),
	{ok, <<"OK">>}=gen_tcp:recv(Socket, 0, 1000),
	ok=ranch:stop_listener(Name),
	{error, closed}=gen_tcp:recv(Socket, 0, 1000),
	{'EXIT', _}=begin catch ranch:get_port(Name) end,
	ok.

tcp_upgrade(_) ->
	doc("Ensure that protocol options can be updated."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		notify_and_wait_protocol, [{msg, connected}, {pid, self()}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 1, 0),
	receive connected -> ok after 1000 -> error(timeout) end,
	ranch:set_protocol_options(Name, [{msg, upgraded}, {pid, self()}]),
	ok = connect_loop(Port, 1, 0),
	receive upgraded -> ok after 1000 -> error(timeout) end,
	ok = ranch:stop_listener(Name).

tcp_error_eaddrinuse(_) ->
	doc("Ensure that failure due to an eaddrinuse returns a compact readable error."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		active_echo_protocol, []),
	Port = ranch:get_port(Name),
	{error, eaddrinuse} = ranch:start_listener({Name, fails},
		ranch_tcp, [{port, Port}],
		active_echo_protocol, []),
	ok = ranch:stop_listener(Name),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_error_eacces(_) ->
	case os:type() of
		{win32, nt} ->
			doc("There are no privileged ports on Windows.");
		_ ->
			doc("Ensure that failure due to an eacces returns a compact readable error."),
			Name = name(),
			{error, eacces} = ranch:start_listener(Name,
				ranch_tcp, [{port, 283}],
				active_echo_protocol, []),
			ok
	end.

%% Supervisor tests

connection_type_supervisor(_) ->
	doc("The supervisor connection type must be reflected in the specifications."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{connection_type => supervisor},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ConnsSup = ranch_server:get_connections_sup(Name),
	[{echo_protocol, _, supervisor, [echo_protocol]}] = supervisor:which_children(ConnsSup),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

connection_type_supervisor_separate_from_connection(_) ->
	doc("The supervisor connection type allows separate supervised and connection processes."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{connection_type => supervisor},
		supervisor_separate, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ConnsSup = ranch_server:get_connections_sup(Name),
	[{supervisor_separate, _, supervisor, [supervisor_separate]}] = supervisor:which_children(ConnsSup),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

supervisor_changed_options_restart(_) ->
	doc("Ensure that a listener is restarted with changed transport options."),
	Name = name(),
	%% Start a listener using send_timeout as option change marker.
	{ok, ListenerSupPid1} = ranch:start_listener(Name,
		ranch_tcp, [{send_timeout, 300000}],
		echo_protocol, []),
	%% Ensure send_timeout is really set to initial value.
	{ok, [{send_timeout, 300000}]}
		= inet:getopts(do_get_listener_socket(ListenerSupPid1), [send_timeout]),
	%% Change send_timeout option.
	ok = ranch:suspend_listener(Name),
	ok = ranch:set_transport_options(Name, [{send_timeout, 300001}]),
	ok = ranch:resume_listener(Name),
	%% Ensure send_timeout is really set to the changed value.
	{ok, [{send_timeout, 300001}]}
		= inet:getopts(do_get_listener_socket(ListenerSupPid1), [send_timeout]),
	%% Crash the listener_sup process, allow a short time for restart to succeed.
	exit(ListenerSupPid1, kill),
	timer:sleep(1000),
	%% Obtain pid of restarted listener_sup process.
	[ListenerSupPid2] = [Pid || {{ranch_listener_sup, Ref}, Pid, supervisor, _}
		<- supervisor:which_children(ranch_sup), Ref =:= Name],
	%% Ensure send_timeout is still set to the changed value.
	{ok, [{send_timeout, 300001}]}
		= inet:getopts(do_get_listener_socket(ListenerSupPid2), [send_timeout]),
	ok = ranch:stop_listener(Name),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

supervisor_clean_child_restart(Config) ->
	case code:is_module_native(?MODULE) of
		true -> doc("This test uses tracing and is not compatible with native code.");
		false -> do_supervisor_clean_child_restart(Config)
	end.

do_supervisor_clean_child_restart(_) ->
	doc("Verify that only the relevant parts of the supervision tree restarted "
		"when the listening socket is closed."),
	Name = name(),
	%% Trace socket allocations.
	_ = erlang:trace(new, true, [call]),
	1 = erlang:trace_pattern({ranch_tcp, listen, 1},
		[{'_', [], [{return_trace}]}], [global]),
	{ok, Pid} = ranch:start_listener(Name,
		ranch_tcp, #{num_acceptors => 1},
		echo_protocol, []),
	%% Trace supervisor spawns.
	1 = erlang:trace(Pid, true, [procs, set_on_spawn]),
	ConnsSup = ranch_server:get_connections_sup(Name),
	%% Manually shut the listening socket down.
	LSocket = receive
		{trace, _, return_from, {ranch_tcp, listen, 1}, {ok, Socket}} ->
			Socket
	after 0 ->
		error(lsocket_unknown)
	end,
	ok = gen_tcp:close(LSocket),
	receive after 1000 -> ok end,
	%% Verify that supervisor and its first two children are alive.
	true = is_process_alive(Pid),
	true = is_process_alive(ConnsSup),
	%% Check that acceptors_sup is restarted properly.
	AccSupPid = receive {trace, Pid, spawn, Pid1, _} -> Pid1 end,
	receive {trace, AccSupPid, spawn, _, _} -> ok end,
	%% No more traces then.
	receive
		{trace, _, spawn, _, _} -> error(invalid_restart)
	after 1000 -> ok end,
	%% Verify that children still registered right.
	ConnsSup = ranch_server:get_connections_sup(Name),
	_ = erlang:trace_pattern({ranch_tcp, listen, 1}, false, []),
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces(),
	ok = ranch:stop_listener(Name).

supervisor_clean_restart(Config) ->
	case code:is_module_native(?MODULE) of
		true -> doc("This test uses tracing and is not compatible with native code.");
		false -> do_supervisor_clean_restart(Config)
	end.

do_supervisor_clean_restart(_) ->
	doc("Verify that killing ranch_conns_sup does not crash everything "
		"and that it restarts properly."),
	Name = name(),
	NumAcc = 4,
	{ok, Pid} = ranch:start_listener(Name,
		ranch_tcp, #{num_acceptors => NumAcc},
		echo_protocol, []),
	%% Trace supervisor spawns.
	1 = erlang:trace(Pid, true, [procs, set_on_spawn]),
	ConnsSup0 = ranch_server:get_connections_sup(Name),
	erlang:exit(ConnsSup0, kill),
	receive after 1000 -> ok end,
	%% Verify that supervisor is alive
	true = is_process_alive(Pid),
	%% ...but children are dead.
	false = is_process_alive(ConnsSup0),
	%% Receive traces from newly started children
	ConnsSup = receive {trace, Pid, spawn, Pid2, _} -> Pid2 end,
	AccSupPid = receive {trace, Pid, spawn, Pid3, _} -> Pid3 end,
	%% ...and its acceptors.
	[receive {trace, AccSupPid, spawn, _Pid, _} -> ok end ||
		_ <- lists:seq(1, NumAcc)],
	%% No more traces then.
	receive
		{trace, EPid, spawn, _, _} when EPid == Pid; EPid == AccSupPid ->
			error(invalid_restart)
	after 1000 -> ok end,
	%% Verify that new children registered themselves properly.
	ConnsSup = ranch_server:get_connections_sup(Name),
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces(),
	ok = ranch:stop_listener(Name).

supervisor_conns_alive(Config) ->
	case code:is_module_native(?MODULE) of
		true -> doc("This test uses tracing and is not compatible with native code.");
		false -> do_supervisor_conns_alive(Config)
	end.

do_supervisor_conns_alive(_) ->
	doc("Ensure that active connections stay open when the listening socket gets closed."),
	Name = name(),
	_ = erlang:trace(new, true, [call]),
	1 = erlang:trace_pattern({ranch_tcp, listen, 1},
		[{'_', [], [{return_trace}]}], [global]),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		remove_conn_and_wait_protocol, [{remove, false, 2500}]),
	%% Get the listener socket
	LSocket = receive
		{trace, _, return_from, {ranch_tcp, listen, 1}, {ok, S}} ->
			S
	after 500 ->
		error(lsocket_unknown)
	end,
	TcpPort = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", TcpPort,
		[binary, {active, true}, {packet, raw}]),
	receive after 500 -> ok end,
	%% Shut the socket down
	ok = gen_tcp:close(LSocket),
	%% Assert that client is still viable.
	receive {tcp_closed, _} -> error(closed) after 1500 -> ok end,
	ok = gen_tcp:send(Socket, <<"poke">>),
	receive {tcp_closed, _} -> ok end,
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces(),
	ok = ranch:stop_listener(Name).

supervisor_protocol_start_link_crash(_) ->
	doc("Ensure a protocol start crash does not kill all connections."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		crash_protocol, []),
	ConnsSup = ranch_server:get_connections_sup(Name),
	Port = ranch:get_port(Name),
	{ok, _} = gen_tcp:connect("localhost", Port, [binary, {active, true}, {packet, raw}]),
	receive after 500 -> ok end,
	ConnsSup = ranch_server:get_connections_sup(Name),
	ok = ranch:stop_listener(Name).

supervisor_server_recover_state(Config) ->
	case code:is_module_native(?MODULE) of
		true -> doc("This test uses tracing and is not compatible with native code.");
		false -> do_supervisor_server_recover_state(Config)
	end.

do_supervisor_server_recover_state(_) ->
	doc("Ensure that when ranch_server crashes and restarts, it recovers "
		"its state and continues monitoring the same processes."),
	Name = name(),
	_ = erlang:trace(new, true, [call]),
	1 = erlang:trace_pattern({ranch_server, init, 1},
		[{'_', [], [{return_trace}]}], [global]),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		echo_protocol, []),
	ConnsSup = ranch_server:get_connections_sup(Name),
	ServerPid = erlang:whereis(ranch_server),
	{monitors, Monitors} = erlang:process_info(ServerPid, monitors),
	erlang:exit(ServerPid, kill),
	receive
		{trace, ServerPid2, return_from, {ranch_server, init, 1}, _Result} ->
			{monitors, Monitors2} = erlang:process_info(ServerPid2, monitors),
			%% Check that ranch_server is monitoring the same processes.
			true = (lists:usort(Monitors) == lists:usort(Monitors2))
	after
		1000 ->
			error(timeout)
	end,
	ConnsSup = ranch_server:get_connections_sup(Name),
	ok = ranch:stop_listener(Name),
	%% Check ranch_server has removed the ranch_conns_sup.
	{'EXIT', {badarg, _}} = (catch ranch_server:get_connections_sup(Name)),
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces().

supervisor_unexpected_message(_) ->
	doc("Ensure the connections supervisor stays alive when it receives "
		"an unexpected message."),
	Name = name(),
	{ok, ListenerPid} = ranch:start_listener(Name,
		ranch_tcp, #{},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	%% Send the unexpected message to ranch_conns_sup.
	Procs = supervisor:which_children(ListenerPid),
	{_, ConnsSup, _, _} = lists:keyfind(ranch_conns_sup, 1, Procs),
	ConnsSup ! hello,
	%% Connection is still up.
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

%% Utility functions.

connect_loop(_, 0, _) ->
	ok;
connect_loop(Port, N, Sleep) ->
	{ok, _} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	receive after Sleep -> ok end,
	connect_loop(Port, N - 1, Sleep).

receive_loop(Message, Timeout) ->
	receive_loop(Message, Timeout, 0).
receive_loop(Message, Timeout, N) ->
	receive Message ->
		receive_loop(Message, Timeout, N + 1)
	after Timeout ->
		N
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

do_get_listener_socket(ListenerSupPid) ->
	[AcceptorsSupPid] = [Pid || {ranch_acceptors_sup, Pid, supervisor, _}
		<- supervisor:which_children(ListenerSupPid)],
	{links, Links} = erlang:process_info(AcceptorsSupPid, links),
	[LSocket] = [P || P <- Links, is_port(P)],
	LSocket.
