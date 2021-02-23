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

-module(shutdown_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).
-import(ct_helper, [name/0]).

%% ct.

all() ->
	ct_helper:all(?MODULE).

%% Tests.

brutal_kill(_) ->
	doc("Shutdown Ranch listener with shutdown option set to brutal_kill."),
	Name = name(),
	{ok, ListenerSup} = ranch:start_listener(Name,
		ranch_tcp, #{shutdown => brutal_kill},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, _} = gen_tcp:connect("localhost", Port, []),
	receive after 100 -> ok end,
	ListenerSupChildren = supervisor:which_children(ListenerSup),
	{_, ConnsSup, _, _} = lists:keyfind(ranch_conns_sup, 1, ListenerSupChildren),
	[{_, Pid, _, _}] = supervisor:which_children(ConnsSup),
	true = is_process_alive(Pid),
	ok = ranch:stop_listener(Name),
	receive after 100 -> ok end,
	false = is_process_alive(Pid),
	false = is_process_alive(ListenerSup),
	{error, _} = gen_tcp:connect("localhost", Port, []),
	ok.

infinity(_) ->
	doc("Shutdown Ranch listener with shutdown option set to infinity."),
	Name = name(),
	{ok, ListenerSup} = ranch:start_listener(Name,
		ranch_tcp, #{shutdown => infinity},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, _} = gen_tcp:connect("localhost", Port, []),
	receive after 100 -> ok end,
	ListenerSupChildren = supervisor:which_children(ListenerSup),
	{_, ConnsSup, _, _} = lists:keyfind(ranch_conns_sup, 1, ListenerSupChildren),
	[{_, Pid, _, _}] = supervisor:which_children(ConnsSup),
	true = is_process_alive(Pid),
	ok = ranch:stop_listener(Name),
	receive after 100 -> ok end,
	false = is_process_alive(Pid),
	false = is_process_alive(ListenerSup),
	{error, _} = gen_tcp:connect("localhost", Port, []),
	ok.

infinity_trap_exit(_) ->
	doc("Shutdown Ranch listener with shutdown option set to infinity "
		"and protocol process trapping exits. The listener must not stop "
		"until the protocol process terminates."),
	Name = name(),
	{ok, ListenerSup} = ranch:start_listener(Name,
		ranch_tcp, #{shutdown => infinity},
		trap_exit_protocol, []),
	Port = ranch:get_port(Name),
	{ok, _} = gen_tcp:connect("localhost", Port, []),
	receive after 100 -> ok end,
	ListenerSupChildren = supervisor:which_children(ListenerSup),
	{_, ConnsSup, _, _} = lists:keyfind(ranch_conns_sup, 1, ListenerSupChildren),
	[{_, Pid, _, _}] = supervisor:which_children(ConnsSup),
	true = is_process_alive(Pid),
	%% This call will block infinitely.
	SpawnPid = spawn(fun() -> ok = ranch:stop_listener(Name) end),
	receive after 100 -> ok end,
	%% The protocol traps exit signals, and ignore them, so it won't die.
	true = is_process_alive(Pid),
	%% The listener will stay up forever too.
	true = is_process_alive(ListenerSup),
	%% We can't connect, though.
	{error, _} = gen_tcp:connect("localhost", Port, []),
	%% Killing the process unblocks everything.
	exit(Pid, kill),
	receive after 100 -> ok end,
	false = is_process_alive(ListenerSup),
	false = is_process_alive(SpawnPid),
	ok.

timeout(_) ->
	doc("Shutdown Ranch listener with shutdown option set to 500ms."),
	Name = name(),
	{ok, ListenerSup} = ranch:start_listener(Name,
		ranch_tcp, #{shutdown => 500},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, _} = gen_tcp:connect("localhost", Port, []),
	receive after 100 -> ok end,
	ListenerSupChildren = supervisor:which_children(ListenerSup),
	{_, ConnsSup, _, _} = lists:keyfind(ranch_conns_sup, 1, ListenerSupChildren),
	[{_, Pid, _, _}] = supervisor:which_children(ConnsSup),
	true = is_process_alive(Pid),
	ok = ranch:stop_listener(Name),
	receive after 100 -> ok end,
	false = is_process_alive(Pid),
	false = is_process_alive(ListenerSup),
	{error, _} = gen_tcp:connect("localhost", Port, []),
	ok.

timeout_trap_exit(_) ->
	doc("Shutdown Ranch listener with shutdown option set to 500ms "
		"and protocol process trapping exits. The listener will only stop "
		"after the 500ms timeout."),
	Name = name(),
	{ok, ListenerSup} = ranch:start_listener(Name,
		ranch_tcp, #{shutdown => 500},
		trap_exit_protocol, []),
	Port = ranch:get_port(Name),
	{ok, _} = gen_tcp:connect("localhost", Port, []),
	receive after 100 -> ok end,
	ListenerSupChildren = supervisor:which_children(ListenerSup),
	{_, ConnsSup, _, _} = lists:keyfind(ranch_conns_sup, 1, ListenerSupChildren),
	[{_, Pid, _, _}] = supervisor:which_children(ConnsSup),
	true = is_process_alive(Pid),
	%% This call will block for the duration of the shutdown.
	SpawnPid = spawn(fun() -> ok = ranch:stop_listener(Name) end),
	receive after 100 -> ok end,
	%% The protocol traps exit signals, and ignore them, so it won't die.
	true = is_process_alive(Pid),
	%% The listener will stay up for now too.
	true = is_process_alive(ListenerSup),
	%% We can't connect, though.
	{error, _} = gen_tcp:connect("localhost", Port, []),
	%% Wait for the timeout to finish and see that everything is killed.
	receive after 500 -> ok end,
	false = is_process_alive(Pid),
	false = is_process_alive(ListenerSup),
	false = is_process_alive(SpawnPid),
	ok.
