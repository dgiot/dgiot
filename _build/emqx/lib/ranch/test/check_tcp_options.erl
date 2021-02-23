-module(check_tcp_options).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).

start_link(_, Socket, _, [{pid, TestPid}|TcpOptions]) ->
	{ok, RealTcpOptions} =
		inet:getopts(Socket, [Key || {Key, _} <- TcpOptions]),
	Pid = spawn_link(?MODULE, init, [TestPid, RealTcpOptions, TcpOptions]),
	{ok, Pid}.

init(Pid, TcpOptions, TcpOptions) ->
	Pid ! checked,
	receive after 2500 -> ok end.
