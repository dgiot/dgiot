-module(notify_and_wait_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/2]).

start_link(_, _, _, [{msg, Msg}, {pid, TestPid}]) ->
	Pid = spawn_link(?MODULE, init, [Msg, TestPid]),
	{ok, Pid}.

init(Msg, Pid) ->
	Pid ! Msg,
	receive after 2500 -> ok end.
