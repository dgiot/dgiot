-module(remove_conn_and_wait_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).

start_link(Ref, _, _, [{remove, MaybeRemove, Timeout}]) ->
	Pid = spawn_link(?MODULE, init, [Ref, MaybeRemove, Timeout]),
	{ok, Pid}.

init(Ref, MaybeRemove, Timeout) ->
	{ok, _} = ranch:handshake(Ref),
	case MaybeRemove of
		true ->
			ranch:remove_connection(Ref);
		false ->
			ok
	end,
	receive after Timeout -> ok end.
