-module(ssl_upgrade_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).

start_link(Ref, _Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
	{ok, Pid}.

init(Ref, Transport, _Opts = []) ->
	{ok, Socket} = ranch:handshake(Ref),
	loop(Socket, Transport).

loop(Socket, Transport) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, <<"UPGRADE">>} when Transport =:= ranch_tcp ->
			ok = Transport:send(Socket, <<"READY">>),
			Opts = ct_helper:get_certs_from_ets(),
			{ok, NewSocket} = ranch_ssl:handshake(Socket, [{verify, verify_none}|Opts], 1000),
			loop(NewSocket, ranch_ssl);
		{ok, <<"ECHO ", More/binary>>} ->
			ok = Transport:send(Socket, More),
			loop(Socket, Transport);
		_ ->
			ok = Transport:close(Socket)
	end.
