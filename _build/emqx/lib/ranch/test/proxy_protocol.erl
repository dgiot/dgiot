-module(proxy_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).

start_link(Ref, _Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
	{ok, Pid}.

init(Ref, Transport, _Opts = []) ->
	{ok, ProxyInfo} = ranch:recv_proxy_header(Ref, 1000),
	{ok, Socket} = ranch:handshake(Ref),
	Pid = case Transport of
		ranch_tcp -> ct_helper:get_remote_pid_tcp(Socket);
		ranch_ssl -> ct_helper:get_remote_pid_tls(Socket)
	end,
	Pid ! {?MODULE, ProxyInfo},
	loop(Socket, Transport).

loop(Socket, Transport) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} ->
			_ = Transport:send(Socket, Data),
			loop(Socket, Transport);
		_ ->
			ok = Transport:close(Socket)
	end.
