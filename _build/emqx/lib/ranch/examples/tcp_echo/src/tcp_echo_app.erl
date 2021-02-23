%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(tcp_echo_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	{ok, _} = ranch:start_listener(tcp_echo,
		ranch_tcp, [{port, 5555}], echo_protocol, []),
	tcp_echo_sup:start_link().

stop(_State) ->
	ok.
