-module(crash_protocol).

-export([start_link/4]).

-spec start_link(_, _, _, _) -> no_return().
start_link(_, _, _, _) ->
	exit(crash).
