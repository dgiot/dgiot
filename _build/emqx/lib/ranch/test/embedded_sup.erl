-module(embedded_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/0]).
-export([stop/1]).
-export([start_listener/6]).
-export([stop_listener/2]).

start_link() ->
	supervisor:start_link(?MODULE, []).

stop(SupPid) ->
	erlang:exit(SupPid, normal).

init([]) ->
	{ok, {{one_for_one, 10, 10}, []}}.

start_listener(SupPid, Ref, Transport, TransOpts, Protocol, ProtoOpts) ->
	supervisor:start_child(
		SupPid,
		ranch:child_spec(Ref, Transport, TransOpts, Protocol, ProtoOpts)
	).

stop_listener(SupPid, Ref) ->
	ok = supervisor:terminate_child(SupPid, {ranch_listener_sup, Ref}),
	ok = supervisor:delete_child(SupPid, {ranch_listener_sup, Ref}),
	ranch_server:cleanup_listener_opts(Ref).
