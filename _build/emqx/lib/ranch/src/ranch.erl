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

-module(ranch).

-export([start_listener/5]).
-export([start_listener/6]).
-export([normalize_opts/1]).
-export([stop_listener/1]).
-export([suspend_listener/1]).
-export([resume_listener/1]).
-export([child_spec/5]).
-export([child_spec/6]).
-export([accept_ack/1]).
-export([handshake/1]).
-export([handshake/2]).
-export([recv_proxy_header/2]).
-export([remove_connection/1]).
-export([get_status/1]).
-export([get_addr/1]).
-export([get_port/1]).
-export([get_max_connections/1]).
-export([set_max_connections/2]).
-export([get_transport_options/1]).
-export([set_transport_options/2]).
-export([get_protocol_options/1]).
-export([set_protocol_options/2]).
-export([info/0]).
-export([info/1]).
-export([procs/2]).
-export([wait_for_connections/3]).
-export([wait_for_connections/4]).
-export([filter_options/3]).
-export([set_option_default/3]).
-export([require/1]).
-export([log/4]).

-deprecated([start_listener/6, child_spec/6, accept_ack/1]).

-type max_conns() :: non_neg_integer() | infinity.
-export_type([max_conns/0]).

%% This type is deprecated and will be removed in Ranch 2.0.
-type opt() :: {ack_timeout, timeout()}
	| {connection_type, worker | supervisor}
	| {max_connections, max_conns()}
	| {num_acceptors, pos_integer()}
	| {shutdown, timeout() | brutal_kill}
	| {socket, any()}.
-export_type([opt/0]).

-type opts() :: any() | #{
	connection_type => worker | supervisor,
	handshake_timeout => timeout(),
	max_connections => max_conns(),
	logger => module(),
	num_acceptors => pos_integer(),
	shutdown => timeout() | brutal_kill,
	socket => any(),
	socket_opts => any()
}.
-export_type([opts/0]).

-type ref() :: any().
-export_type([ref/0]).

-spec start_listener(ref(), module(), opts(), module(), any())
	-> supervisor:startchild_ret().
start_listener(Ref, Transport, TransOpts0, Protocol, ProtoOpts)
		when is_atom(Transport), is_atom(Protocol) ->
	TransOpts = normalize_opts(TransOpts0),
	_ = code:ensure_loaded(Transport),
	case erlang:function_exported(Transport, name, 0) of
		false ->
			{error, badarg};
		true ->
			Res = supervisor:start_child(ranch_sup, child_spec(Ref,
					Transport, TransOpts, Protocol, ProtoOpts)),
			Socket = maps:get(socket, TransOpts, undefined),
			case Res of
				{ok, Pid} when Socket =/= undefined ->
					%% Give ownership of the socket to ranch_acceptors_sup
					%% to make sure the socket stays open as long as the
					%% listener is alive. If the socket closes however there
					%% will be no way to recover because we don't know how
					%% to open it again.
					Children = supervisor:which_children(Pid),
					{_, AcceptorsSup, _, _}
						= lists:keyfind(ranch_acceptors_sup, 1, Children),
					Transport:controlling_process(Socket, AcceptorsSup);
				_ ->
					ok
			end,
			maybe_started(Res)
	end.

-spec start_listener(ref(), non_neg_integer(), module(), opts(), module(), any())
	-> supervisor:startchild_ret().
start_listener(Ref, NumAcceptors, Transport, TransOpts0, Protocol, ProtoOpts)
		when is_integer(NumAcceptors), is_atom(Transport), is_atom(Protocol) ->
	TransOpts = normalize_opts(TransOpts0),
	start_listener(Ref, Transport, TransOpts#{num_acceptors => NumAcceptors},
		Protocol, ProtoOpts).

-spec normalize_opts(opts()) -> opts().
normalize_opts(Map) when is_map(Map) ->
	Map;
normalize_opts(List0) when is_list(List0) ->
	Map0 = #{},
	{Map1, List1} = case take(ack_timeout, List0) of
		{value, HandshakeTimeout, Tail0} ->
			{Map0#{handshake_timeout => HandshakeTimeout}, Tail0};
		false ->
			{Map0, List0}
	end,
	{Map, List} = lists:foldl(fun(Key, {Map2, List2}) ->
		case take(Key, List2) of
			{value, ConnectionType, Tail2} ->
				{Map2#{Key => ConnectionType}, Tail2};
			false ->
				{Map2, List2}
		end
	end, {Map1, List1}, [connection_type, max_connections, num_acceptors, shutdown, socket]),
	if
		Map =:= #{} ->
			ok;
		true ->
			log(warning,
				"Setting Ranch options together with socket options "
				"is deprecated. Please use the new map syntax that allows "
				"specifying socket options separately from other options.~n",
				[], Map)
	end,
	case List of
		[] -> Map;
		_ -> Map#{socket_opts => List}
	end;
normalize_opts(Any) ->
	#{socket_opts => Any}.

take(Key, List) ->
	take(Key, List, []).

take(_, [], _) ->
	false;
take(Key, [{Key, Value}|Tail], Acc) ->
	{value, Value, lists:reverse(Acc, Tail)};
take(Key, [Value|Tail], Acc) ->
	take(Key, Tail, [Value|Acc]).

maybe_started({error, {{shutdown,
		{failed_to_start_child, ranch_acceptors_sup,
			{listen_error, _, Reason}}}, _}} = Error) ->
	start_error(Reason, Error);
maybe_started(Res) ->
	Res.

start_error(E=eaddrinuse, _) -> {error, E};
start_error(E=eacces, _) -> {error, E};
start_error(E=no_cert, _) -> {error, E};
start_error(_, Error) -> Error.

-spec stop_listener(ref()) -> ok | {error, not_found}.
stop_listener(Ref) ->
	case supervisor:terminate_child(ranch_sup, {ranch_listener_sup, Ref}) of
		ok ->
			_ = supervisor:delete_child(ranch_sup, {ranch_listener_sup, Ref}),
			ranch_server:cleanup_listener_opts(Ref);
		{error, Reason} ->
			{error, Reason}
	end.

-spec suspend_listener(ref()) -> ok | {error, any()}.
suspend_listener(Ref) ->
	case get_status(Ref) of
		running ->
			ListenerSup = ranch_server:get_listener_sup(Ref),
			ok = ranch_server:set_addr(Ref, {undefined, undefined}),
			supervisor:terminate_child(ListenerSup, ranch_acceptors_sup);
		suspended ->
			ok
	end.

-spec resume_listener(ref()) -> ok | {error, any()}.
resume_listener(Ref) ->
	case get_status(Ref) of
		running ->
			ok;
		suspended ->
			ListenerSup = ranch_server:get_listener_sup(Ref),
			Res = supervisor:restart_child(ListenerSup, ranch_acceptors_sup),
			maybe_resumed(Res)
	end.

maybe_resumed(Error={error, {listen_error, _, Reason}}) ->
	start_error(Reason, Error);
maybe_resumed({ok, _}) ->
	ok;
maybe_resumed({ok, _, _}) ->
	ok;
maybe_resumed(Res) ->
	Res.

-spec child_spec(ref(), module(), opts(), module(), any())
	-> supervisor:child_spec().
child_spec(Ref, Transport, TransOpts0, Protocol, ProtoOpts) ->
	TransOpts = normalize_opts(TransOpts0),
	{{ranch_listener_sup, Ref}, {ranch_listener_sup, start_link, [
		Ref, Transport, TransOpts, Protocol, ProtoOpts
	]}, permanent, infinity, supervisor, [ranch_listener_sup]}.

-spec child_spec(ref(), non_neg_integer(), module(), opts(), module(), any())
	-> supervisor:child_spec().
child_spec(Ref, NumAcceptors, Transport, TransOpts0, Protocol, ProtoOpts)
		when is_integer(NumAcceptors), is_atom(Transport), is_atom(Protocol) ->
	TransOpts = normalize_opts(TransOpts0),
	child_spec(Ref, Transport, TransOpts#{num_acceptors => NumAcceptors},
		Protocol, ProtoOpts).

-spec accept_ack(ref()) -> ok.
accept_ack(Ref) ->
	{ok, _} = handshake(Ref),
	ok.

-spec handshake(ref()) -> {ok, ranch_transport:socket()}.
handshake(Ref) ->
	handshake(Ref, []).

-spec handshake(ref(), any()) -> {ok, ranch_transport:socket()}.
handshake(Ref, Opts) ->
	receive {handshake, Ref, Transport, CSocket, HandshakeTimeout} ->
		case Transport:handshake(CSocket, Opts, HandshakeTimeout) of
			OK = {ok, _} ->
				OK;
			%% Garbage was most likely sent to the socket, don't error out.
			{error, {tls_alert, _}} ->
				ok = Transport:close(CSocket),
				exit(normal);
			%% Socket most likely stopped responding, don't error out.
			{error, Reason} when Reason =:= timeout; Reason =:= closed ->
				ok = Transport:close(CSocket),
				exit(normal);
			{error, Reason} ->
				ok = Transport:close(CSocket),
				error(Reason)
		end
	end.

%% Unlike handshake/2 this function always return errors because
%% the communication between the proxy and the server are expected
%% to be reliable. If there is a problem while receiving the proxy
%% header, we probably want to know about it.
-spec recv_proxy_header(ref(), timeout())
	-> {ok, ranch_proxy_header:proxy_info()}
	| {error, closed | atom()}
	| {error, protocol_error, atom()}.
recv_proxy_header(Ref, Timeout) ->
	receive HandshakeState={handshake, Ref, Transport, CSocket, _} ->
		self() ! HandshakeState,
		Transport:recv_proxy_header(CSocket, Timeout)
	end.

-spec remove_connection(ref()) -> ok.
remove_connection(Ref) ->
	ConnsSup = ranch_server:get_connections_sup(Ref),
	ConnsSup ! {remove_connection, Ref, self()},
	ok.

-spec get_status(ref()) -> running | suspended.
get_status(Ref) ->
	ListenerSup = ranch_server:get_listener_sup(Ref),
	Children = supervisor:which_children(ListenerSup),
	case lists:keyfind(ranch_acceptors_sup, 1, Children) of
		{_, undefined, _, _} ->
			suspended;
		_ ->
			running
	end.

-spec get_addr(ref()) -> {inet:ip_address(), inet:port_number()} | {undefined, undefined}.
get_addr(Ref) ->
	ranch_server:get_addr(Ref).

-spec get_port(ref()) -> inet:port_number() | undefined.
get_port(Ref) ->
	{_, Port} = get_addr(Ref),
	Port.

-spec get_max_connections(ref()) -> max_conns().
get_max_connections(Ref) ->
	ranch_server:get_max_connections(Ref).

-spec set_max_connections(ref(), max_conns()) -> ok.
set_max_connections(Ref, MaxConnections) ->
	ranch_server:set_max_connections(Ref, MaxConnections).

-spec get_transport_options(ref()) -> any().
get_transport_options(Ref) ->
	ranch_server:get_transport_options(Ref).

-spec set_transport_options(ref(), opts()) -> ok | {error, running}.
set_transport_options(Ref, TransOpts0) ->
	TransOpts = normalize_opts(TransOpts0),
	case get_status(Ref) of
		suspended ->
			ok = ranch_server:set_transport_options(Ref, TransOpts);
		running ->
			{error, running}
	end.

-spec get_protocol_options(ref()) -> opts().
get_protocol_options(Ref) ->
	ranch_server:get_protocol_options(Ref).

-spec set_protocol_options(ref(), any()) -> ok.
set_protocol_options(Ref, Opts) ->
	ranch_server:set_protocol_options(Ref, Opts).

-spec info() -> [{any(), [{atom(), any()}]}].
info() ->
	[{Ref, listener_info(Ref, Pid)}
		|| {Ref, Pid} <- ranch_server:get_listener_sups()].

-spec info(ref()) -> [{atom(), any()}].
info(Ref) ->
	Pid = ranch_server:get_listener_sup(Ref),
	listener_info(Ref, Pid).

listener_info(Ref, Pid) ->
	[_, Transport, _, Protocol, _] = ranch_server:get_listener_start_args(Ref),
	ConnsSup = ranch_server:get_connections_sup(Ref),
	Status = get_status(Ref),
	{IP, Port} = get_addr(Ref),
	MaxConns = get_max_connections(Ref),
	TransOpts = ranch_server:get_transport_options(Ref),
	ProtoOpts = get_protocol_options(Ref),
	[
		{pid, Pid},
		{status, Status},
		{ip, IP},
		{port, Port},
		{max_connections, MaxConns},
		{active_connections, ranch_conns_sup:active_connections(ConnsSup)},
		{all_connections, proplists:get_value(active, supervisor:count_children(ConnsSup))},
		{transport, Transport},
		{transport_options, TransOpts},
		{protocol, Protocol},
		{protocol_options, ProtoOpts}
	].

-spec procs(ref(), acceptors | connections) -> [pid()].
procs(Ref, acceptors) ->
	procs1(Ref, ranch_acceptors_sup);
procs(Ref, connections) ->
	procs1(Ref, ranch_conns_sup).

procs1(Ref, Sup) ->
	ListenerSup = ranch_server:get_listener_sup(Ref),
	{_, SupPid, _, _} = lists:keyfind(Sup, 1,
		supervisor:which_children(ListenerSup)),
	try
		[Pid || {_, Pid, _, _} <- supervisor:which_children(SupPid)]
	catch exit:{noproc, _} when Sup =:= ranch_acceptors_sup ->
		[]
	end.

-spec wait_for_connections
	(ref(), '>' | '>=' | '==' | '=<', non_neg_integer()) -> ok;
	(ref(), '<', pos_integer()) -> ok.
wait_for_connections(Ref, Op, NumConns) ->
	wait_for_connections(Ref, Op, NumConns, 1000).

-spec wait_for_connections
	(ref(), '>' | '>=' | '==' | '=<', non_neg_integer(), non_neg_integer()) -> ok;
	(ref(), '<', pos_integer(), non_neg_integer()) -> ok.
wait_for_connections(Ref, Op, NumConns, Interval) ->
	validate_op(Op, NumConns),
	validate_num_conns(NumConns),
	validate_interval(Interval),
	wait_for_connections_loop(Ref, Op, NumConns, Interval).

validate_op('>', _) -> ok;
validate_op('>=', _) -> ok;
validate_op('==', _) -> ok;
validate_op('=<', _) -> ok;
validate_op('<', NumConns) when NumConns > 0 -> ok;
validate_op(_, _) -> error(badarg).

validate_num_conns(NumConns) when is_integer(NumConns), NumConns >= 0 -> ok;
validate_num_conns(_) -> error(badarg).

validate_interval(Interval) when is_integer(Interval), Interval >= 0 -> ok;
validate_interval(_) -> error(badarg).

wait_for_connections_loop(Ref, Op, NumConns, Interval) ->
	CurConns = try
		ConnsSup = ranch_server:get_connections_sup(Ref),
		proplists:get_value(active, supervisor:count_children(ConnsSup))
	catch _:_ ->
		0
	end,
	case erlang:Op(CurConns, NumConns) of
		true ->
			ok;
		false when Interval =:= 0 ->
			wait_for_connections_loop(Ref, Op, NumConns, Interval);
		false ->
			timer:sleep(Interval),
			wait_for_connections_loop(Ref, Op, NumConns, Interval)
	end.

-spec filter_options([inet | inet6 | {atom(), any()} | {raw, any(), any(), any()}],
	[atom()], Acc) -> Acc when Acc :: [any()].
filter_options(UserOptions, DisallowedKeys, DefaultOptions) ->
	AllowedOptions = filter_user_options(UserOptions, DisallowedKeys),
	lists:foldl(fun merge_options/2, DefaultOptions, AllowedOptions).

%% 2-tuple options.
filter_user_options([Opt = {Key, _}|Tail], DisallowedKeys) ->
	case lists:member(Key, DisallowedKeys) of
		false ->
			[Opt|filter_user_options(Tail, DisallowedKeys)];
		true ->
			filter_options_warning(Opt),
			filter_user_options(Tail, DisallowedKeys)
	end;
%% Special option forms.
filter_user_options([inet|Tail], DisallowedKeys) ->
	[inet|filter_user_options(Tail, DisallowedKeys)];
filter_user_options([inet6|Tail], DisallowedKeys) ->
	[inet6|filter_user_options(Tail, DisallowedKeys)];
filter_user_options([Opt = {raw, _, _, _}|Tail], DisallowedKeys) ->
	[Opt|filter_user_options(Tail, DisallowedKeys)];
filter_user_options([Opt|Tail], DisallowedKeys) ->
	filter_options_warning(Opt),
	filter_user_options(Tail, DisallowedKeys);
filter_user_options([], _) ->
	[].

filter_options_warning(Opt) ->
	Logger = case get(logger) of
		undefined -> error_logger;
		Logger0 -> Logger0
	end,
	log(warning,
		"Transport option ~p unknown or invalid.~n",
		[Opt], Logger).

merge_options({Key, _} = Option, OptionList) ->
	lists:keystore(Key, 1, OptionList, Option);
merge_options(Option, OptionList) ->
	[Option|OptionList].

-spec set_option_default(Opts, atom(), any())
	-> Opts when Opts :: [{atom(), any()}].
set_option_default(Opts, Key, Value) ->
	case lists:keymember(Key, 1, Opts) of
		true -> Opts;
		false -> [{Key, Value}|Opts]
	end.

-spec require([atom()]) -> ok.
require([]) ->
	ok;
require([App|Tail]) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end,
	require(Tail).

-spec log(logger:level(), io:format(), list(), module() | #{logger => module()}) -> ok.
log(Level, Format, Args, Logger) when is_atom(Logger) ->
	log(Level, Format, Args, #{logger => Logger});
log(Level, Format, Args, #{logger := Logger})
		when Logger =/= error_logger ->
	_ = Logger:Level(Format, Args),
	ok;
%% We use error_logger by default. Because error_logger does
%% not have all the levels we accept we have to do some
%% mapping to error_logger functions.
log(Level, Format, Args, _) ->
	Function = case Level of
		emergency -> error_msg;
		alert -> error_msg;
		critical -> error_msg;
		error -> error_msg;
		warning -> warning_msg;
		notice -> warning_msg;
		info -> info_msg;
		debug -> info_msg
	end,
	error_logger:Function(Format, Args).
