%% Copyright (c) 2012-2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(ranch_server).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([set_new_listener_opts/5]).
-export([cleanup_listener_opts/1]).
-export([set_connections_sup/2]).
-export([get_connections_sup/1]).
-export([get_connections_sups/0]).
-export([set_listener_sup/2]).
-export([get_listener_sup/1]).
-export([get_listener_sups/0]).
-export([set_addr/2]).
-export([get_addr/1]).
-export([set_max_connections/2]).
-export([get_max_connections/1]).
-export([set_transport_options/2]).
-export([get_transport_options/1]).
-export([set_protocol_options/2]).
-export([get_protocol_options/1]).
-export([get_listener_start_args/1]).
-export([count_connections/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TAB, ?MODULE).

-type monitors() :: [{{reference(), pid()}, any()}].
-record(state, {
	monitors = [] :: monitors()
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec set_new_listener_opts(ranch:ref(), ranch:max_conns(), any(), any(), [any()]) -> ok.
set_new_listener_opts(Ref, MaxConns, TransOpts, ProtoOpts, StartArgs) ->
	gen_server:call(?MODULE, {set_new_listener_opts, Ref, MaxConns, TransOpts, ProtoOpts, StartArgs}).

-spec cleanup_listener_opts(ranch:ref()) -> ok.
cleanup_listener_opts(Ref) ->
	_ = ets:delete(?TAB, {addr, Ref}),
	_ = ets:delete(?TAB, {max_conns, Ref}),
	_ = ets:delete(?TAB, {trans_opts, Ref}),
	_ = ets:delete(?TAB, {proto_opts, Ref}),
	_ = ets:delete(?TAB, {listener_start_args, Ref}),
	%% We also remove the pid of the connections supervisor.
	%% Depending on the timing, it might already have been deleted
	%% when we handled the monitor DOWN message. However, in some
	%% cases when calling stop_listener followed by get_connections_sup,
	%% we could end up with the pid still being returned, when we
	%% expected a crash (because the listener was stopped).
	%% Deleting it explictly here removes any possible confusion.
	_ = ets:delete(?TAB, {conns_sup, Ref}),
	%% Ditto for the listener supervisor.
	_ = ets:delete(?TAB, {listener_sup, Ref}),
	ok.

-spec set_connections_sup(ranch:ref(), pid()) -> ok.
set_connections_sup(Ref, Pid) ->
	gen_server:call(?MODULE, {set_connections_sup, Ref, Pid}).

-spec get_connections_sup(ranch:ref()) -> pid().
get_connections_sup(Ref) ->
	ets:lookup_element(?TAB, {conns_sup, Ref}, 2).

-spec get_connections_sups() -> [{ranch:ref(), pid()}].
get_connections_sups() ->
	[{Ref, Pid} || [Ref, Pid] <- ets:match(?TAB, {{conns_sup, '$1'}, '$2'})].

-spec set_listener_sup(ranch:ref(), pid()) -> ok.
set_listener_sup(Ref, Pid) ->
	gen_server:call(?MODULE, {set_listener_sup, Ref, Pid}).

-spec get_listener_sup(ranch:ref()) -> pid().
get_listener_sup(Ref) ->
	ets:lookup_element(?TAB, {listener_sup, Ref}, 2).

-spec get_listener_sups() -> [{ranch:ref(), pid()}].
get_listener_sups() ->
	[{Ref, Pid} || [Ref, Pid] <- ets:match(?TAB, {{listener_sup, '$1'}, '$2'})].

-spec set_addr(ranch:ref(), {inet:ip_address(), inet:port_number()} | {undefined, undefined}) -> ok.
set_addr(Ref, Addr) ->
	gen_server:call(?MODULE, {set_addr, Ref, Addr}).

-spec get_addr(ranch:ref()) -> {inet:ip_address(), inet:port_number()} | {undefined, undefined}.
get_addr(Ref) ->
	ets:lookup_element(?TAB, {addr, Ref}, 2).

-spec set_max_connections(ranch:ref(), ranch:max_conns()) -> ok.
set_max_connections(Ref, MaxConnections) ->
	gen_server:call(?MODULE, {set_max_conns, Ref, MaxConnections}).

-spec get_max_connections(ranch:ref()) -> ranch:max_conns().
get_max_connections(Ref) ->
	ets:lookup_element(?TAB, {max_conns, Ref}, 2).

-spec set_transport_options(ranch:ref(), any()) -> ok.
set_transport_options(Ref, TransOpts) ->
	gen_server:call(?MODULE, {set_trans_opts, Ref, TransOpts}).

-spec get_transport_options(ranch:ref()) -> any().
get_transport_options(Ref) ->
	ets:lookup_element(?TAB, {trans_opts, Ref}, 2).

-spec set_protocol_options(ranch:ref(), any()) -> ok.
set_protocol_options(Ref, ProtoOpts) ->
	gen_server:call(?MODULE, {set_proto_opts, Ref, ProtoOpts}).

-spec get_protocol_options(ranch:ref()) -> any().
get_protocol_options(Ref) ->
	ets:lookup_element(?TAB, {proto_opts, Ref}, 2).

-spec get_listener_start_args(ranch:ref()) -> [any()].
get_listener_start_args(Ref) ->
	ets:lookup_element(?TAB, {listener_start_args, Ref}, 2).

-spec count_connections(ranch:ref()) -> non_neg_integer().
count_connections(Ref) ->
	ranch_conns_sup:active_connections(get_connections_sup(Ref)).

%% gen_server.

init([]) ->
	ConnMonitors = [{{erlang:monitor(process, Pid), Pid}, {conns_sup, Ref}} ||
		[Ref, Pid] <- ets:match(?TAB, {{conns_sup, '$1'}, '$2'})],
	ListenerMonitors = [{{erlang:monitor(process, Pid), Pid}, {listener_sup, Ref}} ||
		[Ref, Pid] <- ets:match(?TAB, {{listener_sup, '$1'}, '$2'})],
	{ok, #state{monitors=ConnMonitors++ListenerMonitors}}.

handle_call({set_new_listener_opts, Ref, MaxConns, TransOpts, ProtoOpts, StartArgs}, _, State) ->
	ets:insert_new(?TAB, {{max_conns, Ref}, MaxConns}),
	ets:insert_new(?TAB, {{trans_opts, Ref}, TransOpts}),
	ets:insert_new(?TAB, {{proto_opts, Ref}, ProtoOpts}),
	ets:insert_new(?TAB, {{listener_start_args, Ref}, StartArgs}),
	{reply, ok, State};
handle_call({set_connections_sup, Ref, Pid}, _, State0) ->
	State = set_monitored_process({conns_sup, Ref}, Pid, State0),
	{reply, ok, State};
handle_call({set_listener_sup, Ref, Pid}, _, State0) ->
	State = set_monitored_process({listener_sup, Ref}, Pid, State0),
	{reply, ok, State};
handle_call({set_addr, Ref, Addr}, _, State) ->
	true = ets:insert(?TAB, {{addr, Ref}, Addr}),
	{reply, ok, State};
handle_call({set_max_conns, Ref, MaxConns}, _, State) ->
	ets:insert(?TAB, {{max_conns, Ref}, MaxConns}),
	ConnsSup = get_connections_sup(Ref),
	ConnsSup ! {set_max_conns, MaxConns},
	{reply, ok, State};
handle_call({set_trans_opts, Ref, Opts}, _, State) ->
	ets:insert(?TAB, {{trans_opts, Ref}, Opts}),
	{reply, ok, State};
handle_call({set_proto_opts, Ref, Opts}, _, State) ->
	ets:insert(?TAB, {{proto_opts, Ref}, Opts}),
	ConnsSup = get_connections_sup(Ref),
	ConnsSup ! {set_opts, Opts},
	{reply, ok, State};
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, Reason},
		State=#state{monitors=Monitors}) ->
	{_, TypeRef} = lists:keyfind({MonitorRef, Pid}, 1, Monitors),
	ok = case {TypeRef, Reason} of
		{{listener_sup, Ref}, normal} ->
			cleanup_listener_opts(Ref);
		{{listener_sup, Ref}, shutdown} ->
			cleanup_listener_opts(Ref);
		{{listener_sup, Ref}, {shutdown, _}} ->
			cleanup_listener_opts(Ref);
		_ ->
			_ = ets:delete(?TAB, TypeRef),
			ok
	end,
	Monitors2 = lists:keydelete({MonitorRef, Pid}, 1, Monitors),
	{noreply, State#state{monitors=Monitors2}};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

set_monitored_process(Key, Pid, State=#state{monitors=Monitors0}) ->
	%% First we cleanup the monitor if a residual one exists.
	%% This can happen during crashes when the restart is faster
	%% than the cleanup.
	Monitors = case lists:keytake(Key, 2, Monitors0) of
		false ->
			Monitors0;
		{value, {{OldMonitorRef, _}, _}, Monitors1} ->
			true = erlang:demonitor(OldMonitorRef, [flush]),
			Monitors1
	end,
	%% Then we unconditionally insert in the ets table.
	%% If residual data is there, it will be overwritten.
	true = ets:insert(?TAB, {Key, Pid}),
	%% Finally we start monitoring this new process.
	MonitorRef = erlang:monitor(process, Pid),
	State#state{monitors=[{{MonitorRef, Pid}, Key}|Monitors]}.
