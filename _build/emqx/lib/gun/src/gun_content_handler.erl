%% Copyright (c) 2017-2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(gun_content_handler).

-export([init/5]).
-export([handle/3]).
-export([check_option/1]).

-type opt() :: [module() | {module(), map()}].
-export_type([opt/0]).

-type state() :: opt() | [{module(), any()}].
-export_type([state/0]).

-callback init(pid(), any(), cow_http:status(),
	cow_http:headers(), map()) -> {ok, any()} | disable.
%% @todo Make fin | nofin its own type.
-callback handle(fin | nofin, any(), State)
	-> {ok, any(), State} | {done, State} when State::any().

-spec init(pid(), any(), cow_http:status(),
	cow_http:headers(), State) -> State when State::state().
init(_, _, _, _, []) ->
	[];
init(ReplyTo, StreamRef, Status, Headers, [Handler|Tail]) ->
	{Mod, Opts} = case Handler of
		Tuple = {_, _} -> Tuple;
		Atom -> {Atom, #{}}
	end,
	case Mod:init(ReplyTo, StreamRef, Status, Headers, Opts) of
		{ok, State} -> [{Mod, State}|init(ReplyTo, StreamRef, Status, Headers, Tail)];
		disable -> init(ReplyTo, StreamRef, Status, Headers, Tail)
	end.

-spec handle(fin | nofin, any(), State) -> State when State::state().
handle(_, _, []) ->
	[];
handle(IsFin, Data0, [{Mod, State0}|Tail]) ->
	case Mod:handle(IsFin, Data0, State0) of
		{ok, Data, State} -> [{Mod, State}|handle(IsFin, Data, Tail)];
		{done, State} -> [{Mod, State}|Tail]
	end.

-spec check_option(list()) -> ok | error.
check_option([]) ->
	error;
check_option(Opt) ->
	check_option1(Opt).

check_option1([]) ->
	ok;
check_option1([Atom|Tail]) when is_atom(Atom) ->
	check_option1(Tail);
check_option1([{Atom, #{}}|Tail]) when is_atom(Atom) ->
	check_option1(Tail);
check_option1(_) ->
	error.
