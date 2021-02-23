%% Copyright (c) 2012, Magnus Klaar <klaar@ninenines.eu>
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

%% @doc Accessor function for goldrush event terms.
-module(gre).

-export([
    make/2,
    has/2,
    fetch/2,
    append/2,
    merge/2,
    find/2,
    keys/1,
    pairs/1
]).

-type event() :: {list, [{atom(), term()}]}.
-export_type([event/0]).

%% @doc Construct an event term.
-spec make(term(), [list]) -> event().
make(Term, [Type]) ->
    {Type, Term}.


%% @doc Check if a field exists in an event.
-spec has(atom(), event()) -> boolean().
has(Key, {list, List}) ->
    lists:keymember(Key, 1, List).

-spec append(term(), event()) -> event().
append(KeyVal, {list, List}) ->
    {list, [KeyVal|List]}.

-spec merge(event(), event()) -> event().
merge({list, AList}, {list, BList}) ->
    {list, lists:merge(AList, BList)}.

%% @doc Get the value of a field in an event.
%% The field is expected to exist in the event.
-spec fetch(atom(), event()) -> term().
fetch(Key, {list, List}=Event) ->
    case lists:keyfind(Key, 1, List) of
        {_, Value} -> Value;
        false -> erlang:error(badarg, [Key, Event])
    end.


%% @doc Find the value of a field in an event.
%% This is equivalent to testing if a field exists using {@link has/2}
%% before accessing the value of the field using {@link fetch/2}.
-spec find(atom(), event()) -> {true, term()} | false.
find(Key, {list, List}) ->
    case lists:keyfind(Key, 1, List) of
        {_, Value} -> {true, Value};
        false -> false
    end.


%% @doc Get the names of all fields in an event.
-spec keys(event()) -> [atom()].
keys({list, List}) ->
    kv_keys_(List).

%% @private Get the names of all fields in a key-value list.
-spec kv_keys_([{atom(), term()}]) -> [atom()].
kv_keys_([{Key, _}|T]) ->
    [Key|kv_keys_(T)];
kv_keys_([]) ->
    [].

%% @doc Get the name and value of all fields in an event.
-spec pairs(event()) -> [{atom(), term()}].
pairs({list, List}) ->
    lists:sort(List).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

gre_test_() ->
    [?_assert(gre:has(a, gre:make([{a,1}], [list]))),
     ?_assertNot(gre:has(b, gre:make([{a,1}], [list]))),
     ?_assertEqual(1, gre:fetch(a, gre:make([{a,1}], [list]))),
     ?_assertError(badarg, gre:fetch(a, gre:make([], [list]))),
     ?_assertEqual([], gre:keys(gre:make([], [list]))),
     ?_assertEqual([a], gre:keys(gre:make([{a,1}], [list]))),
     ?_assertEqual([a,b], gre:keys(gre:make([{a,1},{b,2}], [list]))),
     ?_assertEqual([{a,1},{b,2}], gre:pairs(gre:make([{b,2},{a,1}], [list])))
    ].

-endif.
