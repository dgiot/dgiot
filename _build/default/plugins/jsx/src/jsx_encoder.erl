%% The MIT License

%% Copyright (c) 2010-2013 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.


-module(jsx_encoder).

-export([encoder/3, encode/1, encode/2]).

-spec encoder(Handler::module(), State::any(), Config::list()) -> jsx:encoder().

encoder(Handler, State, Config) ->
    Parser = jsx:parser(Handler, State, Config),
    fun(Term) -> Parser(encode(Term) ++ [end_json]) end.


-spec encode(Term::any()) -> any().

encode(Term) -> encode(Term, ?MODULE).


-spec encode(Term::any(), EntryPoint::module()) -> any().

-ifndef(maps_support).
encode(Term, EntryPoint) -> encode_(Term, EntryPoint).
-endif.

-ifdef(maps_support).
encode(Map, _EntryPoint) when is_map(Map), map_size(Map) < 1 ->
    [start_object, end_object];
encode(Term, EntryPoint) when is_map(Term) ->
    [start_object] ++ unpack(Term, EntryPoint);
encode(Term, EntryPoint) -> encode_(Term, EntryPoint).
-endif.

encode_([], _EntryPoint) -> [start_array, end_array];
encode_([{}], _EntryPoint) -> [start_object, end_object];

%% datetime special case
encode_([{{_,_,_},{_,_,_}} = DateTime|Rest], EntryPoint) ->
    [start_array] ++ [DateTime] ++ unhitch(Rest, EntryPoint);
encode_([{_, _}|_] = Term, EntryPoint) ->
    [start_object] ++ unzip(Term, EntryPoint);
encode_(Term, EntryPoint) when is_list(Term) ->
    [start_array] ++ unhitch(Term, EntryPoint);

encode_(Else, _EntryPoint) -> [Else].


unzip([{K, V}|Rest], EntryPoint) when is_integer(K); is_binary(K); is_atom(K) ->
    [K] ++ EntryPoint:encode(V, EntryPoint) ++ unzip(Rest, EntryPoint);
unzip([], _) -> [end_object];
unzip(_, _) -> erlang:error(badarg).


unhitch([V|Rest], EntryPoint) ->
    EntryPoint:encode(V, EntryPoint) ++ unhitch(Rest, EntryPoint);
unhitch([], _) -> [end_array].


-ifdef(maps_support).
unpack(Map, EntryPoint) -> unpack(Map, maps:keys(Map), EntryPoint).

unpack(Map, [K|Rest], EntryPoint) when is_integer(K); is_binary(K); is_atom(K) ->
    [K] ++ EntryPoint:encode(maps:get(K, Map), EntryPoint) ++ unpack(Map, Rest, EntryPoint);
unpack(_, [], _) -> [end_object].
-endif.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


parser(Term, Opts) -> (jsx:parser(jsx, [], Opts))(Term).


error_test_() ->
    [
        {"value error", ?_assertError(badarg, parser(self(), []))},
        {"string error", ?_assertError(badarg, parser(<<239, 191, 191>>, [strict]))}
    ].

custom_error_handler_test_() ->
    Error = fun(Term, {_, State, _, _}, _) -> {State, Term} end,
    [
        {"value error", ?_assertEqual(
            {value, [self()]},
            parser(self(), [{error_handler, Error}])
        )},
        {"string error", ?_assertEqual(
            {value, [{string, <<237, 160, 128>>}]},
            parser(<<237, 160, 128>>, [{error_handler, Error}, strict])
        )}
    ].

improper_lists_test_() ->
    [
        {"improper proplist", ?_assertError(
          badarg,
          encode([{<<"key">>, <<"value">>}, false])
        )},
        {"improper list", ?_assertError(
          badarg,
          encode([{literal, true}, false, null])
        )}
    ].

-endif.
