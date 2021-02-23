%% The MIT License

%% Copyright (c) 2010-2013 alisdair sullivan <alisdairsullivan@yahoo.ca>

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


-module(jsx).

-export([encode/1, encode/2, decode/1, decode/2]).
-export([is_json/1, is_json/2, is_term/1, is_term/2]).
-export([format/1, format/2, minify/1, prettify/1]).
-export([consult/1, consult/2]).
-export([encoder/3, decoder/3, parser/3]).
-export([resume/3]).
-export([maps_support/0]).

-export_type([json_term/0, json_text/0, token/0]).
-export_type([encoder/0, decoder/0, parser/0, internal_state/0]).
-export_type([config/0]).


-ifdef(TEST).
%% data and helper functions for tests
-export([test_cases/0, special_test_cases/0]).
-export([init/1, handle_event/2]).
-endif.


-ifndef(maps_support).
-type json_term() :: [{binary() | atom(), json_term()}] | [{},...]
    | [json_term()] | []
    | true | false | null
    | integer() | float()
    | binary() | atom()
    | calendar:datetime().
-endif.

-ifdef(maps_support).
-type json_term() :: [{binary() | atom(), json_term()}] | [{},...]
    | [json_term()] | []
    | map()
    | true | false | null
    | integer() | float()
    | binary() | atom()
    | calendar:datetime().
-endif.

-type json_text() :: binary().

-type config() :: jsx_config:config().

-spec encode(Source::json_term()) -> json_text().

encode(Source) -> encode(Source, []).

-spec encode(Source::json_term(), Config::jsx_to_json:config()) -> json_text() | {incomplete, encoder()}.

encode(Source, Config) -> jsx_to_json:to_json(Source, Config).


-spec decode(Source::json_text()) -> json_term().

decode(Source) -> decode(Source, []).

-spec decode(Source::json_text(), Config::jsx_to_term:config()) -> json_term() | {incomplete, decoder()}.

decode(Source, Config) -> jsx_to_term:to_term(Source, Config).


-spec format(Source::json_text()) -> json_text().

format(Source) -> format(Source, []).

-spec format(Source::json_text(), Config::jsx_to_json:config()) -> json_text() | {incomplete, decoder()}.

format(Source, Config) -> jsx_to_json:format(Source, Config).


-spec minify(Source::json_text()) -> json_text().

minify(Source) -> format(Source, []).


-spec prettify(Source::json_text()) -> json_text().

prettify(Source) -> format(Source, [space, {indent, 2}]).


-spec is_json(Source::any()) -> boolean().

is_json(Source) -> is_json(Source, []).

-spec is_json(Source::any(), Config::jsx_verify:config()) -> boolean() | {incomplete, decoder()}.

is_json(Source, Config) -> jsx_verify:is_json(Source, Config).


-spec is_term(Source::any()) -> boolean().

is_term(Source) -> is_term(Source, []).

-spec is_term(Source::any(), Config::jsx_verify:config()) -> boolean() | {incomplete, encoder()}.

is_term(Source, Config) -> jsx_verify:is_term(Source, Config).


-spec consult(File::file:name_all()) -> list(json_term()).

consult(File) -> consult(File, []).

-spec consult(File::file:name_all(), Config::jsx_to_term:config()) -> list(json_term()).

consult(File, Config) -> jsx_consult:consult(File, Config).


-type decoder() :: fun((json_text() | end_stream | end_json) -> any()).

-spec decoder(Handler::module(), State::any(), Config::list()) -> decoder().

decoder(Handler, State, Config) -> jsx_decoder:decoder(Handler, State, Config).


-type encoder() :: fun((json_term() | end_stream | end_json) -> any()).

-spec encoder(Handler::module(), State::any(), Config::list()) -> encoder().

encoder(Handler, State, Config) -> jsx_encoder:encoder(Handler, State, Config).


-type token() :: [token()]
    | start_object
    | end_object
    | start_array
    | end_array
    | {key, binary()}
    | {string, binary()}
    | binary()
    | {number, integer() | float()}
    | {integer, integer()}
    | {float, float()}
    | integer()
    | float()
    | {literal, true}
    | {literal, false}
    | {literal, null}
    | true
    | false
    | null
    | end_json.


-type parser() :: fun((token() | end_stream) -> any()).

-spec parser(Handler::module(), State::any(), Config::list()) -> parser().

parser(Handler, State, Config) -> jsx_parser:parser(Handler, State, Config).

-opaque internal_state() :: tuple().

-spec resume(Term::json_text() | token(), InternalState::internal_state(), Config::list()) -> any().

resume(Term, {decoder, State, Handler, Acc, Stack}, Config) ->
    jsx_decoder:resume(Term, State, Handler, Acc, Stack, jsx_config:parse_config(Config));
resume(Term, {parser, State, Handler, Stack}, Config) ->
    jsx_parser:resume(Term, State, Handler, Stack, jsx_config:parse_config(Config)).


-spec maps_support() -> boolean().

-ifndef(maps_support).
maps_support() -> false.
-endif.
-ifdef(maps_support).
maps_support() -> true.
-endif.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


%% test handler
init([]) -> [].

handle_event(end_json, State) -> lists:reverse([end_json] ++ State);
handle_event(Event, State) -> [Event] ++ State.


test_cases() ->
    empty_array()
    ++ nested_array()
    ++ empty_object()
    ++ nested_object()
    ++ strings()
    ++ literals()
    ++ integers()
    ++ floats()
    ++ compound_object().

%% segregate these so we can skip them in `jsx_to_term`
special_test_cases() -> special_objects() ++ special_array().


empty_array() -> [{"[]", <<"[]">>, [], [start_array, end_array]}].


nested_array() ->
    [{
        "[[[]]]",
        <<"[[[]]]">>,
        [[[]]],
        [start_array, start_array, start_array, end_array, end_array, end_array]
    }].


empty_object() -> [{"{}", <<"{}">>, [{}], [start_object, end_object]}].


nested_object() ->
    [{
        "{\"key\":{\"key\":{}}}",
        <<"{\"key\":{\"key\":{}}}">>,
        [{<<"key">>, [{<<"key">>, [{}]}]}],
        [
            start_object,
                {key, <<"key">>},
                start_object,
                    {key, <<"key">>},
                    start_object,
                    end_object,
                end_object,
            end_object
        ]
    }].


naked_strings() ->
    Raw = [
        "",
        "hello world"
    ],
    [
        {
            String,
            <<"\"", (list_to_binary(String))/binary, "\"">>,
            list_to_binary(String),
            [{string, list_to_binary(String)}]
        }
        || String <- Raw
    ].


strings() ->
    naked_strings()
    ++ [ wrap_with_array(Test) || Test <- naked_strings() ]
    ++ [ wrap_with_object(Test) || Test <- naked_strings() ].


naked_integers() ->
    Raw = [
        1, 2, 3,
        127, 128, 129,
        255, 256, 257,
        65534, 65535, 65536,
        18446744073709551616,
        18446744073709551617
    ],
    [
        {
            integer_to_list(X),
            list_to_binary(integer_to_list(X)),
            X,
            [{integer, X}]
        }
        || X <- Raw ++ [ -1 * Y || Y <- Raw ] ++ [0]
    ].


integers() ->
    naked_integers()
    ++ [ wrap_with_array(Test) || Test <- naked_integers() ]
    ++ [ wrap_with_object(Test) || Test <- naked_integers() ].


naked_floats() ->
    Raw = [
        0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
        1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9,
        1234567890.0987654321,
        0.0e0,
        1234567890.0987654321e16,
        0.1e0, 0.1e1, 0.1e2, 0.1e4, 0.1e8, 0.1e16, 0.1e308,
        1.0e0, 1.0e1, 1.0e2, 1.0e4, 1.0e8, 1.0e16, 1.0e308,
        2.2250738585072014e-308,    %% min normalized float
        1.7976931348623157e308,     %% max normalized float
        5.0e-324,                   %% min denormalized float
        2.225073858507201e-308      %% max denormalized float
    ],
    [
        {
            sane_float_to_list(X),
            list_to_binary(sane_float_to_list(X)),
            X,
            [{float, X}]
        }
        || X <- Raw ++ [ -1 * Y || Y <- Raw ]
    ].


floats() ->
    naked_floats()
    ++ [ wrap_with_array(Test) || Test <- naked_floats() ]
    ++ [ wrap_with_object(Test) || Test <- naked_floats() ].


naked_literals() ->
    [
        {
            atom_to_list(Literal),
            atom_to_binary(Literal, unicode),
            Literal,
            [{literal, Literal}]
        }
        || Literal <- [true, false, null]
    ].


literals() ->
    naked_literals()
    ++ [ wrap_with_array(Test) || Test <- naked_literals() ]
    ++ [ wrap_with_object(Test) || Test <- naked_literals() ].


compound_object() ->
    [{
        "[{\"alpha\":[1,2,3],\"beta\":{\"alpha\":[1.0,2.0,3.0],\"beta\":[true,false]}},[{}]]",
        <<"[{\"alpha\":[1,2,3],\"beta\":{\"alpha\":[1.0,2.0,3.0],\"beta\":[true,false]}},[{}]]">>,
        [[{<<"alpha">>, [1, 2, 3]}, {<<"beta">>, [{<<"alpha">>, [1.0, 2.0, 3.0]}, {<<"beta">>, [true, false]}]}], [[{}]]],
        [
            start_array,
                start_object,
                    {key, <<"alpha">>},
                    start_array,
                        {integer, 1},
                        {integer, 2},
                        {integer, 3},
                    end_array,
                    {key, <<"beta">>},
                    start_object,
                        {key, <<"alpha">>},
                        start_array,
                            {float, 1.0},
                            {float, 2.0},
                            {float, 3.0},
                        end_array,
                        {key, <<"beta">>},
                        start_array,
                            {literal, true},
                            {literal, false},
                        end_array,
                    end_object,
                end_object,
                start_array,
                    start_object,
                    end_object,
                end_array,
            end_array
        ]
    }].


special_objects() ->
    [
        {
            "[{key, atom}]",
            <<"{\"key\":\"atom\"}">>,
            [{key, atom}],
            [start_object, {key, <<"key">>}, {string, <<"atom">>}, end_object]
        },
        {
            "[{1, true}]",
            <<"{\"1\":true}">>,
            [{1, true}],
            [start_object, {key, <<"1">>}, {literal, true}, end_object]
        }
    ].


special_array() ->
    [    
        {
            "[foo, bar]",
            <<"[\"foo\",\"bar\"]">>,
            [foo, bar],
            [start_array, {string, <<"foo">>}, {string, <<"bar">>}, end_array]
        }
    ].


wrap_with_array({Title, JSON, Term, Events}) ->
    {
        "[" ++ Title ++ "]",
        <<"[", JSON/binary, "]">>,
        [Term],
        [start_array] ++ Events ++ [end_array]
    }.


wrap_with_object({Title, JSON, Term, Events}) ->
    {
        "{\"key\":" ++ Title ++ "}",
        <<"{\"key\":", JSON/binary, "}">>,
        [{<<"key">>, Term}],
        [start_object, {key, <<"key">>}] ++ Events ++ [end_object]
    }.


sane_float_to_list(X) ->
    [Output] = io_lib:format("~p", [X]),
    Output.


incremental_decode(JSON) ->
    Final = lists:foldl(
        fun(Byte, Decoder) -> {incomplete, F} = Decoder(Byte), F end,
        decoder(jsx, [], [stream]),
        json_to_bytes(JSON)
    ),
    Final(end_stream).


incremental_parse(Events) ->
    Final = lists:foldl(
        fun(Event, Parser) -> {incomplete, F} = Parser(Event), F end,
        parser(?MODULE, [], [stream]),
        lists:map(fun(X) -> [X] end, Events)
    ),
    Final(end_stream).


%% used to convert a json text into a list of codepoints to be incrementally
%% parsed
json_to_bytes(JSON) -> json_to_bytes(JSON, []).

json_to_bytes(<<>>, Acc) -> [<<>>] ++ lists:reverse(Acc);
json_to_bytes(<<X, Rest/binary>>, Acc) -> json_to_bytes(Rest, [<<X>>] ++ Acc).


%% actual tests!
decode_test_() ->
    Data = test_cases(),
    [{Title, ?_assertEqual(Events ++ [end_json], (decoder(?MODULE, [], []))(JSON))}
        || {Title, JSON, _, Events} <- Data
    ] ++
    [{Title ++ " (incremental)", ?_assertEqual(Events ++ [end_json], incremental_decode(JSON))}
        || {Title, JSON, _, Events} <- Data
    ].


parse_test_() ->
    Data = test_cases(),
    [{Title, ?_assertEqual(Events ++ [end_json], (parser(?MODULE, [], []))(Events ++ [end_json]))}
        || {Title, _, _, Events} <- Data
    ] ++
    [{Title ++ " (incremental)", ?_assertEqual(Events ++ [end_json], incremental_parse(Events))}
        || {Title, _, _, Events} <- Data
    ].


encode_test_() ->
    Data = test_cases(),
    [
        {
            Title, ?_assertEqual(
                Events ++ [end_json],
                (jsx:encoder(jsx, [], []))(Term)
            )
        } || {Title, _, Term, Events} <- Data
    ].

end_stream_test_() ->
    Tokens = [start_object, end_object, end_json],
    [
        {"encoder end_stream", ?_assertEqual(
            Tokens,
            begin
                {incomplete, F} = (jsx:parser(jsx, [], [stream]))([start_object, end_object]),
                F(end_stream)
            end
        )},
        {"encoder end_json", ?_assertEqual(
            Tokens,
            begin
                {incomplete, F} = (jsx:parser(jsx, [], [stream]))([start_object, end_object]),
                F(end_json)
            end
        )},
        {"decoder end_stream", ?_assertEqual(
            Tokens,
            begin {incomplete, F} = (jsx:decoder(jsx, [], [stream]))(<<"{}">>), F(end_stream) end
        )},
        {"decoder end_json", ?_assertEqual(
            Tokens,
            begin {incomplete, F} = (jsx:decoder(jsx, [], [stream]))(<<"{}">>), F(end_json) end
        )}
    ].


-endif.
