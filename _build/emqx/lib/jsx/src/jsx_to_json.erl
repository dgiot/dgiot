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


-module(jsx_to_json).

-export([to_json/2, format/2]).
-export([init/1, handle_event/2]).
-export([start_json/0, start_json/1]).
-export([start_object/1, start_array/1, finish/1, insert/2, get_key/1, get_value/1]).


-record(config, {
    space = 0,
    indent = 0,
    depth = 0
}).

-type config() :: list().
-export_type([config/0]).


-spec to_json(Source::any(), Config::config()) -> binary().

to_json(Source, Config) when is_list(Config) ->
    (jsx:encoder(?MODULE, Config, jsx_config:extract_config(Config ++ [escaped_strings])))(Source).


-spec format(Source::binary(), Config::config()) -> binary().

format(Source, Config) when is_binary(Source) andalso is_list(Config) ->
    (jsx:decoder(?MODULE, Config, jsx_config:extract_config(Config ++ [escaped_strings])))(Source);
format(_, _) -> erlang:error(badarg).


parse_config(Config) -> parse_config(Config, #config{}).

parse_config([{space, Val}|Rest], Config) when is_integer(Val), Val > 0 ->
    parse_config(Rest, Config#config{space = Val});
parse_config([space|Rest], Config) ->
    parse_config(Rest, Config#config{space = 1});
parse_config([{indent, Val}|Rest], Config) when is_integer(Val), Val > 0 ->
    parse_config(Rest, Config#config{indent = Val});
parse_config([indent|Rest], Config) ->
    parse_config(Rest, Config#config{indent = 1});
parse_config([{K, _}|Rest] = Options, Config) ->
    case lists:member(K, jsx_config:valid_flags()) of
        true -> parse_config(Rest, Config)
        ; false -> erlang:error(badarg, [Options, Config])
    end;
parse_config([K|Rest] = Options, Config) ->
    case lists:member(K, jsx_config:valid_flags()) of
        true -> parse_config(Rest, Config)
        ; false -> erlang:error(badarg, [Options, Config])
    end;
parse_config([], Config) ->
    Config.


-define(start_object, <<"{">>).
-define(start_array, <<"[">>).
-define(end_object, <<"}">>).
-define(end_array, <<"]">>).
-define(colon, <<":">>).
-define(comma, <<",">>).
-define(quote, <<"\"">>).
-define(space, <<" ">>).
-define(newline, <<"\n">>).


-type state() :: {unicode:charlist(), #config{}}.
-spec init(Config::proplists:proplist()) -> state().

init(Config) -> {[], parse_config(Config)}.


-spec handle_event(Event::any(), State::state()) -> state().

handle_event(end_json, State) -> get_value(State);

handle_event(start_object, State) -> start_object(State);
handle_event(end_object, State) -> finish(State);

handle_event(start_array, State) -> start_array(State);
handle_event(end_array, State) -> finish(State);

handle_event({Type, Event}, {_, Config} = State) -> insert(encode(Type, Event, Config), State).


encode(string, String, _Config) ->
    [?quote, String, ?quote];
encode(key, Key, _Config) ->
    [?quote, Key, ?quote];
encode(literal, Literal, _Config) ->
    erlang:atom_to_list(Literal);
encode(integer, Integer, _Config) ->
    erlang:integer_to_list(Integer);
encode(float, Float, _Config) ->
    io_lib:format("~p", [Float]).


space(Config) ->
    case Config#config.space of
        0 -> <<>>
        ; X when X > 0 -> binary:copy(?space, X)
    end.


indent(Config) ->
    case Config#config.indent of
        0 -> <<>>
        ; X when X > 0 -> <<?newline/binary, (binary:copy(?space, X * Config#config.depth))/binary>>
    end.


indent_or_space(Config) ->
    case Config#config.indent > 0 of
        true -> indent(Config)
        ; false -> space(Config)
    end.


%% internal state is a stack and a config object
%%  `{Stack, Config}`
%% the stack is a list of in progress objects/arrays
%%  `[Current, Parent, Grandparent,...OriginalAncestor]`
%% an object has the representation on the stack of
%%  `{object, Object}`
%% of if there's a key with a yet to be matched value
%%  `{object, Key, Object}`
%% an array looks like
%%  `{array, Array}`
%% `Object` and `Array` are utf8 encoded binaries

start_json() -> {[], #config{}}.

start_json(Config) when is_list(Config) -> {[], parse_config(Config)}.

%% allocate a new object on top of the stack
start_object({Stack, Config = #config{depth = Depth}}) ->
    {[{object, ?start_object}] ++ Stack, Config#config{depth = Depth + 1}}.

%% allocate a new array on top of the stack
start_array({Stack, Config = #config{depth = Depth}}) ->
    {[{array, ?start_array}] ++ Stack, Config#config{depth = Depth + 1}}.

%% finish an object or array and insert it into the parent object if it exists
finish({Stack, Config = #config{depth = Depth}}) ->
    NewConfig = Config#config{depth = Depth - 1},
    finish_({Stack, NewConfig}).

finish_({[{object, <<"{">>}], Config}) -> {<<"{}">>, Config};
finish_({[{array, <<"[">>}], Config}) -> {<<"[]">>, Config};
finish_({[{object, <<"{">>}|Rest], Config}) -> insert(<<"{}">>, {Rest, Config});
finish_({[{array, <<"[">>}|Rest], Config}) -> insert(<<"[]">>, {Rest, Config});
finish_({[{object, Object}], Config}) ->
    {[Object, indent(Config), ?end_object], Config};
finish_({[{object, Object}|Rest], Config}) ->
    insert([Object, indent(Config), ?end_object], {Rest, Config});
finish_({[{array, Array}], Config}) ->
    {[Array, indent(Config), ?end_array], Config};
finish_({[{array, Array}|Rest], Config}) ->
    insert([Array, indent(Config), ?end_array], {Rest, Config});
finish_(_) -> erlang:error(badarg).

%% insert a value when there's no parent object or array
insert(Value, {[], Config}) ->
    {Value, Config};
%% insert a key or value into an object or array, autodetects the 'right' thing
insert(Key, {[{object, Object}|Rest], Config}) ->
    {[{object, Key, Object}] ++ Rest, Config};
insert(Value, {[{object, Key, ?start_object}|Rest], Config}) ->
    {
        [{object, [
            ?start_object,
            indent(Config),
            Key,
            ?colon,
            space(Config),
            Value
        ]}] ++ Rest,
        Config
    };
insert(Value, {[{object, Key, Object}|Rest], Config}) ->
    {
        [{object, [
            Object,
            ?comma,
            indent_or_space(Config),
            Key,
            ?colon,
            space(Config),
            Value
        ]}] ++ Rest,
        Config
    };
insert(Value, {[{array, ?start_array}|Rest], Config}) ->
    {[{array, [?start_array, indent(Config), Value]}] ++ Rest, Config};
insert(Value, {[{array, Array}|Rest], Config}) ->
    {
        [{array, [Array,
            ?comma,
            indent_or_space(Config),
            Value
        ]}] ++ Rest,
        Config
    };
insert(_, _) -> erlang:error(badarg).


get_key({[{object, Key, _}|_], _}) -> Key;
get_key(_) -> erlang:error(badarg).


get_value({Value, _Config}) ->
    try unicode:characters_to_binary(Value)
    catch error:_ -> erlang:error(badarg)
    end;
get_value(_) -> erlang:error(badarg).



%% eunit tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


config_test_() ->
    [
        {"empty config", ?_assertEqual(#config{}, parse_config([]))},
        {"unspecified indent/space", ?_assertEqual(
            #config{space=1, indent=1},
            parse_config([space, indent])
        )},
        {"specific indent", ?_assertEqual(
            #config{indent=4},
            parse_config([{indent, 4}])
        )},
        {"specific space", ?_assertEqual(
            #config{space=2},
            parse_config([{space, 2}])
        )},
        {"specific space and indent", ?_assertEqual(
            #config{space=2, indent=2},
            parse_config([{space, 2}, {indent, 2}])
        )},
        {"invalid opt flag", ?_assertError(badarg, parse_config([error]))},
        {"invalid opt tuple", ?_assertError(badarg, parse_config([{error, true}]))}
    ].


space_test_() ->
    [
        {"no space", ?_assertEqual(<<>>, space(#config{space=0}))},
        {"one space", ?_assertEqual(<<" ">>, space(#config{space=1}))},
        {"four spaces", ?_assertEqual(<<"    ">>, space(#config{space=4}))}
    ].


indent_test_() ->
    [
        {"no indent", ?_assertEqual(<<>>, indent(#config{indent=0, depth=1}))},
        {"indent 1 depth 1", ?_assertEqual(
            <<?newline/binary, <<" ">>/binary>>,
            indent(#config{indent=1, depth=1})
        )},
        {"indent 1 depth 2", ?_assertEqual(
            <<?newline/binary, <<"  ">>/binary>>,
            indent(#config{indent=1, depth=2})
        )},
        {"indent 4 depth 1", ?_assertEqual(
            <<?newline/binary, <<"    ">>/binary>>,
            indent(#config{indent=4, depth=1})
        )},
        {"indent 4 depth 2", ?_assertEqual(
            <<?newline/binary, <<"    ">>/binary, <<"    ">>/binary>>,
            indent(#config{indent=4, depth=2})
        )}
    ].


indent_or_space_test_() ->
    [
        {"no indent so space", ?_assertEqual(
            <<" ">>,
            indent_or_space(#config{space=1, indent=0, depth=1})
        )},
        {"indent so no space", ?_assertEqual(
            <<?newline/binary, <<" ">>/binary>>,
            indent_or_space(#config{space=1, indent=1, depth=1})
        )}
    ].


encode_test_() ->
    [
        {"0.0", ?_assert(encode(float, 0.0, #config{}) =:= ["0.0"])},
        {"1.0", ?_assert(encode(float, 1.0, #config{}) =:= ["1.0"])},
        {"-1.0", ?_assert(encode(float, -1.0, #config{}) =:= ["-1.0"])},
        {"3.1234567890987654321", 
            ?_assert(
                encode(float, 3.1234567890987654321, #config{}) =:= ["3.1234567890987655"])
        },
        {"1.0e23", ?_assert(encode(float, 1.0e23, #config{}) =:= ["1.0e23"])},
        {"0.3", ?_assert(encode(float, 3.0/10.0, #config{}) =:= ["0.3"])},
        {"0.0001", ?_assert(encode(float, 0.0001, #config{}) =:= ["0.0001"])},
        {"0.00001", ?_assert(encode(float, 0.00001, #config{}) =:= ["1.0e-5"])},
        {"0.00000001", ?_assert(encode(float, 0.00000001, #config{}) =:= ["1.0e-8"])},
        {"1.0e-323", ?_assert(encode(float, 1.0e-323, #config{}) =:= ["1.0e-323"])},
        {"1.0e308", ?_assert(encode(float, 1.0e308, #config{}) =:= ["1.0e308"])},
        {"min normalized float", 
            ?_assert(
                encode(float, math:pow(2, -1022), #config{}) =:= ["2.2250738585072014e-308"]
            )
        },
        {"max normalized float", 
            ?_assert(
                encode(float, (2 - math:pow(2, -52)) * math:pow(2, 1023), #config{}) 
                    =:= ["1.7976931348623157e308"]
            )
        },
        {"min denormalized float", 
            ?_assert(encode(float, math:pow(2, -1074), #config{}) =:= ["5.0e-324"])
        },
        {"max denormalized float", 
            ?_assert(
                encode(float, (1 - math:pow(2, -52)) * math:pow(2, -1022), #config{}) 
                    =:= ["2.225073858507201e-308"]
            )
        },
        {"hello world", ?_assert(encode(string, <<"hello world">>, #config{})
            =:= [<<"\"">>, <<"hello world">>, <<"\"">>]
        )},
        {"key", ?_assert(encode(key, <<"key">>, #config{}) =:= [<<"\"">>, <<"key">>, <<"\"">>])},
        {"1", ?_assert(encode(integer, 1, #config{}) =:= "1")},
        {"-1", ?_assert(encode(integer, -1, #config{}) =:= "-1")},
        {"true", ?_assert(encode(literal, true, #config{}) =:= "true")},
        {"false", ?_assert(encode(literal, false, #config{}) =:= "false")},
        {"null", ?_assert(encode(literal, null, #config{}) =:= "null")}
    ].


format_test_() ->
    % {minified version, pretty version}
    Cases = [
        {"empty object", <<"{}">>, <<"{}">>},
        {"empty array", <<"[]">>, <<"[]">>},
        {"single key object", <<"{\"k\":\"v\"}">>, <<"{\n  \"k\": \"v\"\n}">>},
        {"single member array", <<"[true]">>, <<"[\n  true\n]">>},
        {"multiple key object",
            <<"{\"k\":\"v\",\"x\":\"y\"}">>,
            <<"{\n  \"k\": \"v\",\n  \"x\": \"y\"\n}">>
        },
        {"multiple member array",
            <<"[1.0,2.0,3.0]">>,
            <<"[\n  1.0,\n  2.0,\n  3.0\n]">>
        },
        {"nested structure",
            <<"[[{},[],true],{\"k\":\"v\",\"x\":\"y\"}]">>,
            <<"[\n  [\n    {},\n    [],\n    true\n  ],\n  {\n    \"k\": \"v\",\n    \"x\": \"y\"\n  }\n]">>
        }
    ],
    [{Title, ?_assertEqual(Min, jsx:minify(Pretty))} || {Title, Min, Pretty} <- Cases] ++
        [{Title, ?_assertEqual(Pretty, jsx:prettify(Min))} || {Title, Min, Pretty} <- Cases].


handle_event_test_() ->
    Data = jsx:test_cases() ++ jsx:special_test_cases(),
    [
        {
            Title, ?_assertEqual(
                JSON,
                lists:foldl(fun handle_event/2, init([]), Events ++ [end_json])
            )
        } || {Title, JSON, _, Events} <- Data
    ].


-endif.
