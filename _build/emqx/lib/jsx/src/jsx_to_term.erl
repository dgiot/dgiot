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


-module(jsx_to_term).

-export([to_term/2]).
-export([init/1, handle_event/2]).
-export([
    start_term/1,
    start_object/1,
    start_array/1,
    finish/1,
    insert/2,
    get_key/1,
    get_value/1
]).


-record(config, {
    labels = binary,
    return_maps = false
}).

-type config() :: list().
-export_type([config/0]).

-ifndef(maps_support).
-type json_value() :: list(json_value())
    | list({binary() | atom(), json_value()}) | [{},...]
    | true
    | false
    | null
    | integer()
    | float()
    | binary().
-endif.

-ifdef(maps_support).
-type json_value() :: list(json_value())
    | list({binary() | atom(), json_value()}) | [{},...]
    | map()
    | true
    | false
    | null
    | integer()
    | float()
    | binary().
-endif.


-spec to_term(Source::binary(), Config::config()) -> json_value().

-ifdef(maps_always).
to_term(Source, Config) when is_list(Config) ->
    (jsx:decoder(?MODULE, [return_maps] ++ Config, jsx_config:extract_config(Config)))(Source).
-endif.
-ifndef(maps_always).
to_term(Source, Config) when is_list(Config) ->
    (jsx:decoder(?MODULE, Config, jsx_config:extract_config(Config)))(Source).
-endif.

parse_config(Config) -> parse_config(Config, #config{}).

parse_config([{labels, Val}|Rest], Config)
        when Val == binary; Val == atom; Val == existing_atom; Val == attempt_atom ->
    parse_config(Rest, Config#config{labels = Val});
parse_config([labels|Rest], Config) ->
    parse_config(Rest, Config#config{labels = binary});
parse_config([{return_maps, Val}|Rest], Config)
        when Val == true; Val == false ->
    parse_config(Rest, Config#config{return_maps = Val});
parse_config([return_maps|Rest], Config) ->
    parse_config(Rest, Config#config{return_maps = true});
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


-type state() :: {list(), #config{}}.
-spec init(Config::proplists:proplist()) -> state().

init(Config) -> start_term(Config).

-spec handle_event(Event::any(), State::state()) -> state().

handle_event(end_json, State) -> get_value(State);

handle_event(start_object, State) -> start_object(State);
handle_event(end_object, State) -> finish(State);

handle_event(start_array, State) -> start_array(State);
handle_event(end_array, State) -> finish(State);

handle_event({key, Key}, {_, Config} = State) -> insert(format_key(Key, Config), State);

handle_event({_, Event}, State) -> insert(Event, State).


format_key(Key, Config) ->
    case Config#config.labels of
        binary -> Key
        ; atom -> binary_to_atom(Key, utf8)
        ; existing_atom -> binary_to_existing_atom(Key, utf8)
        ; attempt_atom ->
            try binary_to_existing_atom(Key, utf8) of
                Result -> Result
            catch
                error:badarg -> Key
            end
    end.


%% internal state is a stack and a config object
%%  `{Stack, Config}`
%% the stack is a list of in progress objects/arrays
%%  `[Current, Parent, Grandparent,...OriginalAncestor]`
%% an object has the representation on the stack of
%%  `{object, [
%%    {NthKey, NthValue},
%%    {NMinus1Key, NthMinus1Value},
%%    ...,
%%    {FirstKey, FirstValue}
%%  ]}`
%% or if returning maps
%%  `{object, #{
%%    FirstKey => FirstValue,
%%    SecondKey => SecondValue,
%%    ...,
%%    NthKey => NthValue
%%  }}`
%% or if there's a key with a yet to be matched value
%%  `{object, Key, ...}`
%% an array looks like
%%  `{array, [NthValue, NthMinus1Value,...FirstValue]}`

start_term(Config) when is_list(Config) -> {[], parse_config(Config)}.


-ifndef(maps_support).
%% allocate a new object on top of the stack
start_object({Stack, Config}) -> {[{object, []}] ++ Stack, Config}.


%% allocate a new array on top of the stack
start_array({Stack, Config}) -> {[{array, []}] ++ Stack, Config}.


%% finish an object or array and insert it into the parent object if it exists or
%% return it if it is the root object
finish({[{object, []}], Config}) -> {[{}], Config};
finish({[{object, []}|Rest], Config}) -> insert([{}], {Rest, Config});
finish({[{object, Pairs}], Config}) -> {lists:reverse(Pairs), Config};
finish({[{object, Pairs}|Rest], Config}) -> insert(lists:reverse(Pairs), {Rest, Config});
finish({[{array, Values}], Config}) -> {lists:reverse(Values), Config};
finish({[{array, Values}|Rest], Config}) -> insert(lists:reverse(Values), {Rest, Config});
finish(_) -> erlang:error(badarg).


%% insert a value when there's no parent object or array
insert(Value, {[], Config}) -> {Value, Config};
%% insert a key or value into an object or array, autodetects the 'right' thing
insert(Key, {[{object, Pairs}|Rest], Config}) ->
    {[{object, Key, Pairs}] ++ Rest, Config};
insert(Value, {[{object, Key, Pairs}|Rest], Config}) ->
    {[{object, [{Key, Value}] ++ Pairs}] ++ Rest, Config};
insert(Value, {[{array, Values}|Rest], Config}) ->
    {[{array, [Value] ++ Values}] ++ Rest, Config};
insert(_, _) -> erlang:error(badarg).
-endif.


-ifdef(maps_support).
%% allocate a new object on top of the stack
start_object({Stack, Config=#config{return_maps=true}}) ->
    {[{object, #{}}] ++ Stack, Config};
start_object({Stack, Config}) ->
    {[{object, []}] ++ Stack, Config}.


%% allocate a new array on top of the stack
start_array({Stack, Config}) -> {[{array, []}] ++ Stack, Config}.


%% finish an object or array and insert it into the parent object if it exists or
%% return it if it is the root object
finish({[{object, Map}], Config=#config{return_maps=true}}) -> {Map, Config};
finish({[{object, Map}|Rest], Config=#config{return_maps=true}}) -> insert(Map, {Rest, Config});
finish({[{object, []}], Config}) -> {[{}], Config};
finish({[{object, []}|Rest], Config}) -> insert([{}], {Rest, Config});
finish({[{object, Pairs}], Config}) -> {lists:reverse(Pairs), Config};
finish({[{object, Pairs}|Rest], Config}) -> insert(lists:reverse(Pairs), {Rest, Config});
finish({[{array, Values}], Config}) -> {lists:reverse(Values), Config};
finish({[{array, Values}|Rest], Config}) -> insert(lists:reverse(Values), {Rest, Config});
finish(_) -> erlang:error(badarg).


%% insert a value when there's no parent object or array
insert(Value, {[], Config}) -> {Value, Config};
%% insert a key or value into an object or array, autodetects the 'right' thing
insert(Key, {[{object, Map}|Rest], Config=#config{return_maps=true}}) ->
    {[{object, Key, Map}] ++ Rest, Config};
insert(Key, {[{object, Pairs}|Rest], Config}) ->
    {[{object, Key, Pairs}] ++ Rest, Config};
insert(Value, {[{object, Key, Map}|Rest], Config=#config{return_maps=true}}) ->
    {[{object, maps:put(Key, Value, Map)}] ++ Rest, Config};
insert(Value, {[{object, Key, Pairs}|Rest], Config}) ->
    {[{object, [{Key, Value}] ++ Pairs}] ++ Rest, Config};
insert(Value, {[{array, Values}|Rest], Config}) ->
    {[{array, [Value] ++ Values}] ++ Rest, Config};
insert(_, _) -> erlang:error(badarg).
-endif.


get_key({[{object, Key, _}|_], _}) -> Key;
get_key(_) -> erlang:error(badarg).


get_value({Value, _Config}) -> Value;
get_value(_) -> erlang:error(badarg).



%% eunit tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


config_test_() ->
    [
        {"empty config", ?_assertEqual(#config{}, parse_config([]))},
        {"implicit binary labels", ?_assertEqual(#config{}, parse_config([labels]))},
        {"binary labels", ?_assertEqual(#config{}, parse_config([{labels, binary}]))},
        {"atom labels", ?_assertEqual(#config{labels=atom}, parse_config([{labels, atom}]))},
        {"existing atom labels", ?_assertEqual(
            #config{labels=existing_atom},
            parse_config([{labels, existing_atom}])
        )},
        {"return_maps true", ?_assertEqual(
            #config{return_maps=true},
            parse_config([return_maps])
        )},
        {"invalid opt flag", ?_assertError(badarg, parse_config([error]))},
        {"invalid opt tuple", ?_assertError(badarg, parse_config([{error, true}]))}
    ].


format_key_test_() ->
    [
        {"binary key", ?_assertEqual(<<"key">>, format_key(<<"key">>, #config{labels=binary}))},
        {"atom key", ?_assertEqual(key, format_key(<<"key">>, #config{labels=atom}))},
        {"existing atom key", ?_assertEqual(
            key,
            format_key(<<"key">>, #config{labels=existing_atom})
        )},
        {"nonexisting atom key", ?_assertError(
            badarg,
            format_key(<<"nonexistentatom">>, #config{labels=existing_atom})
        )},
        {"sloppy existing atom key", ?_assertEqual(
            key,
            format_key(<<"key">>, #config{labels=attempt_atom})
        )},
        {"nonexisting atom key", ?_assertEqual(
            <<"nonexistentatom">>,
            format_key(<<"nonexistentatom">>, #config{labels=attempt_atom})
        )}
    ].


rep_manipulation_test_() ->
    [
        {"allocate a new context with option", ?_assertEqual(
            {[], #config{labels=atom}},
            start_term([{labels, atom}])
        )},
        {"allocate a new object on an empty stack", ?_assertEqual(
            {[{object, []}], #config{}},
            start_object({[], #config{}})
        )},
        {"allocate a new object on a stack", ?_assertEqual(
            {[{object, []}, {object, []}], #config{}},
            start_object({[{object, []}], #config{}})
        )},
        {"allocate a new array on an empty stack", ?_assertEqual(
            {[{array, []}], #config{}},
            start_array({[], #config{}})
        )},
        {"allocate a new array on a stack", ?_assertEqual(
            {[{array, []}, {object, []}], #config{}},
            start_array({[{object, []}], #config{}})
        )},
        {"insert a key into an object", ?_assertEqual(
            {[{object, key, []}, junk], #config{}},
            insert(key, {[{object, []}, junk], #config{}})
        )},
        {"get current key", ?_assertEqual(
            key,
            get_key({[{object, key, []}], #config{}})
        )},
        {"try to get non-key from object", ?_assertError(
            badarg,
            get_key({[{object, []}], #config{}})
        )},
        {"try to get key from array", ?_assertError(
            badarg,
            get_key({[{array, []}], #config{}})
        )},
        {"insert a value into an object", ?_assertEqual(
            {[{object, [{key, value}]}, junk], #config{}},
            insert(value, {[{object, key, []}, junk], #config{}})
        )},
        {"insert a value into an array", ?_assertEqual(
            {[{array, [value]}, junk], #config{}},
            insert(value, {[{array, []}, junk], #config{}})
        )},
        {"finish an object with no ancestor", ?_assertEqual(
            {[{a, b}, {x, y}], #config{}},
            finish({[{object, [{x, y}, {a, b}]}], #config{}})
        )},
        {"finish an empty object", ?_assertEqual(
            {[{}], #config{}},
            finish({[{object, []}], #config{}})
        )},
        {"finish an object with an ancestor", ?_assertEqual(
            {[{object, [{key, [{a, b}, {x, y}]}, {foo, bar}]}], #config{}},
            finish({[{object, [{x, y}, {a, b}]}, {object, key, [{foo, bar}]}], #config{}})
        )},
        {"finish an array with no ancestor", ?_assertEqual(
            {[a, b, c], #config{}},
            finish({[{array, [c, b, a]}], #config{}})
        )},
        {"finish an array with an ancestor", ?_assertEqual(
            {[{array, [[a, b, c], d, e, f]}], #config{}},
            finish({[{array, [c, b, a]}, {array, [d, e, f]}], #config{}})
        )}
    ].


-ifdef(maps_support).
rep_manipulation_with_maps_test_() ->
    [
        {"allocate a new object on an empty stack", ?_assertEqual(
            {[{object, #{}}], #config{return_maps=true}},
            start_object({[], #config{return_maps=true}})
        )},
        {"allocate a new object on a stack", ?_assertEqual(
            {[{object, #{}}, {object, #{}}], #config{return_maps=true}},
            start_object({[{object, #{}}], #config{return_maps=true}})
        )},
        {"insert a key into an object", ?_assertEqual(
            {[{object, key, #{}}, junk], #config{return_maps=true}},
            insert(key, {[{object, #{}}, junk], #config{return_maps=true}})
        )},
        {"get current key", ?_assertEqual(
            key,
            get_key({[{object, key, #{}}], #config{return_maps=true}})
        )},
        {"try to get non-key from object", ?_assertError(
            badarg,
            get_key({[{object, #{}}], #config{return_maps=true}})
        )},
        {"insert a value into an object", ?_assertEqual(
            {[{object, #{key => value}}, junk], #config{return_maps=true}},
            insert(value, {[{object, key, #{}}, junk], #config{return_maps=true}})
        )},
        {"finish an object with no ancestor", ?_assertEqual(
            {#{a => b, x => y}, #config{return_maps=true}},
            finish({[{object, #{x => y, a => b}}], #config{return_maps=true}})
        )},
        {"finish an empty object", ?_assertEqual(
            {#{}, #config{return_maps=true}},
            finish({[{object, #{}}], #config{return_maps=true}})
        )},
        {"finish an object with an ancestor", ?_assertEqual(
            {
                [{object, #{key => #{a => b, x => y}, foo => bar}}],
                #config{return_maps=true}
            },
            finish({
                [{object, #{x => y, a => b}}, {object, key, #{foo => bar}}],
                #config{return_maps=true}
            })
        )}
    ].


return_maps_test_() ->
    [
        {"an empty map", ?_assertEqual(
            #{},
            jsx:decode(<<"{}">>, [return_maps])
        )},
        {"an empty map", ?_assertEqual(
            [{}],
            jsx:decode(<<"{}">>, [])
        )},
        {"an empty map", ?_assertEqual(
            [{}],
            jsx:decode(<<"{}">>, [{return_maps, false}])
        )},
        {"a small map", ?_assertEqual(
            #{<<"awesome">> => true, <<"library">> => <<"jsx">>},
            jsx:decode(<<"{\"library\": \"jsx\", \"awesome\": true}">>, [return_maps])
        )},
        {"a recursive map", ?_assertEqual(
            #{<<"key">> => #{<<"key">> => true}},
            jsx:decode(<<"{\"key\": {\"key\": true}}">>, [return_maps])
        )},
        {"a map inside a list", ?_assertEqual(
            [#{}],
            jsx:decode(<<"[{}]">>, [return_maps])
        )}
    ].
-endif.


handle_event_test_() ->
    Data = jsx:test_cases(),
    [
        {
            Title, ?_assertEqual(
                Term,
                lists:foldl(fun handle_event/2, init([]), Events ++ [end_json])
            )
        } || {Title, _, Term, Events} <- Data
    ].


-endif.
