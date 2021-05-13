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


-module(jsx_verify).

-export([is_json/2, is_term/2]).
-export([init/1, handle_event/2]).


-spec is_json(Source::binary(), Config::jsx_config:config()) -> true | false | {incomplete, jsx:decoder()}.

is_json(Source, Config) when is_list(Config) ->
    try (jsx:decoder(?MODULE, Config, jsx_config:extract_config(Config)))(Source)
    catch error:badarg -> false
    end.


-spec is_term(Source::any(), Config::jsx_config:config()) -> true | false | {incomplete, jsx:encoder()}.

is_term(Source, Config) when is_list(Config) ->
    try (jsx:encoder(?MODULE, Config, jsx_config:extract_config(Config)))(Source)
    catch error:badarg -> false
    end.


parse_config(Config) -> parse_config(Config, []).

%% ignore deprecated flags
parse_config([no_repeated_keys|Rest], Config) ->
    parse_config(Rest, Config);
parse_config([{repeated_keys, Val}|Rest], Config) when Val == true; Val == false ->
    parse_config(Rest, Config);
parse_config([repeated_keys|Rest], Config) ->
    parse_config(Rest, Config);
parse_config([{K, _}|Rest] = Options, Config) ->
    case lists:member(K, jsx_config:valid_flags()) of
        true -> parse_config(Rest, Config);
        false -> erlang:error(badarg, [Options, Config])
    end;
parse_config([K|Rest] = Options, Config) ->
    case lists:member(K, jsx_config:valid_flags()) of
        true -> parse_config(Rest, Config);
        false -> erlang:error(badarg, [Options, Config])
    end;
parse_config([], Config) ->
    Config.


%% we don't actually need any state for this
-type state() :: [].
-spec init(Config::proplists:proplist()) -> state().

init(Config) -> parse_config(Config).


-spec handle_event(Event::any(), State::state()) -> state().

handle_event(end_json, _) -> true;

handle_event(_, State) -> State.



%% eunit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


config_test_() ->
    [
        {"empty config", ?_assertEqual([], parse_config([]))},
        {"no repeat keys", ?_assertEqual([], parse_config([no_repeated_keys]))},
        {"bare repeated keys", ?_assertEqual([], parse_config([repeated_keys]))},
        {"repeated keys true", ?_assertEqual(
            [],
            parse_config([{repeated_keys, true}])
        )},
        {"repeated keys false", ?_assertEqual(
            [],
            parse_config([{repeated_keys, false}])
        )},
        {"invalid opt flag", ?_assertError(badarg, parse_config([error]))},
        {"invalid opt tuple", ?_assertError(badarg, parse_config([{error, true}]))}
    ].


handle_event_test_() ->
    Data = jsx:test_cases() ++ jsx:special_test_cases(),
    [
        {
            Title, ?_assertEqual(
                true,
                lists:foldl(fun handle_event/2, [], Events ++ [end_json])
            )
        } || {Title, _, _, Events} <- Data
    ].


-endif.
