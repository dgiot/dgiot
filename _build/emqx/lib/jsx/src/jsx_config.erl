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


-module(jsx_config).

-export([parse_config/1]).
-export([config_to_list/1]).
-export([extract_config/1, valid_flags/0]).

-ifdef(TEST).
-export([fake_error_handler/3]).
-endif.

-include("jsx_config.hrl").

-type handler_type(Handler) ::
    fun((jsx:json_text() | end_stream |
         jsx:json_term(),
         {decoder, any(), module(), null | list(), list()} |
         {parser, any(), module(), list()} |
         {encoder, any(), module()},
         list({pre_encode, fun((any()) -> any())} |
              {error_handler, Handler} |
              {incomplete_handler, Handler} |
              atom())) -> any()).
-type handler() :: handler_type(handler()).
-export_type([handler/0]).

-type config() :: #config{}.
-export_type([config/0]).

%% parsing of jsx config
-spec parse_config(Config::proplists:proplist()) -> config().

parse_config(Config) -> parse_config(Config, #config{}).

parse_config([], Config) -> Config;
parse_config([escaped_forward_slashes|Rest], Config) ->
    parse_config(Rest, Config#config{escaped_forward_slashes=true});
parse_config([escaped_strings|Rest], Config) ->
    parse_config(Rest, Config#config{escaped_strings=true});
parse_config([unescaped_jsonp|Rest], Config) ->
    parse_config(Rest, Config#config{unescaped_jsonp=true});
parse_config([dirty_strings|Rest], Config) ->
    parse_config(Rest, Config#config{dirty_strings=true});
parse_config([multi_term|Rest], Config) ->
    parse_config(Rest, Config#config{multi_term=true});
parse_config([return_tail|Rest], Config) ->
    parse_config(Rest, Config#config{return_tail=true});
%% retained for backwards compat, now does nothing however
parse_config([repeat_keys|Rest], Config) ->
    parse_config(Rest, Config);
parse_config([uescape|Rest], Config) ->
    parse_config(Rest, Config#config{uescape=true});
parse_config([strict|Rest], Config) ->
    parse_config(Rest, Config#config{
        strict_comments=true,
        strict_commas=true,
        strict_utf8=true,
        strict_single_quotes=true,
        strict_escapes=true,
        strict_control_codes=true
    });
parse_config([{strict, Strict}|Rest], Config) ->
    parse_strict(Strict, Rest, Config);
parse_config([stream|Rest], Config) ->
    parse_config(Rest, Config#config{stream=true});
parse_config([{error_handler, ErrorHandler}|Rest] = Options, Config) when is_function(ErrorHandler, 3) ->
    case Config#config.error_handler of
        false -> parse_config(Rest, Config#config{error_handler=ErrorHandler})
        ; _ -> erlang:error(badarg, [Options, Config])
    end;
parse_config([{incomplete_handler, IncompleteHandler}|Rest] = Options, Config) when is_function(IncompleteHandler, 3) ->
    case Config#config.incomplete_handler of
        false -> parse_config(Rest, Config#config{incomplete_handler=IncompleteHandler})
        ; _ -> erlang:error(badarg, [Options, Config])
    end;
parse_config(_Options, _Config) -> erlang:error(badarg).


parse_strict([], Rest, Config) -> parse_config(Rest, Config);
parse_strict([comments|Strict], Rest, Config) ->
    parse_strict(Strict, Rest, Config#config{strict_comments=true});
parse_strict([trailing_commas|Strict], Rest, Config) ->
    parse_strict(Strict, Rest, Config#config{strict_commas=true});
parse_strict([utf8|Strict], Rest, Config) ->
    parse_strict(Strict, Rest, Config#config{strict_utf8=true});
parse_strict([single_quotes|Strict], Rest, Config) ->
    parse_strict(Strict, Rest, Config#config{strict_single_quotes=true});
parse_strict([escapes|Strict], Rest, Config) ->
    parse_strict(Strict, Rest, Config#config{strict_escapes=true});
parse_strict([control_codes|Strict], Rest, Config) ->
    parse_strict(Strict, Rest, Config#config{strict_control_codes=true});
parse_strict(_Strict, _Rest, _Config) ->
    erlang:error(badarg).



-spec config_to_list(Config::config()) -> proplists:proplist().

config_to_list(Config) ->
    reduce_config(lists:map(
        fun ({error_handler, F}) -> {error_handler, F};
            ({incomplete_handler, F}) -> {incomplete_handler, F};
            ({Key, true}) -> Key
        end,
        lists:filter(
            fun({_, false}) -> false; (_) -> true end,
            lists:zip(record_info(fields, config), tl(tuple_to_list(Config)))
        )
    )).


reduce_config(Input) -> reduce_config(Input, [], []).

reduce_config([], Output, Strict) ->
    case length(Strict) of
        0 -> lists:reverse(Output);
        5 -> lists:reverse(Output) ++ [strict];
        _ -> lists:reverse(Output) ++ [{strict, lists:reverse(Strict)}]
    end;
reduce_config([strict_comments|Input], Output, Strict) ->
    reduce_config(Input, Output, [comments] ++ Strict);
reduce_config([strict_utf8|Input], Output, Strict) ->
    reduce_config(Input, Output, [utf8] ++ Strict);
reduce_config([strict_single_quotes|Input], Output, Strict) ->
    reduce_config(Input, Output, [single_quotes] ++ Strict);
reduce_config([strict_escapes|Input], Output, Strict) ->
    reduce_config(Input, Output, [escapes] ++ Strict);
reduce_config([strict_control_codes|Input], Output, Strict) ->
    reduce_config(Input, Output, [control_codes] ++ Strict);
reduce_config([Else|Input], Output, Strict) ->
    reduce_config(Input, [Else] ++ Output, Strict).


-spec valid_flags() -> [atom()].

valid_flags() ->
    [
        escaped_forward_slashes,
        escaped_strings,
        unescaped_jsonp,
        dirty_strings,
        multi_term,
        return_tail,
        repeat_keys,
        strict,
        stream,
        uescape,
        error_handler,
        incomplete_handler
    ].


-spec extract_config(Config::proplists:proplist()) -> proplists:proplist().

extract_config(Config) ->
    extract_parser_config(Config, []).

extract_parser_config([], Acc) -> Acc;
extract_parser_config([{K,V}|Rest], Acc) ->
    case lists:member(K, valid_flags()) of
        true -> extract_parser_config(Rest, [{K,V}] ++ Acc)
        ; false -> extract_parser_config(Rest, Acc)
    end;
extract_parser_config([K|Rest], Acc) ->
    case lists:member(K, valid_flags()) of
        true -> extract_parser_config(Rest, [K] ++ Acc)
        ; false -> extract_parser_config(Rest, Acc)
    end.


%% eunit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


config_test_() ->
    [
        {"all flags",
            ?_assertEqual(
                #config{escaped_forward_slashes = true,
                    escaped_strings = true,
                    unescaped_jsonp = true,
                    dirty_strings = true,
                    multi_term = true,
                    return_tail = true,
                    strict_comments = true,
                    strict_commas = true,
                    strict_utf8 = true,
                    strict_single_quotes = true,
                    strict_escapes = true,
                    strict_control_codes = true,
                    stream = true,
                    uescape = true
                },
                parse_config([dirty_strings,
                    escaped_forward_slashes,
                    escaped_strings,
                    unescaped_jsonp,
                    multi_term,
                    return_tail,
                    repeat_keys,
                    strict,
                    stream,
                    uescape
                ])
            )
        },
        {"strict flag",
            ?_assertEqual(
                #config{strict_comments = true,
                    strict_commas = true,
                    strict_utf8 = true,
                    strict_single_quotes = true,
                    strict_escapes = true,
                    strict_control_codes = true
                },
                parse_config([strict])
            )
        },
        {"strict selective",
            ?_assertEqual(
                #config{strict_comments = true},
                parse_config([{strict, [comments]}])
            )
        },
        {"strict expanded",
            ?_assertEqual(
                #config{strict_comments = true,
                    strict_utf8 = true,
                    strict_single_quotes = true,
                    strict_escapes = true
                },
                parse_config([{strict, [comments, utf8, single_quotes, escapes]}])
            )
        },
        {"error_handler flag", ?_assertEqual(
            #config{error_handler=fun ?MODULE:fake_error_handler/3},
            parse_config([{error_handler, fun ?MODULE:fake_error_handler/3}])
        )},
        {"two error_handlers defined", ?_assertError(
            badarg,
            parse_config([
                {error_handler, fun(_, _, _) -> true end},
                {error_handler, fun(_, _, _) -> false end}
            ])
        )},
        {"incomplete_handler flag", ?_assertEqual(
            #config{incomplete_handler=fun ?MODULE:fake_error_handler/3},
            parse_config([{incomplete_handler, fun ?MODULE:fake_error_handler/3}])
        )},
        {"two incomplete_handlers defined", ?_assertError(
            badarg,
            parse_config([
                {incomplete_handler, fun(_, _, _) -> true end},
                {incomplete_handler, fun(_, _, _) -> false end}
            ])
        )},
        {"bad option flag", ?_assertError(badarg, parse_config([this_flag_does_not_exist]))}
    ].


config_to_list_test_() ->
    [
        {"empty config", ?_assertEqual(
            [],
            config_to_list(#config{})
        )},
        {"all flags", ?_assertEqual(
            [dirty_strings,
                escaped_forward_slashes,
                escaped_strings,
                multi_term,
                stream,
                uescape,
                unescaped_jsonp,
                strict
            ],
            config_to_list(
                #config{escaped_forward_slashes = true,
                    escaped_strings = true,
                    unescaped_jsonp = true,
                    dirty_strings = true,
                    multi_term = true,
                    strict_comments = true,
                    strict_utf8 = true,
                    strict_single_quotes = true,
                    strict_escapes = true,
                    strict_control_codes = true,
                    stream = true,
                    uescape = true
                }
            )
        )},
        {"single strict", ?_assertEqual(
            [{strict, [comments]}],
            config_to_list(#config{strict_comments = true})
        )},
        {"multiple strict", ?_assertEqual(
            [{strict, [utf8, single_quotes, escapes]}],
            config_to_list(#config{strict_utf8 = true, strict_single_quotes = true, strict_escapes = true})
        )},
        {"all strict", ?_assertEqual(
            [strict],
            config_to_list(#config{strict_comments = true,
                strict_utf8 = true,
                strict_single_quotes = true,
                strict_escapes = true,
                strict_control_codes = true})
        )},
        {"error handler", ?_assertEqual(
            [{error_handler, fun ?MODULE:fake_error_handler/3}],
            config_to_list(#config{error_handler=fun ?MODULE:fake_error_handler/3})
        )},
        {"incomplete handler", ?_assertEqual(
            [{incomplete_handler, fun ?MODULE:fake_error_handler/3}],
            config_to_list(#config{incomplete_handler=fun ?MODULE:fake_error_handler/3})
        )}
    ].


fake_error_handler(_, _, _) -> ok.


-endif.
