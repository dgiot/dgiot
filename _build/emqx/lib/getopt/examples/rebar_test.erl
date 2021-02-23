%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2009 Juan Jose Comellas
%%% @doc Example file for the getopt module.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(rebar_test).
-author('juanjo@comellas.org').

-export([test/0, test/1, test_required/0, test_check/0, usage/0]).

test_required() ->
    test2(fun getopt:parse_and_check/2, "-f").

test_check() ->
    test2(fun(OptSpecList, CmdLine) ->
        case getopt:parse(OptSpecList, CmdLine) of
            {ok, {Opts, _}} -> getopt:check(OptSpecList, Opts);
            Other           -> Other
        end
    end, "-f").

test() ->
    test2(fun getopt:parse/2, "-f verbose=1 --quiet=on -j2 dummy1 dummy2").


test(CmdLine) ->
    test2(fun getopt:parse/2, CmdLine).

test2(Fun, CmdLine) ->
    OptSpecList = option_spec_list(),

    io:format("For command line: ~p~n"
              "getopt:parse/2 returns:~n~n", [CmdLine]),
    case Fun(OptSpecList, CmdLine) of
        {ok, {Options, NonOptArgs}} ->
            io:format("Options:~n  ~p~n~nNon-option arguments:~n  ~p~n", [Options, NonOptArgs]);
        {error, {_Reason, _Data}} = Error ->
            io:format("Error: ~s~n~n", [getopt:format_error(OptSpecList, Error)]),
            usage(OptSpecList)
    end.


usage() ->
    usage(option_spec_list()).

usage(OptSpecList) ->
    getopt:usage(OptSpecList, "rebar_test", "[var1=val1 ...] [command1 ...]",
                 [{"var=value", "Variables that will affect the compilation (e.g. debug=1)"},
                  {"command",   "Commands that will be executed by rebar (e.g. compile)"}]).

option_spec_list() ->
    CpuCount = erlang:system_info(logical_processors),
    [
     %% {Name,     ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
     {help,        $h,        "help",        undefined,             "Show the program options"},
     {jobs,        $j,        "jobs",        {integer, CpuCount},   "Number of concurrent jobs"},
     {verbose,     $v,        "verbose",     boolean,               "Be verbose about what gets done"},
     {force,       $f,        "force",       {boolean, false},      "Force"}
    ].
