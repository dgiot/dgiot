#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname ex1 -pz ebin

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
-module(ex1).
-author('juanjo@comellas.org').

main([]) ->
    getopt:usage(option_spec_list(), escript:script_name(), "<arg1> <arg2> <arg3> <arg4> <arg5> <arg6> <arg7> <arg8> <arg9> <arg10>");
main(Args) ->
    OptSpecList = option_spec_list(),
    io:format("For command line: ~p~n"
              "getopt:parse/2 returns:~n~n", [Args]),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, NonOptArgs}} ->
            io:format("Options:~n  ~p~n~nNon-option arguments:~n  ~p~n", [Options, NonOptArgs]);
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            getopt:usage(OptSpecList, "ex1.escript")
    end.


option_spec_list() ->
    CurrentUser = case os:getenv("USER") of
                      false ->
                          "user";
                      User ->
                          User
                  end,
    LongHelp = "Password to connect to the database; this is an arbitrarily long string just to check "
               "whether getopt wraps lines correctly when they become too long for the console. We'll "
               "make it extra long so that it takes at least 3 lines to show on normal consoles. That "
               "wasn't enough, so let's try again by adding more and more redundant text to this help "
               "line. What if we copied a paragraph from a book? That would surely make it easier",
    [
     %% {Name,     ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
     {help,        $?,        "help",        undefined,             "Show the program options"},
     {username,    $U,        "username",    {string, CurrentUser}, "Username to connect to the database"},
     {password,    $P,        "password",    string,                LongHelp},
     {host,        $h,        "host",        {string, "localhost"}, "Database server host name or IP address"},
     {port,        $p,        "port",        {integer, 1000},       "Database server port"},
     {output_file, $o,        "output-file", string,                "File where the data will be saved to"},
     {margin,      $m,        "margin",      {float, 0.66},         "Page margin"},
     {xml,         $x,        "xml",         undefined,             "Output data as XML"},
     {verbose,     $v,        "verbose",     integer,               "Verbosity level"},
     {dbname,      undefined, undefined,     string,                "Database name"}
    ].
