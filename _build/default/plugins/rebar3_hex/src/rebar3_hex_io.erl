-module(rebar3_hex_io).

-export([
         get_password/1,
         say/1,
         say/2,
         ask/2,
         ask/3,
         select_apps/1,
         str_split/2
        ]).



-ifdef(POST_OTP_19).
str_split(Str, Pattern) ->
    string:split(Str, Pattern).
-else.
str_split(Str, Pattern) ->
    Bin = unicode:characters_to_binary(Str),
    Bpat = unicode:characters_to_binary(Pattern),
    Blist = binary:split(Bin, Bpat),
    lists:map(fun(B) -> unicode:characters_to_list(B) end, Blist).
-endif.
-include("rebar3_hex.hrl").

-define(OP_PUTC, 0).

select_apps([App]) ->
    [App];
select_apps(Apps) ->
    io:format("Select application(s):", []),
    lists:foldl(fun(App, Idx) ->
                        io:format("~p) ~s", [Idx, rebar_app_info:name(App)]),
                        Idx+1
                end, 1, Apps),
    io:format("------------", []),
    io:format("A) All", []),
    case ask(io_lib:format("[1-~p] or (A)ll ", [length(Apps)]), string, "A") of
        "A" ->
            Apps;
        Index ->
            [lists:nth(list_to_integer(Index), Apps)]
    end.

get_password(Prompt) ->
    ok = io:setopts([binary]),
    Overwriter = fun() ->
        prompt_password(Prompt),
        receive
            {done, _Pid, _Ref} ->
                ok
        end
    end,
    Pid = spawn_link(Overwriter),
    PwLine = try
        io:get_line(Prompt)
    after
        Ref = make_ref(),
        Pid ! {done, self(), Ref},
        receive
            {done, Pid, Ref} ->
                ok
        after
            timer:seconds(5) ->
                throw(?PRV_ERROR(win32_prompt_timeout))
        end
    end,
    [Pw | _] = binary:split(PwLine, <<"\n">>),
    Pw.

prompt_password(Prompt) ->
    % This is spawned to continually overwrite the prompt the user is
    % entering data on, in order to hide characters typed.
    ClearLine = "\e[2K",
    receive
        {done, Parent, Ref} ->
            Parent ! {done, self(), Ref},
            Spaces = lists:duplicate(byte_size(Prompt) + 24, $ ),
            io:fwrite(standard_error, "~ts\r~ts\r", [ClearLine, Spaces])
    after
        1 ->
            Spaces = lists:duplicate(24, $ ),
            io:fwrite(standard_error, "~ts\r~ts~ts\r~ts", [ClearLine, Prompt, Spaces, Prompt]),
            prompt_password(Prompt)
    end.

ask(Prompt, Type) ->
    ask_convert(Prompt, fun get/2, Type, none).

ask(Prompt, Type, Default)  ->
    ask_convert(Prompt, fun get/2, Type, Default).

ask_convert(Prompt, TransFun, Type,  Default) ->
    DefaultPrompt = erlang:iolist_to_binary([Prompt, default(Default), "> "]),
    NewPrompt = erlang:binary_to_list(DefaultPrompt),
    Data = trim(trim(io:get_line(NewPrompt)), both, [$\n]),
    case TransFun(Type, Data)  of
        no_data ->
            maybe_continue(Prompt, TransFun, Type, Default);
        no_clue ->
            continue(Prompt, TransFun, Type, Default);
        Ret ->
            Ret
    end.

maybe_continue(Prompt, TransFun, Type, Default) ->
        case Default of
                none ->
                    continue(Prompt, TransFun, Type, Default);
                Default ->
                    TransFun(Type, Default)
            end.

continue(Prompt, TransFun, Type, Default) ->
            say("I didn't get that. This ~p kind of question.~n", [Type]),
            ask_convert(Prompt, TransFun, Type, Default).

default(none) ->
    [];
default(Default) ->
    [" (", io_lib:format("~p", [Default]) , ")"].

get(boolean, []) ->
    no_data;
get(boolean, [In | _]) when In =:= $Y orelse In =:= $y ->
    true;
get(boolean, [In | _]) when In =:= $N orelse In =:= $n ->
    false;
get(boolean, _) ->
    no_clue;
get(integer, []) ->
    no_data;
get(number, String) ->
    get(integer, String);
get(integer, String) ->
    case (catch list_to_integer(String)) of
        {'Exit', _} ->
            no_clue;
        Integer ->
            Integer
    end;
get(string, []) ->
    no_data;
get(string, String) ->
    case is_list(String) of
        true ->
            String;
        false ->
            no_clue
    end.

-ifdef(unicode_str).
trim(Str, right, Chars) -> string:trim(Str, trailing, Chars);
trim(Str, left, Chars) -> string:trim(Str, leading, Chars);
trim(Str, both, Chars) -> string:trim(Str, both, Chars).
-else.
trim(Str) -> string:strip(rebar_utils:to_list(Str)).
trim(Str, Dir, [Chars|_]) -> string:strip(rebar_utils:to_list(Str), Dir, Chars).
-endif.

say(Say) ->
    io:format(lists:flatten([Say, "~n"])).

-spec say(string(), [term()] | term()) -> ok.
say(Say, Args) when is_list(Args) ->
    io:format(lists:flatten([Say, "~n"]), Args);
say(Say, Args) ->
    io:format(lists:flatten([Say, "~n"]), [Args]).
