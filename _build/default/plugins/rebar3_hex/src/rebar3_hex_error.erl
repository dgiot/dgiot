-module(rebar3_hex_error).

-export([format_error/1]).

format_error({required, repo}) ->
    "A repository argument is required for this command.";
format_error(Reason) ->
    try io_lib:format("~p", [Reason]) of
        Result ->
            Result
    catch
        _:_  ->
            io_lib:format("Unknown error encountered : ~ts", [Reason])
    end.

