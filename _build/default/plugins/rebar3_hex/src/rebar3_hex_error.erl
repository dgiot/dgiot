-module(rebar3_hex_error).

-export([format_error/1]).

format_error({required, repo}) ->
    "A repository argument is required for this command.";

format_error({error, no_read_key}) ->
    "No read key found for user. Be sure to authenticate first with:"
    " rebar3 hex user auth";

format_error({error, no_write_key}) ->
    "No write key found for user. Be sure to authenticate first with:"
    " rebar3 hex user auth";

format_error({Cmd, unsupported_params}) ->
    io_lib:format("Either some or all of the parameters supplied for the ~ts command are ", [Cmd])
    ++ " invalid or form an invalid combination of parameters.";

format_error({Cmd, missing_required_params}) ->
    io_lib:format("Required parameters for the ~ts command have not been supplied.", [Cmd]);

format_error(Reason) ->
    rebar_api:debug("Unknown error : ~ts", [Reason]),
    "An unknown error was encountered. Run with DEBUG=1 for more details.".
