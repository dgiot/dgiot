%%% Copyright (c) 2009-2015, Dmitry Vasiliev <dima@hlabs.org>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%%  * Redistributions of source code must retain the above copyright notice,
%%%    this list of conditions and the following disclaimer.
%%%  * Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%%  * Neither the name of the copyright holders nor the names of its
%%%    contributors may be used to endorse or promote products derived from
%%%    this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.

%%%
%%% @doc ErlPort options handling
%%% @author Dmitry Vasiliev <dima@hlabs.org>
%%% @copyright 2009-2015 Dmitry Vasiliev <dima@hlabs.org>
%%% @private
%%%

-module(erlport_options).

-author('Dmitry Vasiliev <dima@hlabs.org>').

-export([
    parse/1,
    timeout/1,
    update_port_options/3,
    join_path/1,
    filter_invalid_paths/1,
    get_version/1,
    pathsep/0,
    absname/1,
    joinpath/2,
    getenv/1,
    split_path/1
    ]).

-define(TIMEOUT, 15000).

-type option() :: nouse_stdio
    | use_stdio
    | {compressed, 0..9}
    | {cd, Path :: string()}
    | {packet, 1 | 2 | 4}
    | {start_timeout, pos_integer() | infinity}
    | {call_timeout, pos_integer() | infinity}
    | {env, [{Name :: string(), Value :: string() | false}]}
    | {buffer_size, Size::pos_integer()}.
-type option_name() :: use_stdio
    | cd
    | compressed
    | packet
    | env
    | start_timeout
    | call_timeout
    | buffer_size.

-export_type([option/0]).


%%
%% @doc Parse generic ErlPort option
%%

-spec parse(Option::option()) ->
    {ok, option_name(), Value::term()} | {error, Reason::term()}.

parse(use_stdio=UseStdio) ->
    {ok, use_stdio, UseStdio};
parse(nouse_stdio=UseStdio) ->
    case os:type() of
        {win32, _} ->
            {error, {unsupported_on_this_platform, UseStdio}};
        _ ->
            {ok, use_stdio, UseStdio}
    end;
parse({compressed, Level}=Value) ->
    if
        is_integer(Level) andalso Level >= 0 andalso Level =< 9 ->
            {ok, compressed, Level};
        true ->
            {error, {invalid_option, Value}}
    end;
parse({packet, Packet}=Value) ->
    case lists:member(Packet, [1, 2, 4]) of
        true ->
            {ok, packet, Packet};
        false ->
            {error, {invalid_option, Value}}
    end;
parse({cd, Path}=Value) ->
    case filelib:is_dir(Path) of
        true ->
            {ok, cd, Path};
        false ->
            {error, {invalid_option, Value}}
    end;
parse({env, Env}=Value) ->
    case filter_invalid_env(Env) of
        [] ->
            {ok, env, Env};
        Invalid ->
            {error, {invalid_option, Value, Invalid}}
    end;
parse({start_timeout, Timeout}=Value) ->
    case timeout(Timeout) of
        {ok, T} ->
            {ok, start_timeout, T};
        error ->
            {error, {invalid_option, Value}}
    end;
parse({call_timeout, Timeout}=Value) ->
    case timeout(Timeout) of
        {ok, T} ->
            {ok, call_timeout, T};
        error ->
            {error, {invalid_option, Value}}
    end;
parse({buffer_size, Size}=Value) ->
    case is_integer(Size) andalso Size > 0 of
        true ->
            {ok, buffer_size, Size};
        false ->
            {error, {invalid_option, Value}}
    end;
parse(UnknownOption) ->
    {error, {unknown_option, UnknownOption}}.

%%%
%%% Utility functions
%%%

%%
%% @doc Check timeout value
%%

-spec timeout(Timeout::pos_integer() | infinity) ->
    {ok, Timeout::pos_integer() | infinity} | error.

timeout(Timeout) when is_integer(Timeout) andalso Timeout > 0 ->
    {ok, Timeout};
timeout(Timeout=infinity) ->
    {ok, Timeout};
timeout(_) ->
    error.

%%
%% @doc Update port options with path and use_stdio options
%%

-spec update_port_options(PortOptions::[{atom(), term()} | atom()],
        Path::string() | undefined, UseStdio::use_stdio | nouse_stdio) ->
    PortOptions::[{atom(), term()} | atom()].

update_port_options(PortOptions, Path, UseStdio) ->
    PortOptions1 = case Path of
        undefined ->
            PortOptions;
        Path ->
            [{cd, Path} | PortOptions]
    end,
    case UseStdio of
        nouse_stdio ->
            [UseStdio | PortOptions1];
        _ ->
            PortOptions1
    end.

%%
%% @doc Join paths to a ":" (";" on Windows) delimited string and remove
%% duplicate parts
%%

-spec join_path(Parts::[Path::string() | [Path::string()]]) -> Path::string().

join_path(Parts=[_|_]) ->
    remove_duplicate_path(lists:append(Parts), [], ordsets:new()).

%%
%% @doc Filter invalid paths from list of paths
%%

-spec filter_invalid_paths(Paths::[Path::string() | [Path::string()]]) ->
    {ok, Paths::[Path::string() | [Path::string()]]} | {error, term()}.

filter_invalid_paths(Paths=[List | _]) when is_list(List) ->
    case lists:filter(fun (L) -> not is_list(L) end, Paths) of
        [] ->
            {ok, Paths};
        Invalid ->
            {error, Invalid}
    end;
filter_invalid_paths(Path=[Integer | _]) when is_integer(Integer) ->
    case lists:filter(fun (I) -> not is_integer(I) end, Path) of
        "" ->
            {ok, string:tokens(Path, pathsep())};
        Invalid ->
            {error, Invalid}
    end;
filter_invalid_paths(List) when is_list(List) ->
    {error, invalid_path};
filter_invalid_paths(_Paths) ->
    {error, not_list}.

%%
%% @doc Get version line for Cmd
%%

-spec get_version(Cmd::string()) -> Version::string().

get_version(Cmd) ->
    Port = open_port({spawn, Cmd}, [{line, 80}, stderr_to_stdout, hide]),
    receive_version(Port).

%%
%% @doc Return PATH variable separator for the current platform
%%

-spec pathsep() -> PathSep::string().

pathsep() ->
    case os:type() of
        {win32, _} ->
            ";";
        _ ->
            ":"
    end.

%%
%% @doc Return absolute name for Filename
%%

-spec absname(Filename::string()) -> Path::string().

absname(Filename) ->
    Nativename = filename:nativename(filename:absname(Filename)),
    case os:type() of
        {win32, _} ->
            lists:concat(["\"", Nativename, "\""]);
        _ -> Nativename
    end.

%%
%% @doc Join directory and filename components
%%

-spec joinpath(Dir::string(), Filename::string()) -> Path::string().

joinpath(Dir, Filename) ->
    filename:nativename(filename:join(Dir, Filename)).

%%
%% @doc Return value of environment variable or empty string
%%

-spec getenv(Key::string()) -> Value::string().

getenv(Key) ->
    case os:getenv(Key) of
        false ->
            "";
        Value ->
            Value
    end.

%%
%% @doc Split PATH-like environment variable value:w
%%

-spec split_path(Path::string()) -> [Part::string()].

split_path(Path) ->
    string:tokens(Path, pathsep()).

%%%
%%% Internal functions
%%%

remove_duplicate_path([P | Tail], Paths, Seen) ->
    case P of
        "" ->
            remove_duplicate_path(Tail, Paths, Seen);
        P ->
            % Paths also can be virtual (for example a path inside an archive)
            % so compare them as strings without any normalization
            case ordsets:is_element(P, Seen) of
                false ->
                    Seen2 = ordsets:add_element(P, Seen),
                    remove_duplicate_path(Tail, [P | Paths], Seen2);
                true ->
                    remove_duplicate_path(Tail, Paths, Seen)
            end
    end;
remove_duplicate_path([], Paths, _Seen) ->
    string:join(lists:reverse(Paths), pathsep()).

filter_invalid_env(Env) when is_list(Env) ->
    lists:filter(fun
        ({N, V}) when is_list(N), is_list(V) ->
            false;
        (_) ->
            true
        end, Env);
filter_invalid_env(_Env) ->
    not_list.

receive_version(Port) ->
    Out = receive
        {Port, {data, {eol, Version}}} ->
            Version;
        {Port, _} ->
            "ERROR";
        {'EXIT', Port, _Reason} ->
            "ERROR"
    after
        ?TIMEOUT ->
            "TIMEOUT"
    end,
    catch port_close(Port),
    Out.
