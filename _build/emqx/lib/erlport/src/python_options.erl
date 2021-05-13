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
%%% @doc Python options handling
%%% @author Dmitry Vasiliev <dima@hlabs.org>
%%% @copyright 2009-2015 Dmitry Vasiliev <dima@hlabs.org>
%%% @private
%%%

-module(python_options).

-author('Dmitry Vasiliev <dima@hlabs.org>').

-export([
    parse/1
    ]).

-define(TIMEOUT, 15000).

-type option() :: {python, Python :: string()}
    | {python_path, Path :: string() | [Path :: string()]}
    | erlport_options:option().
-type options() :: [option()].

-export_type([option/0, options/0]).

-include("python.hrl").


%%
%% @doc Parse Python options
%%

-spec parse(Options::options()) ->
    {ok, #python_options{}} | {error, Reason::term()}.

parse(Options) when is_list(Options) ->
    parse(Options, #python_options{}).

parse([{python, Python} | Tail], Options) ->
    % Will be checked later
    parse(Tail, Options#python_options{python=Python});
parse([{python_path, PythonPath}=Value | Tail], Options) ->
    case erlport_options:filter_invalid_paths(PythonPath) of
        {ok, Path} ->
            % Paths will be checked later
            parse(Tail, Options#python_options{python_path=Path});
        {error, Invalid} ->
            {error, {invalid_option, Value, Invalid}}
    end;
parse([Option | Tail], Options) ->
    case erlport_options:parse(Option) of
        {ok, Name, Value} ->
            parse(Tail, set_by_name(Name, Value, Options));
        {error, _}=Error ->
            Error
    end;
parse([], Options=#python_options{env=Env0, python_path=PythonPath0,
        python=Python, port_options=PortOptions, packet=Packet,
        cd=Path, use_stdio=UseStdio}) ->
    PortOptions1 = erlport_options:update_port_options(
        PortOptions, Path, UseStdio),
    case get_python(Python) of
        {ok, PythonFilename, MajVersion} ->
            case update_python_path(Env0, PythonPath0, MajVersion) of
                {ok, PythonPath, Env} ->
                    {ok, Options#python_options{env=Env,
                        python_path=PythonPath, python=PythonFilename,
                        port_options=[{env, Env}, {packet, Packet}
                            | PortOptions1]}};
                {error, _}=Error ->
                    Error
            end;
        {error, _}=Error ->
            Error
    end.

%%%
%%% Utility functions
%%%

set_by_name(Name, Value, Options) ->
    case proplists:get_value(Name, ?PYTHON_FIELDS) of
        N when is_integer(N) andalso N > 1 ->
            setelement(N, Options, Value)
    end.

update_python_path(Env0, PythonPath0, MajVersion) ->
    case code:priv_dir(erlport) of
        {error, bad_name} ->
            {error, {not_found, "erlport/priv"}};
        PrivDir ->
            PythonDir = lists:concat([python, MajVersion]),
            ErlPortPath = erlport_options:joinpath(PrivDir, PythonDir),
            {PathFromSetEnv, Env2} = extract_python_path(Env0, "", []),
            PathFromEnv = erlport_options:getenv("PYTHONPATH"),
            PythonPath = erlport_options:join_path([[ErlPortPath], PythonPath0,
                erlport_options:split_path(PathFromSetEnv),
                erlport_options:split_path(PathFromEnv)]),
            Env3 = [{"PYTHONPATH", PythonPath} | Env2],
            {ok, PythonPath, Env3}
    end.

get_python(default) ->
    case erlport_options:getenv(?PYTHON_VAR_NAME) of
        "" ->
            try find_python(?DEFAULT_PYTHON)
            catch
                throw:not_found ->
                    {error, python_not_found}
            end;
        Python ->
            try find_python(Python)
            catch
                throw:not_found ->
                    {error, {invalid_env_var, {?PYTHON_VAR_NAME, Python},
                        not_found}}
            end
    end;
get_python(Python=[_|_]) ->
    try find_python(Python)
    catch
        throw:not_found ->
            {error, {invalid_option, {python, Python}, not_found}}
    end;
get_python(Python) ->
    {error, {invalid_option, {python, Python}}}.

find_python(Python) ->
    {PythonCommand, Options} = lists:splitwith(fun (C) -> C =/= $ end, Python),
    case os:find_executable(PythonCommand) of
        false ->
            throw(not_found);
        Filename ->
            Fullname = erlport_options:absname(Filename),
            case check_python_version(Fullname) of
                {ok, {MajVersion, _, _}} ->
                    {ok, Fullname ++ Options, MajVersion};
                {error, _}=Error ->
                    Error
            end
    end.

extract_python_path([{"PYTHONPATH", P} | Tail], Path, Env) ->
    extract_python_path(Tail, [P, erlport_options:pathsep() | Path], Env);
extract_python_path([Item | Tail], Path, Env) ->
    extract_python_path(Tail, Path, [Item | Env]);
extract_python_path([], Path, Env) ->
    {lists:append(lists:reverse(Path)), lists:reverse(Env)}.

check_python_version(Python) ->
    Out = erlport_options:get_version(Python ++ " -V"),
    case re:run(Out, "^Python ([0-9]+)\\.([0-9]+)\\.([0-9]+)",
            [{capture, all_but_first, list}]) of
        {match, StrVersion} ->
            Version = list_to_tuple([list_to_integer(N) || N <- StrVersion]),
            if
                Version >= {2, 5, 0} ->
                    {ok, Version};
                true ->
                    {error, {unsupported_python_version, Out}}
            end;
        nomatch ->
            {error, {invalid_python, Python}}
    end.
