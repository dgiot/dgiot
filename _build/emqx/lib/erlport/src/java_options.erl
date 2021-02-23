%%% Copyright (c) 2020, JianBo He <heeejianbo@gmail.com>
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
%%% @doc Java options handling
%%% @author JianBo He<heeejianbo@gmail.com>
%%% @copyright 2020 JianBo He <heeejianbo@gmail.com>
%%%

-module(java_options).

-author('JianBo He<heeejianbo@gmail.com>').

-export([
    parse/1
    ]).

-define(TIMEOUT, 15000).

-type option() :: {java, Java :: string()}
    | {java_path, Path :: string() | [Path :: string()]}
    | erlport_options:option().
-type options() :: [option()].

-export_type([option/0, options/0]).

-include("java.hrl").


%%
%% @doc Parse Java options
%%

-spec parse(Options::options()) ->
    {ok, #java_options{}} | {error, Reason::term()}.

parse(Options) when is_list(Options) ->
    parse(Options, #java_options{}).

parse([{java, Java} | Tail], Options) ->
    % Will be checked later
    parse(Tail, Options#java_options{java=Java});
parse([{java_path, JavaPath}=Value | Tail], Options) ->
    case erlport_options:filter_invalid_paths(JavaPath) of
        {ok, Path} ->
            % Paths will be checked later
            parse(Tail, Options#java_options{java_path=Path});
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
parse([], Options=#java_options{env=Env0, java_path=JavaPath0,
        java=Java, port_options=PortOptions, packet=Packet,
        cd=Path, use_stdio=UseStdio}) ->
    PortOptions1 = erlport_options:update_port_options(
        PortOptions, Path, UseStdio),
    case get_java(Java) of
        {ok, JavaFilename} ->
            case update_java_path(Env0, JavaPath0) of
                {ok, JavaPath, Env} ->
                    {ok, Options#java_options{env=Env,
                        java_path=JavaPath, java=JavaFilename,
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
    case proplists:get_value(Name, ?JAVA_FIELDS) of
        N when is_integer(N) andalso N > 1 ->
            setelement(N, Options, Value)
    end.

update_java_path(Env0, JavaPath0) ->
    case code:priv_dir(erlport) of
        {error, bad_name} ->
            {error, {not_found, "erlport/priv"}};
        PrivDir ->
            JavaDir = filename:join([java, "_pkgs", "erlport.jar"]),
            ErlPortPath = erlport_options:joinpath(PrivDir, JavaDir),
            {PathFromSetEnv, Env2} = extract_java_path(Env0, "", []),
            PathFromEnv = erlport_options:getenv("JAVAPATH"),
            JavaPath = erlport_options:join_path([[ErlPortPath], JavaPath0,
                erlport_options:split_path(PathFromSetEnv),
                erlport_options:split_path(PathFromEnv)]),
            %% FIXME: Env??
            Env3 = [{"JAVAPATH", JavaPath} | Env2],
            {ok, JavaPath, Env3}
    end.

get_java(default) ->
    case erlport_options:getenv(?JAVA_VAR_NAME) of
        "" ->
            try find_java(?DEFAULT_JAVA)
            catch
                throw:not_found ->
                    {error, java_not_found}
            end;
        Java ->
            try find_java(Java)
            catch
                throw:not_found ->
                    {error, {invalid_env_var, {?JAVA_VAR_NAME, Java},
                        not_found}}
            end
    end;
get_java(Java=[_|_]) ->
    try find_java(Java)
    catch
        throw:not_found ->
            {error, {invalid_option, {java, Java}, not_found}}
    end;
get_java(Java) ->
    {error, {invalid_option, {java, Java}}}.

find_java(Java) ->
    {JavaCommand, Options} = lists:splitwith(fun (C) -> C =/= $ end, Java),
    case os:find_executable(JavaCommand) of
        false ->
            throw(not_found);
        Filename ->
            Fullname = erlport_options:absname(Filename),
            {ok, Fullname ++ Options}
    end.

extract_java_path([{"JAVAPATH", P} | Tail], Path, Env) ->
    extract_java_path(Tail, [P, erlport_options:pathsep() | Path], Env);
extract_java_path([Item | Tail], Path, Env) ->
    extract_java_path(Tail, Path, [Item | Env]);
extract_java_path([], Path, Env) ->
    {lists:append(lists:reverse(Path)), lists:reverse(Env)}.
