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
%%% @doc Ruby options handling
%%% @author Dmitry Vasiliev <dima@hlabs.org>
%%% @copyright 2009-2015 Dmitry Vasiliev <dima@hlabs.org>
%%% @private
%%%

-module(ruby_options).

-author('Dmitry Vasiliev <dima@hlabs.org>').

-export([
    parse/1
    ]).


-type option() :: {ruby, Ruby :: string()}
    | {ruby_lib, Path :: string() | [Path :: string()]}
    | erlport_options:option().
-type options() :: [option()].

-export_type([option/0, options/0]).

-include("ruby.hrl").


%%
%% @doc Parse Ruby options
%%

-spec parse(Options::options()) ->
    {ok, #ruby_options{}} | {error, Reason::term()}.

parse(Options) when is_list(Options) ->
    parse(Options, #ruby_options{}).

parse([{ruby, Ruby} | Tail], Options) ->
    % Will be checked later
    parse(Tail, Options#ruby_options{ruby=Ruby});
parse([{ruby_lib, RubyLib}=Value | Tail], Options) ->
    case erlport_options:filter_invalid_paths(RubyLib) of
        {ok, Path} ->
            % Paths will be checked later
            parse(Tail, Options#ruby_options{ruby_lib=Path});
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
parse([], Options=#ruby_options{env=Env0, ruby_lib=RubyLib0,
        ruby=Ruby, port_options=PortOptions, packet=Packet,
        cd=Path, use_stdio=UseStdio}) ->
    PortOptions1 = erlport_options:update_port_options(
        PortOptions, Path, UseStdio),
    case get_ruby(Ruby) of
        {ok, RubyFilename, MajVersion} ->
            case update_ruby_lib(Env0, RubyLib0, MajVersion) of
                {ok, RubyPath, Env} ->
                    {ok, Options#ruby_options{env=Env,
                        ruby_lib=RubyPath, ruby=RubyFilename,
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
    case proplists:get_value(Name, ?RUBY_FIELDS) of
        N when is_integer(N) andalso N > 1 ->
            setelement(N, Options, Value)
    end.

update_ruby_lib(Env0, RubyPath0, MajVersion) ->
    case code:priv_dir(erlport) of
        {error, bad_name} ->
            {error, {not_found, "erlport/priv"}};
        PrivDir ->
            RubyDir = lists:concat([ruby, MajVersion]),
            ErlPortPath = erlport_options:joinpath(PrivDir, RubyDir),
            {PathFromSetEnv, Env2} = extract_ruby_lib(Env0, "", []),
            PathFromEnv = erlport_options:getenv("RUBYLIB"),
            RubyPath = erlport_options:join_path([[ErlPortPath], RubyPath0,
                erlport_options:split_path(PathFromSetEnv),
                erlport_options:split_path(PathFromEnv)]),
            Env3 = [{"RUBYLIB", RubyPath} | Env2],
            {ok, RubyPath, Env3}
    end.

get_ruby(default) ->
    case erlport_options:getenv(?RUBY_VAR_NAME) of
        "" ->
            try find_ruby(?DEFAULT_RUBY)
            catch
                throw:not_found ->
                    {error, ruby_not_found}
            end;
        Ruby ->
            try find_ruby(Ruby)
            catch
                throw:not_found ->
                    {error, {invalid_env_var, {?RUBY_VAR_NAME, Ruby},
                        not_found}}
            end
    end;
get_ruby(Ruby=[_|_]) ->
    try find_ruby(Ruby)
    catch
        throw:not_found ->
            {error, {invalid_option, {ruby, Ruby}, not_found}}
    end;
get_ruby(Ruby) ->
    {error, {invalid_option, {ruby, Ruby}}}.

find_ruby(Ruby) ->
    {RubyCommand, Options} = lists:splitwith(fun (C) -> C =/= $ end, Ruby),
    case os:find_executable(RubyCommand) of
        false ->
            throw(not_found);
        Filename ->
            Fullname = erlport_options:absname(Filename),
            case check_ruby_version(Fullname) of
                {ok, {MajVersion, MinVersion, _}} ->
                    Version = lists:concat([MajVersion, ".", MinVersion]),
                    {ok, Fullname ++ Options, Version};
                {error, _}=Error ->
                    Error
            end
    end.

extract_ruby_lib([{"RUBYLIB", P} | Tail], Path, Env) ->
    extract_ruby_lib(Tail, [P, erlport_options:pathsep() | Path], Env);
extract_ruby_lib([Item | Tail], Path, Env) ->
    extract_ruby_lib(Tail, Path, [Item | Env]);
extract_ruby_lib([], Path, Env) ->
    {lists:append(lists:reverse(Path)), lists:reverse(Env)}.

check_ruby_version(Ruby) ->
    Out = erlport_options:get_version(Ruby ++ " -v"),
    case re:run(Out, "^ruby ([0-9]+)\\.([0-9]+)\\.([0-9]+)",
            [{capture, all_but_first, list}]) of
        {match, StrVersion} ->
            Version = list_to_tuple([list_to_integer(N) || N <- StrVersion]),
            if
                Version >= {1, 8, 6} andalso Version < {2, 0, 0} ->
                    {ok, Version};
                Version >= {2, 0, 0} ->
                    {ok, {1, 9, 0}};
                true ->
                    {error, {unsupported_ruby_version, Out}}
            end;
        nomatch ->
            {error, {invalid_ruby, Ruby}}
    end.
