%%% Copyright (c) 2020, EMQ X <https://github.com/emqx>
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

-module(prop_erlport).

-include_lib("proper/include/proper.hrl").

-define(ALL(Vars, Types, Exprs),
        ?SETUP(fun() ->
            State = do_setup(),
            put(state, State),
            fun() -> do_teardown(State) end
         end, ?FORALL(Vars, Types, Exprs))).

%%--------------------------------------------------------------------
%% Properties
%%--------------------------------------------------------------------

prop_echo_python() ->
    ?ALL(Term, supported_types(),
         begin
            #{python := Pid} = get(state),
             Ret = erlport:call(Pid, 'echo', 'echo', [Term], []),
             Ret = Term, true
         end).

prop_echo_java() ->
    ?ALL(Term, supported_types(),
         begin
             #{java := Pid} = get(state),
             Ret = erlport:call(Pid, 'Echo', 'echo', [Term], []),
             Ret = Term, true
         end).

%%--------------------------------------------------------------------
%% Helper
%%--------------------------------------------------------------------

do_setup() ->
    {ok, P} = python:start([{python, "python3"}, {python_path, script_path(python3)}]),
    {ok, J} = java:start([{java, "java"}, {java_path, script_path(java)}]),
    #{java => J, python => P}. 

do_teardown(_) ->
    ok.

script_path(GrpName) ->
    ScriptPath = filename:join([code:lib_dir(erlport), "test", atom_to_list(GrpName)]),
    _ = compile(GrpName, ScriptPath),
    ScriptPath.

compile(_GrpName = java, Path) ->
    ErlPortJar = filename:join([code:lib_dir(erlport), "priv", "java", "_pkgs", "erlport.jar"]),
    ct:pal(os:cmd(lists:concat(["cd ", Path, " && ",
                                "rm -rf Echo.class && ",
                                "javac -cp ", ErlPortJar, " Echo.java"]))),
    ok;
compile(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% Generator
%%--------------------------------------------------------------------

supported_types() ->
    oneof([atom(), tuple(), binary(), list(), number()]).
