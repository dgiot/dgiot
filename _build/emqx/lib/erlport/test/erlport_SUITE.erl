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

-module(erlport_SUITE).

-compile([export_all, nowarn_export_all]).

-import(erlport_test_utils, [script_path/1]).

%%--------------------------------------------------------------------
%% Setups
%%--------------------------------------------------------------------

all() ->
    [{group, python3}, {group, java}].

groups() ->
    Cases = [t_echo, t_rev_call],
    [{python3, Cases}, {java, Cases}].

init_per_suite(Cfg) ->
    application:ensure_all_started(erlport),
    Cfg.

end_per_suite(_) ->
    application:stop(erlport),
    ok.

init_per_group(GrpName = python3, Cfg) ->
    Opts = [{python, atom_to_list(GrpName)},
            {python_path, script_path(GrpName)}],
    {ok, Pid} = python:start(Opts),
    [{pid, Pid}, {mod, 'echo'}, {'fun', echo} | Cfg];

init_per_group(GrpName = java, Cfg) ->
    Opts = [{java, atom_to_list(GrpName)},
            {java_path, script_path(GrpName)}],
    {ok, Pid} = java:start(Opts),
    [{pid, Pid}, {mod, 'Echo'} | Cfg].

end_per_group(_GrpName, Cfg) ->
    Pid = proplists:get_value(pid, Cfg),
    erlport:stop(Pid),
    ok.

%%--------------------------------------------------------------------
%% Callback from other languages
%%--------------------------------------------------------------------
handle_call(Pid, Req) ->
    Pid ! {resp, Req}.

%%--------------------------------------------------------------------
%% Cases
%%--------------------------------------------------------------------

t_echo(Cfg) ->
    Pid = proplists:get_value(pid, Cfg),
    Mod = proplists:get_value(mod, Cfg),
    Arg = x,
    Arg = erlport:call(Pid, Mod, 'echo', [Arg], []),
    ok.

t_rev_call(Cfg) ->
    Pid = proplists:get_value(pid, Cfg),
    Mod = proplists:get_value(mod, Cfg),
    Arg = x,
    _ = erlport:call(Pid, Mod, 'rev_call', [self(), x], []),
   receive
       {resp, Resp} ->
           Arg = Resp
   after
       5000 ->
           error(receiving_timeout)
   end.

