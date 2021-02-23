%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(cuttlefish_test_group_leader).

-export([new_group_leader/1, 
         group_leader_loop/2, 
         tidy_up/1,
         get_output/0]).

%% @doc spawns the new group leader
new_group_leader(Runner) ->
    spawn_link(?MODULE, group_leader_loop, [Runner, queue:new()]).

%% @doc listens for io_requests, and pipes them into an array
group_leader_loop(Runner, Output) ->
    receive
        {io_request, From, ReplyAs, Req} ->
            P = process_flag(priority, normal),
            %% run this part under normal priority always
            NewOutput = io_request(From, ReplyAs, Req, Output),
            process_flag(priority, P),            
            group_leader_loop(Runner, NewOutput);        
        {get_output, Ref, From} ->
            From ! {Ref, queue:to_list(Output)},
            group_leader_loop(Runner, Output);
        stab ->
            kthxbye;
        _ ->
            %% discard any other messages
            group_leader_loop(Runner, Output)
    end.

%% @doc closes group leader down
tidy_up(FormerGroupLeader) ->
    GroupLeaderToMurder = group_leader(),
    group_leader(FormerGroupLeader, self()),
    GroupLeaderToMurder ! stab.

%% @doc Retrieves the io output from the group leader
get_output() ->
    GL = group_leader(),
    Ref = make_ref(),
    GL ! {get_output, Ref, self()},
    receive
        {Ref, Output} ->
            {ok, Output}
    after 1000 ->
            error
    end.

%% Processes an io_request and sends a reply
io_request(From, ReplyAs, Req, Output) ->
    {Reply, NewOutput} = io_request(Req, Output),
    io_reply(From, ReplyAs, Reply),
    NewOutput.

%% sends a reply back to the sending process
io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

%% If we're processing io:put_chars, Chars shows up as binary
io_request({put_chars, Chars}, Output) when is_binary(Chars); is_list(Chars) ->
    {ok, queue:in(Chars, Output)};
io_request({put_chars, M, F, As}, Output) ->
    try apply(M, F, As) of
        Chars ->
            {ok, queue:in(Chars, Output)}
    catch
        C:T -> {{error, {C,T,erlang:get_stacktrace()}}, Output}
    end;
io_request({put_chars, _Enc, Chars}, Output) ->
    io_request({put_chars, Chars}, Output);
io_request({put_chars, _Enc, Mod, Func, Args}, Output) ->
    io_request({put_chars, Mod, Func, Args}, Output);
%% The rest of these functions just handle expected messages from
%% the io module. They're mostly i, but we only care about o.
io_request({get_chars, _Enc, _Prompt, _N}, O) ->
    {eof, O};
io_request({get_chars, _Prompt, _N}, O) ->
    {eof, O};
io_request({get_line, _Prompt}, O) ->
    {eof, O};
io_request({get_line, _Enc, _Prompt}, O) ->
    {eof, O};
io_request({get_until, _Prompt, _M, _F, _As}, O) ->
    {eof, O};
io_request({setopts, _Opts}, O) ->
    {ok, O};
io_request(getopts, O) ->
    {{error, enotsup}, O};
io_request({get_geometry,columns}, O) ->
    {{error, enotsup}, O};
io_request({get_geometry,rows}, O) ->
    {{error, enotsup}, O};
io_request({requests, Reqs}, O) ->
    {io_requests(Reqs, ok, O), O};
io_request(_, O) ->
    {{error, request}, O}.

io_requests([R | Rs], ok, Output) ->
    {Result, NewOutput} = io_request(R, Output),
    io_requests(Rs, Result, NewOutput);
io_requests(_, Result, Output) ->
    {Result, Output}.
