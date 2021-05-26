%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(dgiot_keepalive).
-author("johnliu").
-record(keepalive, { callback, statval, tref,  tmsg, keepalive_time, keepalive_intvl, keepalive_probes, repeat = 0 }).
%% API
-export([start/5, cancel/1, resume/1, check/1]).


start(Fun, Time, Intvl, Probes, TimeoutMsg) ->
    case catch Fun() of
        {ok, StatVal} ->
            {ok, #keepalive{
                statval = StatVal,
                callback = Fun,
                keepalive_time = Time,
                keepalive_intvl = Intvl,
                keepalive_probes = Probes,
                tmsg = TimeoutMsg,
                tref = timer(Time, {check, TimeoutMsg})
            }};
        {Err, Reason} when Err == error; Err == 'EXIT' ->
            {error, Reason}
    end.


%% @doc Check keepalive, called when timeout...
check(KeepAlive = #keepalive{callback = Fun, statval = LastVal, keepalive_probes = Probes, keepalive_time = TimeoutSec, repeat = Repeat}) ->
    case catch Fun() of
        {ok, NewVal} ->
            if
                NewVal =/= LastVal ->
                    {ok, resume(KeepAlive#keepalive{statval = NewVal, repeat = 0})};
                Repeat < 1 ->
                    {ok, resume(0.1, KeepAlive#keepalive{statval = NewVal, repeat = Repeat + 1})};
                Repeat < Probes ->
                    {ok, resume(TimeoutSec, KeepAlive#keepalive{statval = NewVal, repeat = Repeat + 1})};
                true ->
                    {error, timeout}
            end;
        {error, Error} ->
            {error, Error};
        {'EXIT', Reason} ->
            {error, Reason}
    end.



resume(KeepAlive = #keepalive{keepalive_time = TimeoutSec, tref = TRef, tmsg = TimeoutMsg}) ->
    catch erlang:cancel_timer(TRef),
    KeepAlive#keepalive{tref = timer(TimeoutSec, {check, TimeoutMsg})}.

resume(TimeoutSec, KeepAlive = #keepalive{ tref = TRef, tmsg = TimeoutMsg}) ->
    catch erlang:cancel_timer(TRef),
    KeepAlive#keepalive{tref = timer(TimeoutSec, {timeout, TimeoutMsg})}.



%% @doc Cancel Keepalive
cancel(#keepalive{tref = TRef}) when is_reference(TRef) ->
    catch erlang:cancel_timer(TRef), ok;
cancel(_) ->
    ok.

timer(Secs, Msg) ->
    erlang:send_after(erlang:round(timer:seconds(Secs)), self(), Msg).
