%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% --------------------------------------------------
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
%% --------------------------------------------------
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%% @author Ulf Wiger <ulf@wiger.net>
%%
-module(gproc_info).

-export([i/0]).

-import(lists, [foldl/3]).
-import(io, [format/2]).

%% c:i() extended with gproc info
-spec i() -> 'ok'.

i() -> i(processes()).

-spec i([pid()]) -> 'ok'.

i(Ps) ->
    i(Ps, length(Ps)).

-spec i([pid()], non_neg_integer()) -> 'ok'.

i(Ps, N) when N =< 100 ->
    iformat("Pid", "Initial Call", "Heap", "Reds",
            "Msgs"),
    iformat("Registered", "Current Function", "Stack", "",
            ""),
    {R,M,H,S} = foldl(fun(Pid, {R0,M0,H0,S0}) ->
                              {A,B,C,D} = display_info(Pid),
                              {R0+A,M0+B,H0+C,S0+D}
                      end, {0,0,0,0}, Ps),
    iformat("Total", "", w(H), w(R), w(M)),
    iformat("", "", w(S), "", "");
i(Ps, N) ->
    iformat("Pid", "Initial Call", "Heap", "Reds",
            "Msgs"),
    iformat("Registered", "Current Function", "Stack", "",
            ""),
    paged_i(Ps, {0,0,0,0}, N, 50).

paged_i([], {R,M,H,S}, _, _) ->
    iformat("Total", "", w(H), w(R), w(M)),
    iformat("", "", w(S), "", "");
paged_i(Ps, Acc, N, Page) ->
    {Pids, Rest, N1} =
        if N > Page ->
                {L1,L2} = lists:split(Page, Ps),
                {L1,L2,N-Page};
           true ->
                {Ps, [], 0}
        end,
    NewAcc = foldl(fun(Pid, {R,M,H,S}) ->
                           {A,B,C,D} = display_info(Pid),
                           {R+A,M+B,H+C,S+D}
                   end, Acc, Pids),
    case Rest of
        [_|_] ->
            choice(fun() -> paged_i(Rest, NewAcc, N1, Page) end);
        [] ->
            paged_i([], NewAcc, 0, Page)
    end.

choice(F) ->
    case get_line('(c)ontinue (q)uit -->', "c\n") of
        "c\n" ->
            F();
        <<"c\n">> ->
            F();
        "q\n" ->
            quit;
        <<"q\n">> ->
            quit;
        _ ->
            choice(F)
    end.


iformat(A1, A2, A3, A4, A5) ->
    format("~-21s ~-33s ~8s ~8s ~4s~n", [A1,A2,A3,A4,A5]).

get_line(P, Default) ->
    case io:get_line(P) of
        "\n" ->
            Default;
        L ->
            L
    end.

w(X) ->
    io_lib:write(X).

w(X, L) ->
    S = lists:flatten(io_lib:format("~w", [X])),
    case length(S) of
	Len when Len > L ->
	    lists:sublist(S, 1, L-3) ++ "...";
	_ ->
	    S
    end.

display_info(Pid) ->
    Res = c:display_info(Pid),
    {gproc, GI} = gproc:info(Pid, gproc),
    case GI of
	[] ->
	    skip;
	_ ->
	    display_gproc_info("Gproc: ", [{n, N,S,V} || {{n,S,N},V} <- GI]),
	    display_gproc_info("       ", [{p, N,S,V} || {{p,S,N},V} <- GI])
    end,
    Res.

display_gproc_info(_, []) ->
    skip;
display_gproc_info(Hdr, [H|T] = Entries) ->
    L = 3 + length(Hdr),
    Max = 78 - L - 8,
    {Ck, Cv} = info_cols(Entries, Max),
    Fmt = fun(I, {Type, K, Sc, V}) ->
		  if Cv == 0 ->
			  io_lib:format(
			    I ++ "~w,~w: ~-" ++ i2l(Ck)
			    ++ "s ~n", [Type, Sc, w(K, Ck)]);
		     true ->
			  io_lib:format(
			    I ++ "~w,~w: ~-" ++ i2l(Ck)
			    ++ "s | ~-" ++ i2l(Cv) ++ "s~n",
			    [Type, Sc, w(K, Ck), w(V, Cv)])
		  end
	  end,
    format("   ~s~s", [Hdr, Fmt("", H)]),
    Indent = lists:duplicate(L, $\s),
    lists:foreach(
      fun(X) ->
	      io:format(Fmt(Indent, X))
      end, T).

info_cols(L, Max) ->
    case [V || {_,_,_,V} <- L, V =/= undefined] of
	[] ->
	    {Max, 0};
	_ ->
	    KMax = lists:max([w_length(K) || {_,K,_,_} <- L]),
	    Ck = erlang:min(KMax, round(Max*2/3)),
	    {Ck, Max - Ck}
    end.

w_length(Term) ->
    lists:flatlength(io_lib:format("~w", [Term])).


i2l(I) ->
    integer_to_list(I).
