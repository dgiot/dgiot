%% Copyright (c) 2012, Magnus Klaar <klaar@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @doc Runtime context for events.
-module(gr_context).

-export([
    make/1
]).

make(Options) ->
    make_(undefined, undefined, undefined, undefined, Options).

make_(_Node, App, Pid, Time, [{'$n', Node}|T]) ->
    make_(Node, App, Pid, Time, T);
make_(Node, _App, Pid, Time, [{'$a', App}|T]) ->
    make_(Node, App, Pid, Time, T);
make_(Node, App, _Pid, Time, [{'$p', Pid}|T]) ->
    make_(Node, App, Pid, Time, T);
make_(Node, App, Pid, _Time, [{'$t', Time}|T]) ->
    make_(Node, App, Pid, Time, T);
make_(Node, App, Pid, Time, []) ->
    Pid2 = case Pid of undefined -> self(); _ -> Pid end,
    Node2 = case Node of undefined -> node(Pid2); _ -> Node end,
    App2 = case App of undefined -> application(Pid2); _ -> App end,
    Time2 = case Time of undefined -> os:timestamp(); _ -> Time end,
    {Node2, App2, Pid2, Time2}.

application(Pid) when Pid =:= self() ->
    case application:get_application(group_leader()) of
        {ok, App} -> App;
        undefined -> undefined
    end;
application(Pid) ->
    {_, GroupLeader} = erlang:process_info(Pid, group_leader),
    case application:get_application(GroupLeader) of
        {ok, App} -> App;
        undefined -> undefined
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

make_defaults_test() ->
    {Node, App, Pid, Time} = gr_context:make([]),
    ?assertEqual(Node, node()),
    ?assertEqual(Pid, self()),
    ?assert(is_atom(App)),
    ?assertMatch({_,_,_}, Time).


make_override_test() ->
    Pid = spawn(fun() -> ok end),
    {Node, App, Pid, Time} = gr_context:make([
        {'$n', nodename}, {'$a', appname}, {'$p', Pid}, {'$t', timeval}]),
    ?assertEqual(nodename, Node),
    ?assertEqual(appname, App),
    ?assertEqual(timeval, Time).

-endif.
