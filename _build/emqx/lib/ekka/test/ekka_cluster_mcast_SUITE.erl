%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(ekka_cluster_mcast_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

-define(OPTIONS, [{addr, {239,192,0,1}},
                  {ports, [4369,4370]},
                  {iface, {0,0,0,0}},
                  {ttl, 255},
                  {loop, true}
                 ]).

all() -> ekka_ct:all(?MODULE).

%%--------------------------------------------------------------------
%% CT callbacks
%%--------------------------------------------------------------------

init_per_testcase(t_discover, Config) ->
    ok = meck:new(gen_udp, [unstick, non_strict, passthrough, no_history]),
    ok = meck:expect(gen_udp, send, fun(_, _, _, _) -> ok end),
    ok = meck:new(ekka_cluster_sup, [non_strict, passthrough, no_history]),
    ok = meck:expect(ekka_cluster_sup, start_child,
                     fun(_, _) ->
                             ekka_cluster_mcast:start_link(?OPTIONS)
                     end),
    Config;
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(t_discover, Config) ->
    ok = meck:unload(gen_udp),
    ok = meck:unload(ekka_cluster_sup),
    Config;
end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

t_discover(_) ->
    %% Simulate a UDP packet.
    Cookie = erlang:phash2(erlang:get_cookie()),
    Pong = {pong, 'node1@192.168.10.10', ekka, Cookie},
    Pid = ekka_cluster_mcast:ensure_started(?OPTIONS),
    ?assert(is_process_alive(Pid)),
    Sock = ekka_cluster_mcast:get_sock(),
    Datagram = {udp, Sock, {127,0,0,1}, 5000, term_to_binary(Pong)},
    Pid ! Datagram,
    Node = node(),
    {ok, [Node, 'node1@192.168.10.10']} = ekka_cluster_mcast:discover(?OPTIONS),
    ok = ekka_cluster_mcast:stop().

t_lock(_) ->
    ignore = ekka_cluster_mcast:lock([]).

t_unlock(_) ->
    ignore = ekka_cluster_mcast:unlock([]).

t_register(_) ->
    ignore = ekka_cluster_mcast:register([]).

t_unregister(_) ->
    ignore = ekka_cluster_mcast:unregister([]).

