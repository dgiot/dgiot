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

-module(esockd_transport_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include("esockd.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TCP_OPTS, [{backlog, 1024}, {reuseaddr, true}]).


all() -> esockd_ct:all(?MODULE).

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(esockd),
    Config.

end_per_suite(_Config) ->
    application:stop(esockd).

t_type(_) ->
    {ok, LSock} = esockd_transport:listen(0, []),
    tcp = esockd_transport:type(LSock),
    ssl = esockd_transport:type(#ssl_socket{}),
    proxy = esockd_transport:type(#proxy_socket{}),
    ok = esockd_transport:close(LSock).

t_is_ssl(_) ->
    {ok, LSock} = esockd_transport:listen(0, []),
    false = esockd_transport:is_ssl(LSock),
    true = esockd_transport:is_ssl(SslSock = #ssl_socket{tcp = LSock}),
    false = esockd_transport:is_ssl(#proxy_socket{socket = LSock}),
    true = esockd_transport:is_ssl(#proxy_socket{socket = SslSock}),
    ok = esockd_transport:close(LSock).

t_wait_and_ready(_) ->
    {ok, LSock} = esockd_transport:listen(0, []),
    esockd_transport:ready(self(), LSock, [fun(Sock) -> {ok, Sock} end]),
    {ok, LSock} = esockd_transport:wait(LSock),
    ok = esockd_transport:close(LSock).

t_listen(_) ->
    {ok, Sock} = esockd_transport:listen(0, []),
    ?assert(is_port(Sock)).

t_controlling_process(_) ->
    {ok, LSock} = esockd_transport:listen(0, []),
    ok = esockd_transport:controlling_process(LSock, self()),
    %%ok = esockd_transport:controlling_process(#ssl_socket{ssl = SslSock}, self()),
    ok = esockd_transport:close(LSock).

t_close_tcp(Config) ->
    {ok, LSock} = gen_tcp:listen(3000, [{reuseaddr, true}]),
    ok = esockd_transport:close(LSock).

t_close_ssl(Config) ->
    {ok, SslLSock} = ssl:listen(8883, [binary, {reuseaddr, true},
                                       {certfile, esockd_ct:certfile(Config)},
                                       {keyfile, esockd_ct:keyfile(Config)}
                                      ]),
    ok = esockd_transport:close(#ssl_socket{ssl = SslLSock}).

t_close_proxy(_) ->
    {ok, LSock} = gen_tcp:listen(4000, [{reuseaddr, true}]),
    ok = esockd_transport:close(#proxy_socket{socket = LSock}).

t_send_recv_tcp(_) ->
    {ok, _} = esockd:open(echo, 3000, [{tcp_options, ?TCP_OPTS}], {echo_server, start_link, []}),
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, 3000, [binary, {active, false}]),
    ok = esockd_transport:send(Sock, <<"Hello">>),
    {ok, <<"Hello">>} = esockd_transport:recv(Sock, 0),
    ok = esockd:close(echo, 3000).

t_send_ssl(Config) ->
    ssl:start(),
    SslOpts = [{certfile, esockd_ct:certfile(Config)},
               {keyfile, esockd_ct:keyfile(Config)}],
    {ok, _} = esockd:open(echo, 8883, [{ssl_options, SslOpts}], {echo_server, start_link, []}),
    {ok, SslSock} = ssl:connect({127,0,0,1}, 8883, [], 3000),
    ok = esockd_transport:send(#ssl_socket{ssl = SslSock}, <<"Hello">>),
    ok = esockd_transport:close(#ssl_socket{ssl = SslSock}),
    ok = esockd:close(echo, 8883).

t_send_proxy(_) ->
    {ok, _} = esockd:open(echo, 5000, [{tcp_options, [binary]},
                                       proxy_protocol,
                                       {proxy_protocol_timeout, 3000}
                                      ],
                          {echo_server, start_link, []}),
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, 5000, [binary, {active, false}]),
    ok = gen_tcp:send(Sock, <<"PROXY TCP4 192.168.1.1 192.168.1.2 80 81\r\n">>),
    ok = esockd_transport:send(Sock, <<"Hello">>),
    {ok, <<"Hello">>} = esockd_transport:recv(Sock, 0),
    ok = esockd_transport:close(Sock),
    ok = esockd:close(echo, 5000).

t_async_send(_) ->
    {ok, _} = esockd:open(echo, 3000, [{tcp_options, ?TCP_OPTS}], {echo_server, start_link, []}),
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, 3000, [binary, {active, false}]),
    ok = esockd_transport:async_send(Sock, <<"Hello">>),
    {ok, <<"Hello">>} = esockd_transport:recv(Sock, 0),
    ok = esockd_transport:close(Sock),
    ok = esockd:close(echo, 3000).

t_async_recv(_) ->
    {ok, _} = esockd:open(echo, 3000, [{tcp_options, ?TCP_OPTS}], {async_echo_server, start_link, []}),
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, 3000, [binary, {active, false}]),
    ok = esockd_transport:async_send(Sock, <<"Hello">>),
    {ok, <<"Hello">>} = esockd_transport:recv(Sock, 0),
    ok =esockd_transport:close(Sock),
    ok = esockd:close(echo, 3000).

t_get_setopts(_) ->
    {ok, _} = esockd:open(echo, 3000, [{tcp_options, ?TCP_OPTS}], {echo_server, start_link, []}),
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, 3000, [binary, {active, false}]),
    ok = esockd_transport:setopts(Sock, [{active, true}]),
    {ok, [{active, true}]} = esockd_transport:getopts(Sock, [active]),
    ok = esockd_transport:close(Sock),
    ok = esockd:close(echo, 3000).

t_getstat(_) ->
    {ok, _} = esockd:open(echo, 3000, [{tcp_options, ?TCP_OPTS}], {echo_server, start_link, []}),
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, 3000, [{active, false}]),
    {ok, [{recv_oct, 0}, {recv_cnt, 0}, {send_oct, 0}, {send_cnt, 0}]}
      = esockd_transport:getstat(Sock, [recv_oct, recv_cnt, send_oct, send_cnt]),
    ok = esockd_transport:close(Sock),
    ok = esockd:close(echo, 3000).

t_sockname(_) ->
    {ok, LSock} = esockd_transport:listen(3000, [{reuseaddr, true}]),
    {ok, {{0,0,0,0}, 3000}} = esockd_transport:sockname(LSock),
    ok = esockd_transport:close(LSock).

t_peername(_) ->
    {ok, _} = esockd:open(echo, 3000, [{tcp_options, ?TCP_OPTS}], {echo_server, start_link, []}),
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, 3000, [{active, false}]),
    {ok, {{127,0,0,1}, 3000}} = esockd_transport:peername(Sock),
    {ok, {{127,0,0,1}, 3000}} = esockd_transport:ensure_ok_or_exit(peername, [Sock]),
    {ok, Sockname} = esockd_transport:sockname(Sock),
    {ok, Sockname} = esockd_transport:ensure_ok_or_exit(sockname, [Sock]),
    ok = esockd_transport:close(Sock),
    ok = esockd:close(echo, 3000).

t_peercert(_) ->
    {ok, LSock} = esockd_transport:listen(3000, [{reuseaddr, true}]),
    nossl = esockd_transport:peercert(LSock),
    ok = esockd_transport:close(LSock).

t_peer_cert_subject(_) ->
    {ok, LSock} = esockd_transport:listen(3000, [{reuseaddr, true}]),
    undefined = esockd_transport:peer_cert_subject(LSock),
    ok = esockd_transport:close(LSock).

t_peer_cert_common_name(_) ->
    {ok, LSock} = esockd_transport:listen(3000, [{reuseaddr, true}]),
    undefined = esockd_transport:peer_cert_common_name(LSock),
    ok = esockd_transport:close(LSock).

t_shutdown(_) ->
    {ok, _} = esockd:open(echo, 3000, [{tcp_options, ?TCP_OPTS}], {echo_server, start_link, []}),
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, 3000, [{active, false}]),
    ok = esockd_transport:shutdown(Sock, read_write),
    ok = esockd:close(echo, 3000).

t_gc(_) ->
    {ok, LSock} = esockd_transport:listen(3000, [{reuseaddr, true}]),
    ok = esockd_transport:gc(LSock),
    ok = esockd_transport:close(LSock).

t_proxy_upgrade_fun(_) ->
    Fun = esockd_transport:proxy_upgrade_fun([{proxy_protocol_timeout, 1000}]),
    ?assert(is_function(Fun)).

t_ssl_upgrade_fun(_) ->
    Fun = esockd_transport:ssl_upgrade_fun([{handshake_timeout, 10000}]),
    ?assert(is_function(Fun)).

t_fast_close(_) ->
    {ok, LSock} = esockd_transport:listen(3000, [{reuseaddr, true}]),
    ok = esockd_transport:fast_close(LSock),
    ok = esockd_transport:close(LSock).

