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

-module(emqtt_sock_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> emqx_ct:all(?MODULE).

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

t_tcp_sock(_) ->
    Server = tcp_server:start_link(4000),
    {ok, Sock} = emqtt_sock:connect("127.0.0.1", 4000, [], 3000),
    send_and_recv_with(Sock),
    ok = emqtt_sock:close(Sock),
    ok = tcp_server:stop(Server).

t_ssl_sock(Config) ->
    SslOpts = [{certfile, certfile(Config)},
               {keyfile,  keyfile(Config)}
              ],
    Server = ssl_server:start_link(4443, SslOpts),
    {ok, Sock} = emqtt_sock:connect("127.0.0.1", 4443, [{ssl_opts, []}], 3000),
    send_and_recv_with(Sock),
    ok = emqtt_sock:close(Sock),
    ssl_server:stop(Server).

send_and_recv_with(Sock) ->
    {ok, {{127,0,0,1}, _}} = emqtt_sock:sockname(Sock),
    ok = emqtt_sock:send(Sock, <<"hi">>),
    {ok, <<"hi">>} = emqtt_sock:recv(Sock, 0),
    ok = emqtt_sock:setopts(Sock, [{active, 100}]),
    {ok, Stats} = emqtt_sock:getstat(Sock, [send_cnt, recv_cnt]),
    [{send_cnt, Cnt}, {recv_cnt, Cnt}] = Stats,
    ?assert((Cnt == 1) or (Cnt == 3)).

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

certfile(Config) ->
    filename:join([test_dir(Config), "certs", "test.crt"]).

keyfile(Config) ->
    filename:join([test_dir(Config), "certs", "test.key"]).

test_dir(Config) ->
    filename:dirname(filename:dirname(proplists:get_value(data_dir, Config))).

