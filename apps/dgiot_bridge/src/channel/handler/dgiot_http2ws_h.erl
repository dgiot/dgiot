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
-module(dgiot_http2ws_h).
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([init/2]).
-export([req/0]).

init(Req0, Opts) ->
%%    io:format("~s ~p ~p ~n", [?FILE, ?LINE, Req0]),
    Req = cowboy_req:stream_reply(200, Req0),
    read_body_to_websocket(Req0),
    cowboy_req:stream_body("Chunked!\r\n", fin, Req),
    {cowboy_loop, Req, Opts}.

read_body_to_websocket(Req0) ->
    Key = cowboy_req:path(Req0),
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
            dgiot_ws_h:run_hook(Key, Data),
%%            io:format("~s", [Data]),
            Req;
        {more, Data, Req} ->
%%            io:format("~s", [Data]),
            dgiot_ws_h:run_hook(Key, Data),
            read_body_to_websocket(Req)
    end.

req() ->
    #{bindings => #{},
        body_length => undefined,
        cert => undefined,
        has_body => true,
        headers =>
        #{<<"accept">> => <<"*/*">>,
            <<"connection">> => <<"close">>,
            <<"host">> => <<"127.0.0.1:3080">>,
            <<"icy-metadata">> => <<"1">>,
            <<"user-agent">> => <<"Lavf/58.45.100">>},
        host => <<"127.0.0.1">>,
        host_info => undefined,
        method => <<"POST">>,
        path => <<"/test">>,
        path_info => undefined,
        peer => {{127,0,0,1},65157},
        pid => self(),
        port => 3080,
        qs => <<>>,
        ref => <<"HTTP/fa1c109cd5">>,
        scheme => <<"http">>,
        sock => {{127,0,0,1},3080},
        streamid => 1,version => 'HTTP/1.1'
    }.
