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

-module(dgiot_tdengine_websocket).
-author("kenneth").
-include("dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(HTTPOption, [{timeout, 60000}, {connect_timeout, 60000}]).
-define(REQUESTOption, [{body_format, binary}]).
%% API
-export([start/2]).
-export([test/0]).

test() ->
    {ok, ConnPid} = gun:open("127.0.0.1", 7041, #{
        ws_opts => #{keepalive => 1000}
    }),
    {ok, _} = gun:await_up(ConnPid),
    StreamRef = gun:ws_upgrade(ConnPid, "/rest/ws", []),
    {upgrade, [<<"websocket">>], _} = gun:await(ConnPid, StreamRef),
    %% Gun sent a ping automatically, but we silence ping/pong by default.
%%    {error, timeout} = gun:await(ConnPid, StreamRef, 2000),
    Body= #{<<"action">> => <<"version">>},
    Frame = {text, jiffy:encode(Body)},
    gun:ws_send(ConnPid, StreamRef, Frame),
    {ws, Ack} = gun:await(ConnPid, StreamRef),
    io:format("Ack ~p ~n",[Ack]).
%%    gun:close(ConnPid).

start(Ip, Port) ->
    {ok, WsPid} = gun:open(Ip, Port, #{
        ws_opts => #{keepalive => 1000}
    }),
    {ok, _} = gun:await_up(WsPid),
    StreamRef = gun:ws_upgrade(WsPid, "/rest/ws", []),
    {upgrade, [<<"websocket">>], _} = gun:await(WsPid, StreamRef),
    {WsPid, StreamRef}.

