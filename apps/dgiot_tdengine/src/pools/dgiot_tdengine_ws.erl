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

-module(dgiot_tdengine_ws).
-author("kenneth").
-include("dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(HTTPOption, [{timeout, 60000}, {connect_timeout, 60000}]).
-define(REQUESTOption, [{body_format, binary}]).
%% API
-export([login/5, connect/5, check_version/2]).
-export([test/0]).

test() ->
    {ok, ConnPid} = gun:open("127.0.0.1", 7041, #{
        supervise => true,
        ws_opts => #{keepalive => 1000}
    }),
    {ok, _} = gun:await_up(ConnPid),
    StreamRef = gun:ws_upgrade(ConnPid, "/rest/ws", []),
    {upgrade, [<<"websocket">>], _} = gun:await(ConnPid, StreamRef),
    %% Gun sent a ping automatically, but we silence ping/pong by default.
%%    {error, timeout} = gun:await(ConnPid, StreamRef, 2000),
    Body = #{<<"action">> => <<"version">>},
    Frame = {text, jiffy:encode(Body)},
    gun:ws_send(ConnPid, StreamRef, Frame),
    {ws, Ack} = gun:await(ConnPid, StreamRef),
    io:format("Ack ~p ~n", [Ack]),
    gun:close(ConnPid).

login(ChannelId, Ip, Port, UserName, Password) ->
    {ok, WsPid} = gun:open(Ip, Port, #{
        supervise => false,
        ws_opts => #{keepalive => 1000 * 20}
    }),
    case gun:await_up(WsPid) of
        {ok, _} ->
            StreamRef = gun:ws_upgrade(WsPid, "/rest/ws", []),
            case catch gun:await(WsPid, StreamRef) of
                {'EXIT', Reason} ->
                    {error, Reason};
                {upgrade, [<<"websocket">>], _} ->
                    connect(WsPid, StreamRef, ?Database(ChannelId), UserName, Password);
                Error1 ->
                    {error, Error1}
            end;
        {_, Error} ->
            {error, Error}
    end.

connect(WsPid, StreamRef, Db, UserName, Password) ->
    Version = #{<<"action">> => <<"conn">>,
        <<"args">> => #{
            <<"req_id">> => 0,
            <<"user">> => UserName,
            <<"password">> => Password,
            <<"db">> => Db
        }
    },
    put(req_id, 1),
    VersionFrame = {text, dgiot_json:encode(Version)},
    gun:ws_send(WsPid, StreamRef, VersionFrame),

    case gun:await(WsPid, StreamRef) of
        {ws, {text, Data}} ->
            case catch dgiot_json:decode(Data, [return_maps]) of
                #{<<"code">> := 0, <<"action">> := <<"conn">>} ->
                    {ok, {WsPid, StreamRef}};
                #{<<"code">> := 899, <<"action">> := <<"conn">>} ->
                    connect(WsPid, StreamRef, <<"">>, UserName, Password);
                _ ->
                    {error, Data}
            end;
        Error ->
            {error, Error}
    end.

check_version(WsPid, StreamRef) ->
    Version = #{<<"action">> => <<"version">>},
    VersionFrame = {text, jiffy:encode(Version)},
    gun:ws_send(WsPid, StreamRef, VersionFrame),
    case gun:await(WsPid, StreamRef) of
        {ws, #{<<"code">> := 0, <<"action">> := <<"version">>, <<"version">> := V}} ->
            {ok, #{<<"version">> => V}};
        {ws, Error} ->
            {error, Error}
    end.
