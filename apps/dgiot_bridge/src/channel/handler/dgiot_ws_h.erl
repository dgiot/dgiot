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
-module(dgiot_ws_h).
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).
-export([run_hook/2]).


%%GET /websocket/test HTTP/1.1
%%Host: 127.0.0.1:9082
%%Connection: Upgrade
%%Pragma: no-cache
%%Cache-Control: no-cache
%%User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.127 Safari/537.36
%%Upgrade: websocket
%%Origin: http://127.0.0.1:3080
%%Sec-WebSocket-Version: 13
%%Accept-Encoding: gzip, deflate, br
%%Accept-Language: zh-CN,zh;q=0.9
%%Sec-WebSocket-Key: D7JD3d7II0KEJKvb4qCXcQ==
%%Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits
%%Sec-WebSocket-Protocol: null

init(Req0, Opts) ->
    case cowboy_websocket:is_upgrade_request(Req0) of
        true ->
            io:format("~s ~p ~p ~n", [?FILE, ?LINE, <<"1">>]),
            cowboy_req:headers(Req0),
            cowboy_websocket:upgrade(Req0, Opts, cowboy_websocket, #{});
        _ ->
            io:format("~s ~p ~p ~n", [?FILE, ?LINE, <<"2">>]),
            {cowboy_websocket, Req0, Opts}
    end.

websocket_init([Req, Opts]) ->
    Path = cowboy_req:path(Req),
    add_hook(Path),
    erlang:start_timer(1000, self(), <<"Hello!">>),
    {[], Opts}.

websocket_handle({text, Msg}, State) ->
    {[{text, <<"That's what she said! ", Msg/binary>>}], State};
websocket_handle(_Data, State) ->
    {[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
%%    erlang:start_timer(1000, self(), <<"How' you doin'?">>),
    {[{text, Msg}], State};
websocket_info({http2ws, Data}, State) ->
    io:format("~s ~p ~p ~n", [?FILE, ?LINE, byte_size(Data)]),
    {[{binary, Data}], State};
websocket_info(_Info, State) ->
    {[], State}.

add_hook(Path) ->
    case re:split(Path, <<"/">>) of
        [<<>>, <<"websocket">>, Rest] ->
            Key = <<"/", Rest/binary>>,
            case dgiot_data:get({http2ws, Key}) of
                not_find ->
                    dgiot_data:insert({http2ws, Key}, [self()]);
                Pids ->
                    NewPids =
                        lists:foldl(fun(Pid, Acc) ->
                            case is_process_alive(Pid) of
                                true ->
                                    Acc ++ [Pid];
                                _ ->
                                    Acc
                            end
                                    end, [self()], Pids),
                    dgiot_data:insert({http2ws, Key}, dgiot_utils:unique_1(NewPids))
            end;
        _ ->
            pass
    end.

terminate(_Reason, _Req, _UnExpectedState) ->
    ok.

run_hook(Key, Data) ->
    case dgiot_data:get({http2ws, Key}) of
        not_find ->
            pass;
        Pids ->
            lists:map(fun(Pid) ->
                case is_process_alive(Pid) of
                    true ->
                        Pid ! {http2ws, Data};
                    _ ->
                        pass
                end
                      end, Pids)
    end.

%%check_update(Req0, Opts) ->
%%    case cowboy_websocket:is_upgrade_request(Req0) of
%%        true ->
%%            io:format("~s ~p ~p ~n", [?FILE, ?LINE, <<"1">>]),
%%            cowboy_req:headers(Req0),
%%            cowboy_websocket:upgrade(Req0, Opts, cowboy_websocket, #{});
%%        _ ->
%%            io:format("~s ~p ~p ~n", [?FILE, ?LINE, <<"2">>]),
%%            {cowboy_websocket, Req0, Opts}
%%    end.
%%handshake(Data) ->
%%Key = list_to_binary(lists:last(string:tokens(hd(lists:filter(fun(S) -> lists:prefix("Sec-WebSocket-Key:", S) end, string:tokens(Data, "\r\n"))), ": "))),
%%Challenge = base64:encode(crypto:sha(<< Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>)),
%%["HTTP/1.1 101 Switching Protocols\r\n",
%%"connection: Upgrade\r\n",
%%"upgrade: websocket\r\n",
%%"sec-websocket-accept: ", Challenge, "\r\n",
%%"\r\n",<<>>].
%%
%%http11_keepalive(Config) ->
%%    {ok, ConnPid} = gun:open("localhost", config(port, Config), #{
%%        ws_opts => #{
%%            keepalive => 100,
%%            silence_pings => false
%%        }
%%    }),
%%    {ok, _} = gun:await_up(ConnPid),
%%    StreamRef = gun:ws_upgrade(ConnPid, "/", []),
%%    {upgrade, [<<"websocket">>], _} = gun:await(ConnPid, StreamRef),
%%    %% Gun sent a ping automatically, we therefore receive a pong.
%%    {ws, pong} = gun:await(ConnPid, StreamRef),
%%    gun:close(ConnPid).