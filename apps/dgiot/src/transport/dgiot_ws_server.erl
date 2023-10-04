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

-module(dgiot_ws_server).
-author("johnliu").
-include("dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([start/1]).

start(Prot) ->
    F = fun interact/2,
    spawn(fun() -> start(Prot, F, 0) end).

start(Prot, F, State0) ->
    {ok, Listen} = gen_tcp:listen(Prot, [{packet, raw}, {reuseaddr, true}, {active, true}]),
    par_connect(Prot, Listen, F, State0).

par_connect(Prot, Listen, F, State0) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            spawn(fun() -> par_connect(Prot, Listen, F, State0) end),
            wait(Socket, F, State0);
        {error, closed} ->
            {ok, Listen2} = gen_tcp:listen(Prot, [{packet, raw}, {reuseaddr, true}, {active, true}]),
            par_connect(Prot, Listen2, F, State0)
    end.

wait(Socket, F, State0) ->
    receive
        {tcp, Socket, Data} ->
            Key = list_to_binary(lists:last(string:tokens(hd(lists:filter(fun(S) ->
                lists:prefix("Sec-WebSocket-Key:", S) end, string:tokens(Data, "\r\n"))), ": "))),
            Challenge = base64:encode(crypto:hash(sha, <<Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)),
            Handshake =
                ["HTTP/1.1 101 Switching Protocols\r\n",
                    "connection: Upgrade\r\n",
                    "upgrade: websocket\r\n",
                    "sec-websocket-accept: ", Challenge, "\r\n",
                    "\r\n", <<>>],
            gen_tcp:send(Socket, Handshake),
            send_data(Socket, "Hello, my world"),
            S = self(),
            Pid = spawn_link(fun() -> F(S, State0) end),
            loop(Socket, Pid);
        _Any ->
            wait(Socket, F, State0)
    end.

loop(Socket, Pid) ->
    receive
        {tcp, Socket, Data} ->
            Text = websocket_data(Data),
            case Text =/= <<>> of
                true ->
                    Pid ! {browser, self(), ["You said: ", Text]};
                false ->
                    ok
            end,
            loop(Socket, Pid);
        {tcp_closed, Socket} ->
            ok;
        {send, Data} ->
            send_data(Socket, Data),
            loop(Socket, Pid);
        _Any ->
            loop(Socket, Pid)
    end.

interact(Browser, State) ->
    receive
        {browser, Browser, Str} ->
            Browser ! {send, Str},
            interact(Browser, State)
    after 1000 ->
        Browser ! {send, "clock ! tick " ++ integer_to_list(State)},
        interact(Browser, State + 1)
    end.

%% 仅处理长度为125以内的文本消息
websocket_data(Data) when is_list(Data) ->
    websocket_data(list_to_binary(Data));
websocket_data(<<1:1, 0:3, 1:4, 1:1, Len:7, MaskKey:32, Rest/bits>>) when Len < 126 ->
    <<End:Len/binary, _/bits>> = Rest,
    Text = websocket_unmask(End, MaskKey, <<>>),
    Text;
websocket_data(_) ->
    <<>>.

%% 由于Browser发过来的数据都是mask的,所以需要unmask
websocket_unmask(<<>>, _, Unmasked) ->
    Unmasked;
websocket_unmask(<<O:32, Rest/bits>>, MaskKey, Acc) ->
    T = O bxor MaskKey,
    websocket_unmask(Rest, MaskKey, <<Acc/binary, T:32>>);
websocket_unmask(<<O:24>>, MaskKey, Acc) ->
    <<MaskKey2:24, _:8>> = <<MaskKey:32>>,
    T = O bxor MaskKey2,
    <<Acc/binary, T:24>>;
websocket_unmask(<<O:16>>, MaskKey, Acc) ->
    <<MaskKey2:16, _:16>> = <<MaskKey:32>>,
    T = O bxor MaskKey2,
    <<Acc/binary, T:16>>;
websocket_unmask(<<O:8>>, MaskKey, Acc) ->
    <<MaskKey2:8, _:24>> = <<MaskKey:32>>,
    T = O bxor MaskKey2,
    <<Acc/binary, T:8>>.

%% 发送文本给Client
send_data(Socket, Payload) ->
    Len = iolist_size(Payload),
    BinLen = payload_length_to_binary(Len),
    gen_tcp:send(Socket, [<<1:1, 0:3, 1:4, 0:1, BinLen/bits>>, Payload]).

payload_length_to_binary(N) ->
    case N of
        N when N =< 125 -> <<N:7>>;
        N when N =< 16#ffff -> <<126:7, N:16>>;
        N when N =< 16#7fffffffffffffff -> <<127:7, N:64>>
    end.
