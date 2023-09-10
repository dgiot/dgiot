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
-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).
-export([run_hook/2]).

%%%%GET /websocket/test HTTP/1.1
%%%%Host: 127.0.0.1:9082
%%%%Connection: Upgrade
%%%%Pragma: no-cache
%%%%Cache-Control: no-cache
%%%%User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.127 Safari/537.36
%%%%Upgrade: websocket
%%%%Origin: http://127.0.0.1:3080
%%%%Sec-WebSocket-Version: 13
%%%%Accept-Encoding: gzip, deflate, br
%%%%Accept-Language: zh-CN,zh;q=0.9
%%%%Sec-WebSocket-Key: D7JD3d7II0KEJKvb4qCXcQ==
%%%%Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits
%%%%Sec-WebSocket-Protocol: null
%%
init(Req, State) ->
    Path = cowboy_req:path(Req),
    add_hook(Path),
%%    check_update(Req, State),
    {cowboy_websocket, Req, State}.  %Perform websocket setup


websocket_init(State) ->
    io:format("~s ~p State ~p ~n", [?FILE, ?LINE, State]),
    {ok, State}.

websocket_handle(ping, State) ->
    io:format("~s ~p ~p ~p ~p ~n", [?FILE, ?LINE, ping, cowboy_clock:rfc1123(), State]),
    case maps:find(error, State) of
        {ok, _Code} ->
            {stop, State};
        error ->
            {reply, pong, State, hibernate}
    end;
websocket_handle({text, <<"ping">>}, State) ->
    io:format("~s ~p ~p ~p ~p ~n", [?FILE, ?LINE, <<"ping">>, cowboy_clock:rfc1123(), State]),
    case maps:find(error, State) of
        {ok, _Code} ->
            {stop, State};
        error ->
            {reply, {text, <<"pong2">>}, State, hibernate}
    end;
websocket_handle({text, Msg}, State) ->
    io:format("~s ~p Msg ~p ~n", [?FILE, ?LINE, Msg]),
    case dgiot_json:safe_decode(dgiot_utils:to_binary(Msg), [return_maps]) of
        {ok,#{<<"name">> := _Name} = Json} ->
            io:format("~s ~p ~p ~n",[?FILE,?LINE, Json]),
            {[{text, dgiot_json:encode(#{<<"name">> => <<"dgiotgood">>})}], State};
        _ ->
            {[{text, dgiot_json:encode(#{<<"name">> => <<"dgiotbad">>})}], State}
    end;

websocket_handle(_Data, State) ->
    io:format("~s ~p _Data ~p ~n", [?FILE, ?LINE, _Data]),
    {[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
    io:format("~s ~p  ~p ~p ~p ~p ~p  ~n", [?FILE, ?LINE, timeout, cowboy_clock:rfc1123(), _Ref, Msg, State]),
    {reply, {text, Msg}, State, hibernate};
websocket_info({close, CloseCode, Reason}, State) ->
    io:format("~s ~p  ~p ~p ~p ~p  ~n", [?FILE, ?LINE,close, CloseCode, Reason, State]),
    {reply, {close, CloseCode, Reason}, State};
websocket_info(stop, State) ->
%%    ?LOG([stop, State]),
    {stop, State};
websocket_info({http2ws, Data}, State) ->
    io:format("~s ~p ~p ~n", [?FILE, ?LINE, byte_size(Data)]),
    {[{binary, Data}], State};
websocket_info(_Info, State) ->
    {[], State}.


%% 断开socket onclose
%% Rename websocket_terminate/3 to terminate/3
%% link: https://github.com/ninenines/cowboy/issues/787
terminate(Reason, _Req, State) ->
    io:format("~s ~p ~p ~p ~p ~n", [?FILE, ?LINE, cowboy_clock:rfc1123(), State, Reason]),
    ok.

%%init(Req0, State0) ->
%%    DID = cowboy_req:header(<<"did">>, Req0, undefined),
%%    DType = cowboy_req:header(<<"cos">>, Req0, undefined),
%%%%    Auth = cowboy_req:header(<<"authorization">>, Req0, undefined),
%%    % [<<"sip">>,<<"text">>] = Subprotocols
%%%%    SubPt = cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0),
%%    Opt0 = #{
%%        num_acceptors => infinity,
%%        max_connections => infinity,
%%        max_frame_size => 1048576,  % 1MB
%%        % Cowboy关闭连接空闲120秒 默认值为 60000
%%        idle_timeout => 120000
%%    },
%%    case cowboy_websocket:is_upgrade_request(Req0) of
%%        true ->
%%            cowboy_req:headers(Req0),
%%            cowboy_websocket:upgrade(Req0, State0, cowboy_websocket, Opt0);
%%        _ ->
%%            pass
%%    end,
%%    State1 = State0#{
%%        dtype => DType,
%%        did => DID
%%    },
%%    Req1 = cowboy_req:reply(412, Req0),
%%    case throttle:check(throttle_ws, DID) of
%%        {limit_exceeded, _, _} ->
%%            % 429 Too Many Requests
%%            Req = cowboy_req:reply(429, Req0),
%%            {ok, Req, State0};
%%        _ ->
%%%%            % ?LOG([SubPt]),
%%%%            case websocket_ds:check_subprotocols(SubPt, Req0) of
%%%%                {ok, Req1} ->
%%                    {ok, Req1, State1}
%%%%                {cowboy_websocket, Req1} ->
%%%%                    % ?LOG([State1]),
%%%%                    websocket_ds:auth(Auth, Req1, State1, Opt0)
%%%%            end
%%    end.

%%websocket_init([Req, Opts]) ->
%%    io:format("~s ~p 22 ~p ~n", [?FILE, ?LINE, Req]),
%%    Path = cowboy_req:path(Req),
%%    add_hook(Path),
%%    erlang:start_timer(1000, self(), <<"Hello!">>),
%%    {[], Opts}.
%%

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

%%
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
