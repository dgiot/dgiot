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

-module(dgiot_tcp_client).
-author("johnliu").
-include("dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").
-behaviour(gen_server).
%% API
-export([start_link/1, send/1, send/2, send/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(connect_state, {host, port, mod, socket = undefined, freq = 30, count = 1000}).
-define(TIMEOUT, 10000).
-define(TCP_OPTIONS, [binary, {active, once}, {packet, raw}, {reuseaddr, false}, {send_timeout, ?TIMEOUT}]).

start_link(Args) ->
    dgiot_client:start_link(?MODULE, Args).

init([#{<<"channel">> := ChannelId, <<"client">> := ClientId, <<"ip">> := Host, <<"port">> := Port, <<"mod">> := Mod} = Args]) ->
    Ip = dgiot_utils:to_list(Host),
    Port1 = dgiot_utils:to_int(Port),
    UserData = #connect_state{mod = Mod, host = Ip, port = Port1, freq = 30, count = 300},
    ChildState = maps:get(<<"child">>, Args, #{}),
    StartTime = dgiot_client:get_time(maps:get(<<"starttime">>, Args, dgiot_datetime:now_secs())),
    EndTime = dgiot_client:get_time(maps:get(<<"endtime">>, Args, dgiot_datetime:now_secs() + 1000000000)),
    Freq = maps:get(<<"freq">>, Args, 30),
    NextTime = dgiot_client:get_nexttime(StartTime, Freq),
    Count = dgiot_client:get_count(StartTime, EndTime, Freq),
    Rand =
        case maps:get(<<"rand">>, Args, true) of
            true -> 0;
            _ -> dgiot_client:get_rand(Freq)
        end,
    Clock = #dclock{freq = Freq, nexttime = NextTime + Rand, count = Count, round = 0},
    Dclient = #dclient{channel = ChannelId, client = ClientId, status = ?DCLIENT_INTIALIZED, clock = Clock, userdata = UserData, child = ChildState},
    dgiot_client:add(ChannelId, ClientId),
    case Mod:init(Dclient) of
        {ok, NewDclient} ->
            do_connect(NewDclient),
            {ok, NewDclient, hibernate};
        {stop, Reason} ->
            {stop, Reason}
    end.

handle_call({connection_ready, Socket}, _From, #dclient{ channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod} = UserData} = Dclient) ->
    NewUserData = UserData#connect_state{socket = Socket},
    case Mod:handle_info(connection_ready, Dclient#dclient{userdata = NewUserData}) of
        {noreply, NewDclient} ->
            {reply, ok, NewDclient, hibernate};
        {stop, _Reason, NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),
            {reply, _Reason, NewDclient}
    end;

handle_call(Request, From, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_call(Request, From, Dclient) of
        {reply, Reply, NewDclient} ->
            {reply, Reply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),
            {reply, Reason, NewDclient}
    end.

handle_cast(Msg, #dclient{channel = ChannelId, client = ClientId,
    userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_cast(Msg, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),
            {reply, Reason, NewDclient}
    end.

%% 连接次数为0了
handle_info(do_connect, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{count = Count}} = Dclient) when Count =< 0 ->
    dgiot_client:stop(ChannelId, ClientId),
    {noreply, Dclient, hibernate};
handle_info(do_connect, #dclient{userdata = #connect_state{count = Count, freq = Freq} = UserData} = Dclient) ->
    timer:sleep(Freq * 1000),
    NewDclient = Dclient#dclient{userdata = UserData#connect_state{count = Count - 1}},
    do_connect(NewDclient),
    {noreply, NewDclient, hibernate};

handle_info({connection_ready, Socket}, #dclient{userdata = #connect_state{mod = Mod} = UserData} = Dclient) ->
    case Mod:handle_info(connection_ready, Dclient#dclient{userdata = UserData#connect_state{socket = Socket}}) of
        {noreply, NewDclient} ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            {stop, Reason, NewDclient}
    end;

%% 往tcp server 发送报文
handle_info({send, _PayLoad}, #dclient{userdata = #connect_state{socket = undefined}} = Dclient) ->
    {noreply, Dclient, hibernate};
handle_info({send, PayLoad}, #dclient{userdata = #connect_state{host = _Ip, port = _Port, socket = Socket}} = Dclient) ->
%%    io:format("~s ~p send to from ~p:~p : ~p ~n", [?FILE, ?LINE,  _Ip, _Port, dgiot_utils:to_hex(PayLoad)]),
    gen_tcp:send(Socket, PayLoad),
    {noreply, Dclient, hibernate};

%% 接收tcp server发送过来的报文
handle_info({tcp, Socket, Binary}, #dclient{userdata = #connect_state{host = _Ip, port = _Port, mod = Mod}} = Dclient) ->
%%    io:format("~s ~p recv from ~p:~p ~p ~n", [?FILE, ?LINE, _Ip, _Port, dgiot_utils:to_hex(Binary)]),
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);
            _ ->
                Binary
        end,
    case Mod:handle_info({tcp, NewBin}, Dclient) of
        {noreply, NewDclient} ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, NewDclient, hibernate};
        {stop, Reason, #dclient{channel = ChannelId, client = ClientId} = NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),
            {noreply, Reason, NewDclient, hibernate}
    end;

handle_info({tcp_error, _Socket, _Reason}, Dclient) ->
    {noreply, Dclient, hibernate};

handle_info({tcp_closed, Socket}, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod}} = Dclient) ->
    gen_tcp:close(Socket),
    case Mod:handle_info(tcp_closed, Dclient) of
        {noreply, NewDclient} ->
            self() ! do_connect,
            {noreply, NewDclient, hibernate};
        {stop, _Reason, NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),
            {noreply, NewDclient, hibernate}
    end;

handle_info(Info, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod, socket = Socket}} = Dclient) ->
    case Mod:handle_info(Info, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, _Reason, NewDclient} ->
            gen_tcp:close(Socket),
            timer:sleep(10),
            dgiot_client:stop(ChannelId, ClientId),
            {noreply, NewDclient, hibernate}
    end.

terminate(Reason, #dclient{userdata = #connect_state{mod = Mod}} = Dclient) ->
    Mod:terminate(Reason, Dclient).

code_change(OldVsn, #dclient{userdata = #connect_state{mod = Mod}} = Dclient, Extra) ->
    Mod:code_change(OldVsn, Dclient, Extra).


%%%===================================================================
%%% Internal functions
%%%===================================================================
send(Socket, Payload) ->
    gen_tcp:send(Socket, Payload).

send(Payload) ->
    self() ! {send, Payload}.

send(ChannelId, ClientId, Payload) ->
    case dgiot_client:get(ChannelId, ClientId) of
        {ok, Pid} ->
            Pid ! {send, Payload};
        _ ->
            pass
    end.

do_connect(#dclient{userdata = #connect_state{host = Host, port = Port}}) ->
    case gen_tcp:connect(Host, Port, ?TCP_OPTIONS, ?TIMEOUT) of
        {ok, Socket} ->
            case catch gen_server:call(self(), {connection_ready, Socket}, 5000) of
                ok ->
                    inet:setopts(Socket, [{active, once}]),
                    gen_tcp:controlling_process(Socket, self());
                _ ->
                    ok
            end;
        {error, _Reason} ->
            self() ! do_connect
    end;

do_connect(_) ->
    self() ! do_connect.
