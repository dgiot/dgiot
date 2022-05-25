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
-export([start_link/1, send/1, send/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(connect_state, {host, port, mod, socket = undefined}).
-define(TIMEOUT, 10000).
-define(TCP_OPTIONS, [binary, {active, once}, {packet, raw}, {reuseaddr, false}, {send_timeout, ?TIMEOUT}]).

start_link(Args) ->
    dgiot_client:start_link(?MODULE, Args).

init([#{<<"channel">> := ChannelId, <<"client">> := ClientId, <<"ip">> := Host, <<"port">> := Port, <<"mod">> := Mod} = Args]) ->
    Ip = dgiot_utils:to_list(Host),
    Port1 = dgiot_utils:to_int(Port),
    Count = maps:get(<<"count">>, Args, max),
    Freq = maps:get(<<"freq">>, Args, 30),
    UserData = #connect_state{mod = Mod, host = Ip, port = Port1},
    ChildState = maps:get(<<"child">>, Args, 30),
    Clock = #dclock{freq = Freq, count = Count, round = 0},
    Dclient = #dclient{channel = ChannelId, client = ClientId, status = ?DCLIENT_INTIALIZED, clock = Clock, userdata = UserData, child = ChildState},
    dgiot_client:add(ChannelId, ClientId),
    case Mod:init(Dclient) of
        {ok, NewChildState} ->
            do_connect(Dclient#dclient{child = NewChildState}),
            {ok, Dclient#dclient{child = NewChildState}, hibernate};
        {stop, Reason} ->
            {stop, Reason}
    end.

handle_call({connection_ready, Socket}, _From, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod} = UserData} = Dclient) ->
    NewUserData = UserData#connect_state{socket = Socket},
    case Mod:handle_info(connection_ready, Dclient) of
        {noreply, NewChildState} ->
            {reply, ok, Dclient#dclient{child = NewChildState, userdata = NewUserData}, hibernate};
        {stop, _Reason, NewChildState} ->
            dgiot_client:stop(ChannelId, ClientId),
            {reply, _Reason, Dclient#dclient{child = NewChildState, userdata = NewUserData}}
    end;

handle_call(Request, From, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_call(Request, From, Dclient) of
        {reply, Reply, NewChildState} ->
            {reply, Reply, Dclient#dclient{child = NewChildState}, hibernate};
        {stop, Reason, NewChildState} ->
            dgiot_client:stop(ChannelId, ClientId),
            {reply, Reason, Dclient#dclient{child = NewChildState}}
    end.

handle_cast(Msg, #dclient{channel = ChannelId, client = ClientId,
    userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_cast(Msg, Dclient) of
        {noreply, NewChildState} ->
            {noreply, Dclient#dclient{child = NewChildState}, hibernate};
        {stop, Reason, NewChildState} ->
            dgiot_client:stop(ChannelId, ClientId),
            {reply, Reason, Dclient#dclient{child = NewChildState}}
    end.

%% 连接次数为0了
handle_info(do_connect, #dclient{channel = ChannelId, client = ClientId, clock = #dclock{count = Count}} = State) when Count =< 0 ->
    dgiot_client:stop(ChannelId, ClientId),
    {noreply, State, hibernate};
handle_info(do_connect, #dclient{clock = #dclock{count = Count, round = Round, freq = Freq} = Clock} = State) ->
    timer:sleep(Freq * 1000),
    NewState = State#dclient{clock = Clock#dclock{count = Count - 1, round = Round + 1}},
    do_connect(NewState),
%%    io:format("~s ~p ~p ~n", [?FILE, ?LINE, Count]),
    {noreply, NewState, hibernate};

handle_info({connection_ready, Socket}, #dclient{userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_info(connection_ready, Dclient) of
        {noreply, ChildState} ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, Dclient#dclient{child = ChildState}, hibernate};
        {stop, Reason, ChildState} ->
            {stop, Reason, Dclient#dclient{child = ChildState}}
    end;

%% 往tcp server 发送报文
handle_info({send, _PayLoad}, #dclient{userdata = #connect_state{socket = undefined}} = State) ->
    {noreply, State, hibernate};
handle_info({send, PayLoad}, #dclient{userdata = #connect_state{socket = Socket}} = State) ->
    gen_tcp:send(Socket, PayLoad),
    {noreply, State, hibernate};

%% 接收tcp server发送过来的报文
handle_info({tcp, Socket, Binary}, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod, socket = Socket}} = Dclient) ->
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);
            _ ->
                Binary
        end,
    case Mod:handle_info({tcp, NewBin}, Dclient) of
        {noreply, ChildState} ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, Dclient#dclient{child = ChildState}, hibernate};
        {stop, Reason, ChildState} ->
            dgiot_client:stop(ChannelId, ClientId),
            {noreply, Reason, Dclient#dclient{child = ChildState}, hibernate}
    end;

handle_info({tcp_error, _Socket, _Reason}, State) ->
    {noreply, State, hibernate};

handle_info({tcp_closed, _Sock}, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod, socket = Socket}} = Dclient) ->
    gen_tcp:close(Socket),
    case Mod:handle_info(tcp_closed, Dclient) of
        {noreply, ChildState} ->
            self() ! do_connect,
            {noreply,  Dclient#dclient{child = ChildState}, hibernate};
        {stop, _Reason, ChildState} ->
            dgiot_client:stop(ChannelId, ClientId),
            {noreply, Dclient#dclient{child = ChildState}, hibernate}
    end;

handle_info(Info, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod, socket = Socket}} = Dclient) ->
    case Mod:handle_info(Info, Dclient) of
        {noreply, ChildState} ->
            {noreply, Dclient#dclient{child = ChildState}, hibernate};
        {stop, _Reason, ChildState} ->
            gen_tcp:close(Socket),
            dgiot_client:stop(ChannelId, ClientId),
            {noreply, Dclient#dclient{child = ChildState}, hibernate}
    end.

terminate(Reason, #dclient{userdata = #connect_state{mod = Mod}} = Dclient) ->
    Mod:terminate(Reason, Dclient).

code_change(OldVsn, #dclient{userdata = #connect_state{mod = Mod}, child = ChildState} = Dclient, Extra) ->
    {ok, ChildState} = Mod:code_change(OldVsn, Dclient, Extra),
    {ok, Dclient#dclient{child = ChildState}}.



%%%===================================================================
%%% Internal functions
%%%===================================================================
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

