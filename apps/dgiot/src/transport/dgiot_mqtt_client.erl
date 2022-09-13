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
-module(dgiot_mqtt_client).
-author("jonhliu").
-behaviour(gen_server).
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").

%% API
-export([start_link/1, subscribe/3, publish/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(connect_state, {options, socket = disconnect, props, mod}).

%%%===================================================================
%%% API
%%%===================================================================

subscribe(Client, Topic, Qos) ->
    gen_server:call(Client, {subscribe, Topic, Qos}).

publish(Client, Topic, Payload, Qos) ->
    gen_server:call(Client, {publish, Topic, Payload, Qos}).

start_link(Args) ->
    dgiot_client:start_link(?MODULE, Args).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([#{<<"channel">> := ChannelId, <<"client">> := ClientId, <<"mod">> := Mod, <<"options">> := Options} = Args]) ->
    UserData = #connect_state{mod = Mod, options = Options#{clientid => ClientId}},
    ChildState = maps:get(<<"child">>, Args, #{}),
    Dclient = #dclient{channel = ChannelId, client = ClientId, status = ?DCLIENT_INTIALIZED, userdata = UserData, child = ChildState},
    dgiot_client:add(ChannelId, ClientId),
    case Mod:init(Dclient) of
        {ok, NewDclient} ->
            process_flag(trap_exit, true),
            rand:seed(exs1024),
            Time = erlang:round(rand:uniform() * 60 * 3 + 1) * 1000,
            erlang:send_after(Time, self(), connect),
            {ok, NewDclient, hibernate};
        {stop, Reason} ->
            {stop, Reason}
    end.

handle_call({publish, Topic, Payload, Qos}, _From, #dclient{userdata = #connect_state{socket = ConnPid}} = Dclient) ->
    case ConnPid of
        disconnect ->
            {reply, {error, disconnect}, Dclient};
        Pid ->
            Reply = emqtt:publish(Pid, Topic, Payload, Qos),
            {reply, Reply, Dclient}
    end;

handle_call({subscribe, Topic, Qos}, _From, #dclient{userdata = #connect_state{socket = ConnPid}} = Dclient) ->
    case ConnPid of
        disconnect ->
            {reply, {error, disconnect}, Dclient};
        Pid ->
            Reply = emqtt:subscribe(Pid, {Topic, Qos}),
            {reply, Reply, Dclient}
    end;

handle_call(_Request, _From, Dclient) ->
    {reply, ok, Dclient}.

handle_cast(_Request, Dclient) ->
    {noreply, Dclient}.

handle_info(connect, #dclient{userdata = #connect_state{options = Options, mod = Mod} = ConnectStat} = Dclient) ->
    case connect(Options) of
        {ok, ConnPid, Props} ->
            case Mod:handle_info({connect, ConnPid}, Dclient#dclient{userdata = ConnectStat#connect_state{props = Props}}) of
                {noreply, NewDclient} ->
                    {noreply, NewDclient};
                {stop, Reason, NewDclient} ->
                    {stop, Reason, NewDclient}
            end;
        {error, unauthorized} ->
            ?LOG(warning, "connect ~p", [unauthorized]),
            {stop, normal, Dclient};
        {error, econnrefused} ->
            {noreply, Dclient#dclient{userdata = ConnectStat#connect_state{socket = disconnect, props = undefined}}};
        {error, Reason} ->
            ?LOG(warning, "connect error,~p", [Reason]),
            {noreply, Dclient#dclient{userdata = ConnectStat#connect_state{socket = disconnect, props = undefined}}}
    end;

handle_info({publish, Message}, #dclient{userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_info({publish, Message}, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient};
        {stop, Reason, NewDclient} ->
            {stop, Reason, NewDclient}
    end;

handle_info({disconnected, shutdown, tcp_closed}, Dclient) ->
    {noreply, Dclient#dclient{userdata = #connect_state{socket = disconnect}}};

handle_info({'EXIT', _Conn, Reason}, #dclient{userdata = #connect_state{mod = Mod} = ConnectState} = Dclient) ->
    case Mod:handle_info(disconnect, Dclient) of
        {noreply, NewDclient} ->
            Sec = application:get_env(dgiot, reconnect, 10),
            case erlang:system_time(second) - get(last) > Sec of
                true ->
                    self() ! connect;
                false ->
                    erlang:send_after(Sec * 1000, self(), connect)
            end,
            {noreply, NewDclient#dclient{userdata = ConnectState#connect_state{socket = disconnect}}};
        {stop, Reason, NewDclient} ->
            {stop, Reason, NewDclient#dclient{userdata = ConnectState#connect_state{socket = disconnect}}}
    end;

handle_info(Info, #dclient{userdata = #connect_state{mod = Mod} = ConnectState} = Dclient) ->
    case Mod:handle_info(Info, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient};
        {stop, Reason, NewDclient} ->
            {stop, Reason, NewDclient#dclient{userdata = ConnectState#connect_state{socket = disconnect}}}
    end.

terminate(Reason, #dclient{userdata = #connect_state{mod = Mod}} = Dclient) ->
    Mod:terminate(Reason, Dclient).

code_change(OldVsn, #dclient{userdata = #connect_state{mod = Mod}} = Dclient, Extra) ->
    Mod:code_change(OldVsn, Dclient, Extra).


%%%===================================================================
%%% Internal functions
%%%===================================================================
connect(Options) when is_map(Options) ->
    connect(maps:to_list(Options));
connect(Options) ->
    put(last, erlang:system_time(second)),
    case emqtt:start_link([{owner, self()} | Options]) of
        {ok, ConnPid} ->
            case catch emqtt:connect(ConnPid) of
                {ok, Props} ->
                    {ok, ConnPid, Props};
                {error, {unauthorized_client, _}} ->
                    {error, unauthorized};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
