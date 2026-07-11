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
-module(dgiot_device_client).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").

-record(connect_state, {mod}).

%% gen_server callbacks
-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).


start_link(Args) ->
    dgiot_client:start_link(?MODULE, Args).

init([#{<<"channel">> := ChannelId, <<"client">> := ClientId, <<"mod">> := Mod} = Args]) ->
%%    io:format("~s ~p  Args ~p ~n", [?FILE, ?LINE, Args]),
    UserData = #connect_state{mod = Mod},
    ChildState = maps:get(<<"child">>, Args, #{}),
    StartTime = dgiot_client:get_time(maps:get(<<"starttime">>, Args, dgiot_datetime:now_secs())),
    EndTime = dgiot_client:get_time(maps:get(<<"endtime">>, Args, dgiot_datetime:now_secs() + 1000000000)),
    Freq = maps:get(<<"freq">>, Args, 30),
    Count = dgiot_client:get_count(StartTime, EndTime, Freq),
    Clock = #dclock{freq = Freq, nexttime = StartTime, count = Count, round = 0},
    Dclient = #dclient{channel = ChannelId, client = ClientId, status = ?DCLIENT_INTIALIZED, clock = Clock, userdata = UserData, child = ChildState},
    dgiot_client:add(ChannelId, ClientId),
    case Mod:init(Dclient) of
        {ok, NewDclient} ->
            {ok, NewDclient, hibernate};
        {stop, Reason} ->
            {stop, Reason}
    end.

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

handle_info(Info, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_info(Info, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, _Reason, NewDclient} ->
            timer:sleep(10),
            dgiot_client:stop(ChannelId, ClientId),
            {noreply, NewDclient, hibernate}
    end.

terminate(Reason, #dclient{userdata = #connect_state{mod = Mod}} = Dclient) ->
    Mod:terminate(Reason, Dclient).

code_change(OldVsn, #dclient{userdata = #connect_state{mod = Mod}} = Dclient, Extra) ->
    Mod:code_change(OldVsn, Dclient, Extra).
