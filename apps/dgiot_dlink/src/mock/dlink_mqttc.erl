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
-module(dlink_mqttc).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").

-record(zeta, {tid}).

-export([childspec/2, start_client/2]).

%% API
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

childspec(ChannelId, #{<<"host">> := Host, <<"port">> := Port,
    <<"username">> := UserName, <<"password">> :=  Password}) ->
    Options = #{
        host => dgiot_utils:to_list(Host),
        port => Port,
        username => dgiot_utils:to_list(UserName),
        password => dgiot_utils:to_list(Password),
        proto_ver => v3,
        keepalive => 60,
        clean_start => true
    },
    Args = #{
        <<"channel">> => ChannelId,
        <<"mod">> => ?MODULE,
        <<"options">> => Options
    },
    dgiot_client:register(ChannelId, mqtt_client_sup, Args).


start_client(_ChannelId, 0) ->
    pass;
start_client(ChannelId, Count) ->
    ClientId = <<"">>,
    dgiot_client:start(ChannelId, ClientId),
    start_client(ChannelId, Count - 1).


%%  callback
init(#dclient{channel = ChannelId} = Dclient) ->
    <<_:11/binary, Tid/binary>> = dgiot_utils:to_binary(ChannelId),
    User = #zeta{tid = dgiot_utils:to_atom(Tid)},
    {ok, Dclient#dclient{channel = ChannelId, child = User}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({connect, Client}, #dclient{channel = ChannelId} = Dclient) ->
    erlang:send_after(2000, self(), {subscribe, Client}),
    dgiot_metrics:inc(dgiot_zeta, <<"mqtt_online">>, 1),
    update(ChannelId),
    {noreply, Dclient};

handle_info({subscribe, Client}, #dclient{client = ClientId, child = #zeta{tid = Tid}} = Dclient) ->
    io:format("~s ~p Tid: ~p Client ~p ~n", [?FILE, ?LINE, Tid, Client]),
    #{<<"apiKey">> := Key} = zeta_config:get_config(Tid, <<"zeta_auth">>),
    case re:split(ClientId, <<":">>) of
        [_, _, <<"001">>] ->
            Topic = <<Key/binary, "/v1/ms/#">>,
            ?LOG(info, "Topic ~p ", [Topic]),
            emqtt:subscribe(Client, {Topic, 1});
        [_, _, <<"002">>] ->
            Topic = <<Key/binary, "/v1/ap/#">>,
            ?LOG(info, "Topic ~p  ", [Topic]),
            emqtt:subscribe(Client, {Topic, 1});
        _ ->
            pass
    end,
    {noreply, Dclient};

handle_info(disconnect, Dclient) ->
    dgiot_metrics:dec(dgiot_zeta, <<"mqtt_online">>, 1),
    ?LOG(info, "disconnect ~p ", [Dclient]),
    {noreply, Dclient};

handle_info({publish, #{payload := _Payload} = _Msg}, Dclient) ->
    dgiot_metrics:inc(dgiot_zeta, <<"mqtt_recv">>, 1),
    {noreply, Dclient};

handle_info(_Info, Dclient) ->
%%    ?LOG(info,"ecapturer ~p~n", [_Info]),
    {noreply, Dclient}.

terminate(_Reason, #dclient{channel = ChannelId, client = ClientId}) ->
%%    ?LOG(info,"_Reason ~p~n", [_Reason]),
    dgiot_client:stop(ChannelId, ClientId),
    update(ChannelId),
    ok.

code_change(_OldVsn, Dclient, _Extra) ->
    {ok, Dclient}.

update(ChannelId) ->
    dgiot_data:insert({<<"mqtt_online">>, dlink_metrics}, dgiot_client:count(ChannelId)).