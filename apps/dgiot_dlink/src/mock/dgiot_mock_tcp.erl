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
-module(dgiot_mock_tcp).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").

-export([childspec/2, start/3]).

%% API
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).


start(ChannelId, DeviceId,  Mock) ->
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"devaddr">> := DevAddr, <<"productid">> := ProductId}} ->
            dgiot_client:start(ChannelId, <<ProductId/binary, "_", DevAddr/binary>>, Mock#{ <<"child">> => Mock});
        _ ->
            #{}
    end.

childspec(ChannelId, ChannelArgs) ->
    Args = #{
        <<"channel">> => ChannelId,
        <<"mod">> => ?MODULE,
        <<"ip">> => maps:get(<<"address">>, ChannelArgs, <<"127.0.0.1">>),
        <<"port">> => maps:get(<<"port">>, ChannelArgs, 1883)
    },
    dgiot_client:register(ChannelId, tcp_client_sup, Args).

%%  callback
init(#dclient{channel = ChannelId} = State) ->
    {ok, State#dclient{channel = dgiot_utils:to_binary(ChannelId)}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({connection_ready, _Socket}, Dclient) ->
    {noreply, Dclient};

%% 接收tcp server发送过来的报文
handle_info({tcp, _Binary}, Dclient) ->
%%    dgiot_tcp_client:send(ChannelId, ClientId, Payload)
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
    dgiot_data:insert({<<"tcp_online">>, dlink_metrics}, dgiot_client:count(ChannelId)).
