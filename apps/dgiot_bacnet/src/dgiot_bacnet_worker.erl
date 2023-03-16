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

-module(dgiot_bacnet_worker).
-author("johnliu").
-include_lib("dgiot/include/dgiot_client.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([init/1, handle_info/2, terminate/2]).
-export([childspec/2, start_connect/2]).

-define(MAX_BUFF_SIZE, 10 * 1024).

childspec(ChannelId, Args) ->
    dgiot_client:register(ChannelId, udp_client_sup, Args).

start_connect(ChannelId, #{
    <<"ip">> := Ip} = Opts) ->
    dgiot_client:start(ChannelId, dgiot_utils:to_binary(Ip), Opts#{<<"ip">> => Ip, <<"mod">> => ?MODULE, <<"child">> => #{}}).

init(#dclient{child = ChildState} = Dclient) when is_map(ChildState) ->

    {ok, Dclient};

init(_) ->
    {ok, #{}}.

handle_info(connection_ready, #dclient{child = ChildState} = Dclient) ->
%%    io:format("~s ~p ~p send from Dclient ~p ~n", [?FILE, ?LINE, self(), Dclient]),
    {noreply, Dclient#dclient{child = ChildState}};

handle_info(udp_closed, #dclient{child = ChildState} = Dclient) ->
    {noreply, Dclient#dclient{child = ChildState}};

handle_info({udp, Ip, Port, Buff}, Dclient) ->
    io:format("~s ~p ~p send from ~p:~p : ~p ~n", [?FILE, ?LINE, self(), dgiot_utils:get_ip(Ip), Port, dgiot_utils:to_hex(Buff)]),
    {noreply, Dclient};

handle_info({deliver, _, Msg}, #dclient{channel = ChannelId, client = ClientId} = Dclient) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    Topic = dgiot_mqtt:get_topic(Msg),
    case jsx:is_json(Payload) of
        true ->
            case binary:split(Topic, <<$/>>, [global, trim]) of
                [<<"$dg">>, <<"device">>, ProductId, _, <<"profile">>] ->
%%                    设置参数
                    _ProfilePayload = dgiot_device_profile:encode_profile(ProductId, jsx:decode(Payload)),
                    {noreply, Dclient};
                [<<"$dg">>, <<"device">>, _, _, <<"properties">>] ->
                    case jsx:decode(Payload, [{labels, binary}, return_maps]) of
                        #{<<"piid">> := _Piid, <<"siid">> := _Siid} = _DataSource ->
                            dgiot_udp_broadcast:send(ChannelId, ClientId, <<"Data">>),
                            {noreply, Dclient};
                        _ ->
                            {noreply, Dclient}
                    end;
                _Other ->
                    ?LOG(error, "_Other ~p", [_Other]),
                    {noreply, Dclient}
            end;
        false ->
            case binary:split(Topic, <<$/>>, [global, trim]) of
                [<<"$dg">>, <<"device">>, _ProductId, _, <<"profile">>] ->
                    %% 设置参数
                    {noreply, Dclient};
                _ ->
                    {noreply, Dclient}
            end
    end;

handle_info(_Info, Dclient) ->
    {noreply, Dclient}.

terminate(_Reason, _Dclient) ->
    ok.
