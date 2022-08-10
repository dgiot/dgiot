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

-module(dgiot_tcp2dlink_worker).
-author("kenneth").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").

-define(TYPE, <<"TCP2DLINK">>).
-define(MAX_BUFF_SIZE, 1024).
-define(PUMPTATUS, pumpstatus).
-define(AREA(R),3.14*R*R).
-record(state, {
    id,
    productId,
    is_sub
}).


%% TCP callback
-export([child_spec/2, init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).


child_spec(Port, State) ->
    dgiot_tcp_server:child_spec(?MODULE, Port, State).

%% =======================
%% {ok, State} | {stop, Reason}
init(#tcp{state = #state{id = ChannelId} = State} = TCPState) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, ?TYPE, ProductId} ->
%%                do_cmd(ProductId, connection_ready, <<>>, TCPState),
            NewState = State#state{productId = ProductId},
            {ok, TCPState#tcp{state = NewState}};
        {error, not_find} ->
            {error, not_find_channel}
    end.


%% task2device 下行
handle_info({deliver, _, Msg}, TCPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    Topic = dgiot_mqtt:get_topic(Msg),
    case binary:split(Topic, <<$/>>, [global, trim]) of
        [<<"thing">>, ProductId, DevAddr, <<"tcp">>, <<"hex">>] ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            dgiot_device:save_log(DeviceId, Payload, ['tcp_send']),
            dgiot_tcp_server:send(TCPState, dgiot_utils:hex_to_binary(dgiot_utils:trim_string(Payload))),
            {noreply, TCPState};
        _ ->
            case jsx:is_json(Payload) of
                true ->
                    case jsx:decode(Payload, [{labels, binary}, return_maps]) of
                        #{<<"cmd">> := <<"send">>} = Cmd ->
                            handle_info(Cmd, TCPState);
                        Info ->
                            handle_info(Info, TCPState)
                    end;
                false ->
                    {noreply, TCPState}
            end
    end;

% device2task 上行
handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, productId = ProductIds, is_sub = Is_sub} = State} = TCPState) ->
%%    dgiot_bridge:send_log(ChannelId, "received: ~ts", [dgiot_utils:to_list(Buff)]),
    Map = case jsx:is_json(Buff) of
              true ->
                  NewBuff = dgiot_utils:to_utf8(Buff,"GB18030"),
                  jsx:decode(NewBuff, [{labels, binary}, return_maps]);
              _ ->
                  Buff
          end,

    ProductId = lists:nth(1, ProductIds),
    dgiot_bridge:send_log(ChannelId, ProductId, " Recv ~ts", [jsx:encode(Map)]),
    case maps:size(Map) of
        40 ->
            handle_baseinfo(ChannelId, ProductId, Map),
            {noreply, TCPState};
        1 ->
            handle_status(ChannelId, ProductId, Map),
            {noreply, TCPState};
        11 ->
            handle_data(ChannelId, ProductId, Map),
            DevAddr = maps:get(<<"devaddr">>, Map),
            NewState = case Is_sub of
                           0 ->
                               dgiot_mqtt:subscribe(<<"$dg/device/", ProductId/binary, "/", DevAddr/binary, "/#">>),
                               State#state{is_sub = 1};
                           _ ->

                               State
                       end,
            {noreply, TCPState#tcp{state = NewState}};
        _ ->
            error
    end;


%% {stop, TCPState} | {stop, Reason} | {ok, TCPState} | ok | stop
handle_info(_Info, TCPState) ->
    {noreply, TCPState}.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, #tcp{state = #state{productId = _ProductId}} = _TCPState) ->
    ok;

terminate(_Reason, _TCPState) ->
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.

handle_baseinfo(ChannelId, ProductId, #{<<"Dat 39">> := Devaddr} = Map) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, Devaddr),
    Trans = #{<<"Dat 1">> => <<"QG">>,
        <<"Dat 2">> => <<"HG">>,
        <<"Dat 3">> => <<"PG">>,
        <<"Dat 4">> => <<"XG">>,
        <<"Dat 5">>=><<"npsh">>,
        <<"Dat 6">> => <<"im_area">>,
        <<"Dat 7">> => <<"out_area">>,
        <<"Dat 10">> => <<"NG">>,
        <<"Dat 11">> => <<"dgiot_testing_equipment_pressure">>,
        <<"Dat 12">> => <<"dgiot_testing_equipment_electricity">>,
        <<"Dat 13">> =><<"electnum">>,
        <<"Dat 14">> =><<"normal_fre">>,
        <<"Dat 15">>=><<"phase">>,
        <<"Dat 20">> => <<"tem_ture">>,
        <<"Dat 21">> => <<"atmos">>,
        <<"Dat 22">>=> <<"media">>,
        <<"Dat 23">>=> <<"cst">>,
        <<"Dat 24">>=> <<"temp_before">>,
        <<"Dat 25">>=> <<"temp_after">>,
        <<"Dat 26">> => <<"in_distend">>,
        <<"Dat 27">> => <<"out_distend">>,
        <<"Dat 30">>=> <<"exper_count">>,
        <<"Dat 31">>=> <<"pro_model">>
    },

    NewMap = maps:fold(
        fun(K, V, Acc) ->
            case maps:find(K, Trans) of
                {ok, <<"im_area">>} ->
                    Num = dgiot_utils:to_float(V),
                    Acc#{<<"im_area">> => ?AREA(Num)};
                {ok, <<"out_area">>} ->
                    Num = dgiot_utils:to_float(V),
                    Acc#{<<"out_area">> => ?AREA(Num)};
                {ok, NewK} ->
                    Acc#{NewK => V};
                _ ->
                    Acc#{K => V}
            end
        end, #{}, Map),
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, _} ->
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"basedata">> => NewMap}),
            dgiot_bridge:send_log(ChannelId, ProductId, "Device:~s BaseInfo ~s", [Devaddr, jsx:encode(NewMap)]);
        _ ->
            case dgiot_product:lookup_prod(ProductId) of
                {ok, #{<<"ACL">> := Acl, <<"name">> := Name, <<"devType">> := DevType, <<"dynamicReg">> := true}} ->
                    Device = #{
                        <<"status">> => <<"ONLINE">>,
                        <<"brand">> => Name,
                        <<"name">> => Devaddr,
                        <<"devaddr">> => Devaddr,
                        <<"devModel">> => DevType,
                        <<"product">> => ProductId,
                        <<"ACL">> => Acl,
                        <<"basedata">> => NewMap
                    },
                    dgiot_device:create_device(Device),
                    dgiot_bridge:send_log(ChannelId, ProductId, "Device:~p BaseInfo ~s", [Devaddr, jsx:encode(NewMap)]);
                _ ->
                    error
            end
    end;
handle_baseinfo(_, _, _) ->
    error.

handle_status(ChannelId, ProductId, #{<<"Start">> := Devaddr}) ->
    dgiot_bridge:send_log(ChannelId, ProductId, "Start Device:~s  ", [Devaddr]);
handle_status(ChannelId, ProductId, #{<<"Stop">> := Devaddr}) ->
    dgiot_bridge:send_log(ChannelId, ProductId, "Stop Device:~s ", [Devaddr]).

handle_data(ChannelId, ProductId, Map) ->
    case maps:find(<<"devaddr">>, Map) of
        {ok, DevAddr} ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId,DevAddr),
            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"basedata">> := #{<<"exper_count">> := Exper_count}}} ->
                    Payload = maps:fold(
                        fun(K, V, Acc) ->
                            case K of
                                <<"devaddr">> ->
                                    Acc;
                                _ ->
                                    NewK = re:replace(K, " ", "_", [global, {return, binary}]),
                                    Acc#{NewK => V}
                            end
                        end, #{}, Map),
                    NewPayload = maps:merge(Payload,#{<<"exper_count">> => Exper_count}),
                    dgiot_dlink_proctol:properties_report(ProductId, DevAddr, NewPayload),
                    dgiot_bridge:send_log(ChannelId, ProductId, "Device:~s Save_td ~s", [DevAddr, jsx:encode(NewPayload)]);
                _ ->
                    io:format("~s ~p here~n",[?FILE,?LINE]),
                    error
            end;
        _ ->
            pass
    end.
