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
-define(AREA(R), 3.14 * R * R).
-record(state, {
    id,
    productId,
    devaddr,
    exper_count
}).


%% TCP callback
-export([child_spec/2, init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).


child_spec(Port, State) ->
    dgiot_tcp_server:child_spec(?MODULE, Port, State).

%% =======================
%% {ok, State} | {stop, Reason}
init(#tcp{state = #state{id = ChannelId} = State} = TCPState) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, ?TYPE, ProductIds} ->
            ProductId = lists:nth(1, ProductIds),
            NewState = State#state{productId = ProductId},
            {ok, TCPState#tcp{state = NewState}};
        {error, not_find} ->
            {error, not_find_channel}
    end.

% device2task 上行
handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, productId = ProductId} = State} = TCPState) ->
%%    Map =
    case jsx:is_json(Buff) of
        true ->
            Map = jsx:decode(Buff, [{labels, binary}, return_maps]),
            dgiot_bridge:send_log(ChannelId, ProductId, " tcp Recv ~ts", [unicode:characters_to_list(jsx:encode(Map))]),
            case maps:size(Map) of
                11 ->
                    NewState = handle_data(Map, State),
                    {noreply, TCPState#tcp{state = NewState}};
                _ ->
                    NewState = parse_frame(Map, State),
                    {noreply, TCPState#tcp{state = NewState}}
            end;
        _ ->
            dgiot_bridge:send_log(ChannelId, ProductId, " tcp Recv ~ts", [unicode:characters_to_list(jsx:encode(Buff))])
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

parse_frame(#{<<"Start">> := Devaddr}, #state{id = ChannelId, productId = ProductId, devaddr = Devaddr, exper_count = Exper_count} = State) ->
    dgiot_bridge:send_log(ChannelId, ProductId, "Start Device:~s  ", [Devaddr]),
    TaskDeviceId = dgiot_parse_id:get_deviceid(ProductId, <<Devaddr/binary, Exper_count/binary>>),
    case dgiot_parse:get_object(<<"Device">>, TaskDeviceId) of
        {ok, #{<<"profile">> := Profile}} ->
            Historicaldatacolumns = get_historicaldatacolumns(ProductId),
            dgiot_parse:update_object(<<"Device">>, TaskDeviceId, #{<<"profile">> => Profile#{<<"starttime">> => dgiot_datetime:now_ms(), <<"step">> => 1, <<"historicaldatacolumns">> => Historicaldatacolumns}});
        _R ->
            pass
    end,
    State;

parse_frame(#{<<"Stop">> := Devaddr}, #state{id = ChannelId, productId = ProductId, devaddr = Devaddr, exper_count = Exper_count} = State) ->
    dgiot_bridge:send_log(ChannelId, ProductId, "Stop Device:~s ", [Devaddr]),
    TaskDeviceId = dgiot_parse_id:get_deviceid(ProductId, <<Devaddr/binary, Exper_count/binary>>),
    case dgiot_parse:get_object(<<"Device">>, TaskDeviceId) of
        {ok, #{<<"profile">> := Profile}} ->
            Historicaldata = maps:get(<<"historicaldata">>, Profile, []),
            Jsonlist = dgiot_evidence_handler:arrtojsonlist(Historicaldata),
            DrawxnqxPath = maps:get(<<"drawxnqxPath">>, Profile, <<>>),
            Path =
                case dgiot_evidence_handler:python_drawxnqx(TaskDeviceId, Jsonlist) of
                    <<"">> ->
                        DrawxnqxPath;
                    Path1 ->
                        Path1
                end,
            dgiot_parse:update_object(<<"Device">>, TaskDeviceId, #{<<"profile">> => Profile#{<<"drawxnqxPath">> => Path, <<"endtime">> => dgiot_datetime:now_ms()}});
        _R ->
            pass
    end,
    State#state{devaddr = <<>>, exper_count = <<>>};

parse_frame(#{<<"Dat 39">> := Devaddr} = Map, #state{id = ChannelId, productId = ProductId} = State) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, Devaddr),
    Trans = #{
        <<"Dat 0">> => <<"pump_type">>,
        <<"Dat 1">> => <<"QG">>,
        <<"Dat 2">> => <<"HG">>,
        <<"Dat 3">> => <<"PG">>,
        <<"Dat 4">> => <<"XG">>,
        <<"Dat 5">> => <<"npsh">>,
        <<"Dat 6">> => <<"im_area">>,
        <<"Dat 7">> => <<"out_area">>, %% 出口管径(m)
        <<"Dat 10">> => <<"NG">>,
        <<"Dat 11">> => <<"dgiot_testing_equipment_pressure">>,
        <<"Dat 12">> => <<"dgiot_testing_equipment_electricity">>,
        <<"Dat 13">> => <<"electnum">>,
        <<"Dat 14">> => <<"normal_fre">>,
        <<"Dat 15">> => <<"phase">>,
        <<"Dat 20">> => <<"tem_ture">>,
        <<"Dat 21">> => <<"atmos">>,
        <<"Dat 22">> => <<"media">>,
        <<"Dat 23">> => <<"cst">>,
        <<"Dat 24">> => <<"temp_before">>,
        <<"Dat 25">> => <<"temp_after">>,
        <<"Dat 26">> => <<"in_distend">>,
        <<"Dat 27">> => <<"out_distend">>,
        <<"Dat 30">> => <<"exper_count">>,
        <<"Dat 31">> => <<"pro_model">>
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
    dgiot_bridge:send_log(ChannelId, ProductId, "Device:~p BaseInfo ~ts", [Devaddr, unicode:characters_to_list(jsx:encode(NewMap))]),
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"ACL">> := Acl, <<"name">> := ProductName, <<"devType">> := DevType, <<"dynamicReg">> := true}} ->
            Exper_count = maps:get(<<"exper_count">>, NewMap, <<>>),
%%                     创建台体
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, Devaddr),
            Testbed = <<"先科台体_"/utf8, Devaddr/binary>>,
            dgiot_device:create_device(#{
                <<"status">> => <<"ONLINE">>,
                <<"brand">> => ProductName,
                <<"name">> => Testbed,
                <<"devaddr">> => Devaddr,
                <<"devModel">> => DevType,
                <<"product">> => ProductId,
                <<"ACL">> => Acl,
                <<"basedata">> => NewMap}),
%%                     创建任务
            Profile = #{<<"identifier">> => <<"inspectionReportTemp">>, <<"wordtemplatename">> => ProductName, <<"step">> => 0, <<"testbed">> => Testbed},
            Body = #{<<"name">> => <<Testbed/binary, "_检测任务_"/utf8, Exper_count/binary>>, <<"devaddr">> => <<Devaddr/binary, Exper_count/binary>>, <<"product">> => ProductId, <<"profile">> => Profile, <<"parentId">> => DeviceId, <<"basedata">> => NewMap},
            SessionToken = dgiot_auth:get_sessiontoken(maps:without([<<"*">>], Acl)),
            dgiot_evidence_handler:post_report(Body, SessionToken),
            State#state{devaddr = Devaddr, exper_count = Exper_count};
        _ ->
            State
    end;

parse_frame(_, State) ->
    State.

handle_data(Map, #state{id = ChannelId, productId = ProductId, devaddr = Devaddr, exper_count = Exper_count} = State) when size(Exper_count) > 0 ->
    case maps:find(<<"devaddr">>, Map) of
        {ok, DevAddr} ->
            Payload = maps:fold(
                fun(K, V, Acc) ->
                    case K of
                        <<"devaddr">> ->
                            Acc;
                        _ ->
                            NewK = re:replace(K, " ", "_", [global, {return, binary}]),
                            Acc#{list_to_binary(string:to_lower(binary_to_list(NewK))) => V}
                    end
                end, #{}, Map),
            AllData = dgiot_dlink_proctol:properties_report(ProductId, DevAddr, Payload),
            save_historicaldata(AllData, ProductId, <<Devaddr/binary, Exper_count/binary>>),
            dgiot_bridge:send_log(ChannelId, ProductId, "Device:~s save_historicaldata ~s", [DevAddr, jsx:encode(AllData)]);
        _ ->
            pass
    end,
    State;

handle_data(_, State) ->
    State.

save_historicaldata(AllData, ProductId, TaskDevaddr) ->
    io:format("~s ~p AllData = ~p.~n", [?FILE, ?LINE, AllData]),
    TaskDeviceId = dgiot_parse_id:get_deviceid(ProductId, TaskDevaddr),
    case dgiot_parse:get_object(<<"Device">>, TaskDeviceId) of
        {ok, #{<<"profile">> := Profile}} ->
            Historicaldata = maps:get(<<"historicaldata">>, Profile, []),
            DrawxnqxPath = maps:get(<<"drawxnqxPath">>, Profile, <<>>),
            NewHistoricaldata = lists:merge(Historicaldata, [AllData]),
            Jsonlist = dgiot_evidence_handler:arrtojsonlist(NewHistoricaldata),
            Path =
                case dgiot_evidence_handler:python_drawxnqx(TaskDeviceId, Jsonlist) of
                    <<"">> ->
                        DrawxnqxPath;
                    Path1 ->
                        Path1
                end,
            dgiot_parse:update_object(<<"Device">>, TaskDeviceId, #{<<"profile">> => Profile#{<<"drawxnqxPath">> => Path, <<"historicaldata">> => NewHistoricaldata}});
        _R ->
            pass
    end.

get_historicaldatacolumns(ProductId) ->
    Props = dgiot_product:get_props(ProductId, <<"*">>),
    lists:foldl(fun(Prop, Acc) ->
        case Prop of
            #{<<"name">> := Name, <<"identifier">> := Identifier, <<"dataType">> := DataType} ->
                Specs = maps:get(<<"specs">>, DataType, #{}),
                Unit =
                    case maps:find(<<"unit">>, Specs) of
                        error ->
                            <<>>;
                        {ok, Un} when size(Un) > 0 ->
                            <<"(", Un/binary, ")">>;
                        _ ->
                            <<>>
                    end,
                Acc ++ [#{<<"prop">> => Identifier, <<"label">> => <<Name/binary, Unit/binary>>}];
            _ ->
                Acc
        end
                end, [], Props).
