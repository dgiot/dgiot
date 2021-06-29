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

-module(dgiot_sinmahe_channel).
-include_lib("dgiot/include/logger.hrl").
-behavior(dgiot_channelx).
-author("johnliu").
-include("dgiot_sinmahe.hrl").
-define(TYPE, <<"SINMAHE">>).
-define(MAX_BUFF_SIZE, 1024).
-record(state, {id, mod, product, env = #{}, buff_size = 1024000, header = <<>>}).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel(?TYPE).
-channel_type(#{type => 1,
    title => #{zh => <<"SINMAHE采集通道"/utf8>>},
    description => #{zh => <<"SINMAHE采集通道"/utf8>>}}).
%% 注册通道参数
-params(#{
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_tech/zh/product/dgiot/channel/SINMAHE%20%E5%9B%BE%E6%A0%87.jpg">>,
        title => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        },
        description => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        }
    }
}).


start(ChannelId, ChannelArgs) ->
    Size = maps:get(<<"Size">>, ChannelArgs, 5),
    MaxOverFlow = maps:get(<<"MaxOverFlow">>, ChannelArgs, 10),
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs#{<<"Size">> => Size,
        <<"MaxOverFlow">> => MaxOverFlow}).

%% 通道初始化
init(?TYPE, ChannelId, Args) ->
    #{<<"product">> := Products} = Args,
    [{ProductId, Acl} | _] =
        lists:map(fun({ProdcutId1, #{<<"ACL">> := Acl1}}) ->
            {ProdcutId1, Acl1}
                  end, Products),
    dgiot_sinmahe:get_sinmahe_json(),
    State = #state{id = ChannelId, product = Products, env = #{<<"ACL">> => Acl, <<"productid">> => ProductId}},
    {ok, State, []}.

handle_init(State) ->
    {ok, State}.

handle_event(EventType, Event, _State) ->
    ?LOG(info, "channel ~p, ~p", [EventType, Event]),
    ok.

% SELECT clientid, payload, topic FROM "BasicInformation"
% SELECT clientid, payload, topic FROM "PeriodicInformation"
% SELECT clientid, payload, topic FROM "FaultInformation"
% SELECT clientid, disconnected_at FROM "$events/client_disconnected" WHERE username = 'sinmahe'
% SELECT clientid, connected_at FROM "$events/client_connected" WHERE username = 'sinmahe'
handle_message({rule, #{clientid := DevAddr, connected_at := _ConnectedAt}, #{peername := PeerName} = _Context}, State) ->
    ProductId = dgiot_data:get({simahe, <<"PeriodicInformation">>}),
    dgiot_mqtt:publish(DevAddr, <<"connected/", DevAddr/binary>>, jsx:encode(#{DevAddr => <<"connected">>})),
    dgiot_parse:update_object(<<"Device">>, dgiot_parse:get_deviceid(ProductId, DevAddr), #{<<"isEnable">> => true,
        <<"status">> => <<"ONLINE">>,
        <<"ip">> => PeerName}),
    dgiot_data:insert({simahe, DevAddr}, 1),
    dgiot_sinmahe:create_subdev(ProductId, DevAddr, <<"FaultInformation">>),
    dgiot_sinmahe:create_subdev(ProductId, DevAddr, <<"BasicInformation">>),
    {ok, State};

handle_message({rule, #{clientid := DevAddr, disconnected_at := _DisconnectedAt}, _Context}, State) ->
    ProductId = dgiot_data:get({simahe, <<"PeriodicInformation">>}),
    dgiot_mqtt:publish(DevAddr, <<"disconnected/", DevAddr/binary>>, jsx:encode(#{DevAddr => <<"disconnected">>})),
    dgiot_parse:update_object(<<"Device">>, dgiot_parse:get_deviceid(ProductId, DevAddr), #{<<"status">> => <<"OFFLINE">>}),
    dgiot_data:insert({simahe, DevAddr}, 0),
    {ok, State};

handle_message({rule, #{clientid := DevAddr, payload := Payload, topic := <<"BasicInformation">>}, _Msg},
    #state{id = ChannelId, env = #{<<"productid">> := ProductId}} = State) ->
    dgiot_mqtt:publish(DevAddr, <<"BasicInformation/", DevAddr/binary>>, Payload),
    dgiot_bridge:send_log(ChannelId, "topic:~p Payload:~p ~n", [<<DevAddr/binary, "/", "BasicInformation">>, Payload]),
    case jsx:is_json(Payload) of
        true ->
            Map = jsx:decode(Payload, [{labels, binary}, return_maps]),
            Data = format_basic(Map),
            BasicProductId = dgiot_data:get({simahe, <<"BasicInformation">>}),
            ProductId = dgiot_data:get({simahe, <<"PeriodicInformation">>}),
            Now =
                case maps:get(<<"UpdateTime">>, Data, now) of
                    now ->
                        dgiot_datetime:now_secs();
                    <<"2011", _/binary>> ->
                        dgiot_datetime:now_secs();
                    <<Y:4/binary, M:2/binary, D:2/binary, H:2/binary>> ->
                        dgiot_datetime:to_unixtime(dgiot_datetime:to_localtime(<<Y/binary, "-", M/binary, "-", D/binary, "  ", H/binary, ":00:00">>))
                end,
            NewData = maps:without([<<"GPS">>, <<"UpdateTime">>], Data),
            %% todo 修改为取最后一次周期数据的时间
            Query = #{<<"keys">> => <<"last_row(*)">>, <<"limit">> => 1},
            case dgiot_tdengine:get_device(ProductId, <<"PeriodicInformation_", DevAddr/binary>>, Query) of
                {ok, Data} ->
                    ?LOG(info, "Data ~p", [Data]);
                _ -> Now
            end,
            dgiot_tdengine_adapter:save(BasicProductId, <<"BasicInformation_", DevAddr/binary>>, NewData#{
                <<"createdat">> => dgiot_datetime:format(Now, <<"YY-MM-DDTHH:NN:SS.000Z">>),
                <<"UpdateTime">> => Now}),
            case dgiot_parse:get_object(<<"Device">>, dgiot_parse:get_deviceid(ProductId, DevAddr)) of
                {ok, #{<<"objectId">> := ObjectId, <<"basedata">> := BaseData}} ->
                    Data3 =
                        case maps:find(<<"basicdata">>, BaseData) of
                            {ok, Data1} ->
                                Data2 =
                                    case maps:find(<<"GPS">>, Data) of
                                        {ok, #{<<"Lat">> := Lat, <<"Lon">> := Lon}} ->
                                            Address = dgiot_gps:get_baidu_addr(Lon, Lat),
                                            maps:merge(Data1, Address);
                                        error -> Data1
                                    end,
                                maps:merge(Data1, Data2);
                            error ->
                                case maps:find(<<"GPS">>, Data) of
                                    {ok, #{<<"Lat">> := Lat, <<"Lon">> := Lon}} ->
                                        Address = dgiot_gps:get_baidu_addr(Lon, Lat),
                                        maps:merge(Data, Address);
                                    error -> Data
                                end
                        end,
                    dgiot_parse:update_object(<<"Device">>, ObjectId, #{<<"isEnable">> => true,
                        <<"basedata">> => BaseData#{<<"basicdata">> => maps:merge(Data3, Data)}});
                _ ->
                    ?LOG(info, "Device not exist")
            end;
        false ->
            pass
    end,
    {ok, State};

handle_message({rule, #{clientid := DevAddr, payload := Payload, topic := <<"PeriodicInformation">>}, _Msg},
    #state{id = ChannelId} = State) ->
    ProductId = dgiot_data:get({simahe, <<"PeriodicInformation">>}),
    dgiot_mqtt:publish(DevAddr, <<"PeriodicInformation/", DevAddr/binary>>, Payload),
    dgiot_bridge:send_log(ChannelId, "topic:~p Payload:~p ~n", [<<DevAddr/binary, "/", "PeriodicInformation">>, Payload]),
    case dgiot_data:get({simahe, DevAddr}) of
        {ok, 1} -> pass;
        _ ->
            dgiot_data:insert({simahe, DevAddr}, 1),
            dgiot_parse:update_object(<<"Device">>, dgiot_parse:get_deviceid(ProductId, DevAddr), #{<<"isEnable">> => true, <<"status">> => <<"ONLINE">>})
    end,
    case jsx:is_json(Payload) of
        true ->
            Data1 = format_periodic(jsx:decode(Payload, [{labels, binary}, return_maps])),
            List = maps:to_list(Data1),
            Result =
                if
                    length(List) > 40 ->
                        {ok, Data1};
                    true ->
                        case dgiot_data:get({periodicInformation, DevAddr}) of
                            not_find ->
                                dgiot_data:insert({periodicInformation, DevAddr}, Data1),
                                {error, #{}};
                            Data2 ->
                                dgiot_data:delete({periodicInformation, DevAddr}),
                                {ok, maps:merge(Data1, Data2)}
                        end
                end,
            case Result of
                {ok, Data} ->
                    dgiot_bridge:send_log(ChannelId, "topic:~p Data:~p ~n", [<<DevAddr/binary, "/", "PeriodicInformation">>, Data]),
                    case length(maps:to_list(Data)) < 40 of
                        true -> pass;
                        _ ->
                            dgiot_sinmahe:update_runstate(ProductId, DevAddr, Data),
                            dgiot_sinmahe:update_powerstate(ProductId, DevAddr, Data),
                            dgiot_tdengine_adapter:save(ProductId, DevAddr, Data)
                    end;
                _ -> pass
            end;
        false ->
            pass
    end,
    {ok, State};

handle_message({rule, #{clientid := DevAddr, payload := Payload, topic := <<"FaultInformation">>}, _Msg}, #state{id = ChannelId} = State) ->
    dgiot_bridge:send_log(ChannelId, "topic:~p Payload:~p ~n", [<<DevAddr/binary, "/", "FaultInformation">>, Payload]),
    dgiot_mqtt:publish(DevAddr, <<"FaultInformation/", DevAddr/binary>>, Payload),
    case jsx:is_json(Payload) of
        true ->
            Data = jsx:decode(Payload, [{labels, binary}, return_maps]),
            FaultProductId1 = dgiot_data:get({simahe, <<"FaultInformation">>}),
            ProductId = dgiot_data:get({simahe, <<"PeriodicInformation">>}),
            dgiot_tdengine_adapter:save(FaultProductId1, <<"FaultInformation_", DevAddr/binary>>, Data),
            Data1 = Data#{<<"occurtime">> => dgiot_datetime:now_secs()},
            case dgiot_parse:get_object(<<"Device">>, dgiot_parse:get_deviceid(ProductId, DevAddr)) of
                {ok, #{<<"objectId">> := ObjectId, <<"name">> := Name, <<"basedata">> := BaseData}} ->
                    FaultData =
                        case maps:find(<<"faultdata">>, BaseData) of
                            {ok, Data2} -> maps:merge(Data2, Data1);
                            error -> Data1
                        end,
                    dgiot_parse:update_object(<<"Device">>, ObjectId, #{<<"isEnable">> => false,
                        <<"basedata">> => BaseData#{<<"faultdata">> => FaultData}}),
                    case maps:find(<<"faultrule">>, BaseData) of
                        {ok, FaultRule} -> dgiot_sinmahe:send(ObjectId, Name, FaultRule, Data);
                        error -> Data1
                    end;
                _ ->
                    ?LOG(info, "Device not exist")
            end;
        false ->
            pass
    end,
    {ok, State};

handle_message(Message, #state{id = ChannelId, product = ProductId} = State) ->
    ?LOG(info, "Channel ~p, Product ~p, handle_message ~p", [ChannelId, ProductId, Message]),
    {ok, State}.


stop(ChannelType, ChannelId, _State) ->
    ?LOG(info, "channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.


format_basic(#{<<"WeightFactor">> := WeightFactor} = Message) when is_integer(WeightFactor) ->
    format_basic(Message#{<<"WeightFactor">> => WeightFactor / 1});
format_basic(#{<<"RatedLoad">> := RatedLoad} = Message) when is_integer(RatedLoad) ->
    format_basic(Message#{<<"RatedLoad">> => RatedLoad / 1});
format_basic(#{<<"RatedFreq">> := RatedFreq} = Message) when is_integer(RatedFreq) ->
    format_basic(Message#{<<"RatedFreq">> => RatedFreq / 1});
format_basic(#{<<"RatedPower">> := RatedPower} = Message) when is_integer(RatedPower) ->
    format_basic(Message#{<<"RatedPower">> => RatedPower / 1});
format_basic(#{<<"Lat">> := Lat, <<"Lon">> := Lon} = Message) ->
    [LonDeg, LatDeg] = dgiot_gps:get_deng_gps(Lon, Lat),
    Message#{<<"GPS">> => #{<<"Lon">> => LonDeg, <<"Lat">> => LatDeg},
        <<"Lat">> := LatDeg, <<"Lon">> := LonDeg};

format_basic(Message) ->
    Message.

format_periodic(#{<<"DeadLoad">> := DeadLoad} = Data) when is_integer(DeadLoad) ->
    format_periodic(Data#{<<"DeadLoad">> := DeadLoad / 1});
format_periodic(#{<<"NetWeight">> := NetWeight} = Data) when is_integer(NetWeight) ->
    format_periodic(Data#{<<"NetWeight">> := NetWeight / 1});
format_periodic(#{<<"Iout">> := Iout} = Data) when is_integer(Iout) ->
    format_periodic(Data#{<<"Iout">> := Iout / 1});
format_periodic(#{<<"Vout">> := Vout} = Data) when is_integer(Vout) ->
    format_periodic(Data#{<<"Vout">> := Vout / 1});
format_periodic(#{<<"BusVoltage">> := BusVoltage} = Data) when is_integer(BusVoltage) ->
    format_periodic(Data#{<<"BusVoltage">> := BusVoltage / 1});
format_periodic(#{<<"RunFreq">> := RunFreq} = Data) when is_integer(RunFreq) ->
    format_periodic(Data#{<<"RunFreq">> := RunFreq / 1});
format_periodic(Data) ->
    Data.
