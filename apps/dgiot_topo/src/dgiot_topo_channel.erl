%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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
-module(dgiot_topo_channel).
-behavior(dgiot_channelx).
-define(TYPE, <<"DGIOTTOPO">>).
-author("johnliu").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").

-record(state, {id, env = #{}}).

%% API
-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).


%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"TOPO组态通道"/utf8>>
    },
    description => #{
        zh => <<"TOPO组态通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"TOPOBRAND">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"dgiottopo"/utf8>>,
        title => #{
            zh => <<"DGIOT厂商"/utf8>>
        },
        description => #{
            zh => <<"DGIOT厂商"/utf8>>
        }
    },
    <<"BRIDGEURL">> => #{
        order => 2,
        type => string,
        required => true,
        default => <<"http://127.0.0.1:5080"/utf8>>,
        title => #{
            zh => <<"桥接地址"/utf8>>
        },
        description => #{
            zh => <<"桥接地址"/utf8>>
        }
    },
    <<"OFFSET">> => #{
        order => 3,
        type => string,
        required => true,
        default => <<"WGS:0,0.0012;GCJ-02:0,0.0012;BD-09:0,-0.0012"/utf8>>,
        title => #{
            zh => <<"经纬度纠偏系数"/utf8>>
        },
        description => #{
            zh => <<"WGS:国际上通用的地心坐标系。目前的设备包含GPS芯片获取的经纬度一般为WGS84地理坐标系。谷歌卫星地图使用的就是WGS-84标准。
                  GCJ-02：国家测绘局的一套标准GCJ-02(国测局)，在WGS的基础上进行加密偏移。谷歌街道地图、腾讯、高德地图使用该标准。
                   BD-09：百度在GCJ-02的基础上又进行了加密处理，形成了百度独有的BD-09坐标系。
                   格式：(WGS:Lonoffset,Latoffset;GCJ-02:Lonoffset,Latoffset;BD-09:Lonoffset,Latoffset;...)"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/TOPO%E7%BB%84%E6%80%81%E9%80%9A%E9%81%93%E5%9B%BE%E6%A0%87.png">>,
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
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs#{
        <<"Size">> => 1
    }).

%% 通道初始化
init(?TYPE, ChannelId, #{<<"product">> := Products, <<"BRIDGEURL">> := Bridgeurl} = ChannelArgs) ->
    NewEnv = get_newenv(ChannelArgs),
    State = #state{
        id = ChannelId,
        env = NewEnv#{productids => get_prodcutids(Products)}
    },
    dgiot_data:insert(topourl, <<Bridgeurl/binary, "/iotapi/topo">>),
    dgiot_topo:get_Product(),
    {ok, State}.

%% 初始化池子
handle_init(#state{env = #{productids := ProductIds}} = State) ->
    [dgiot_mqtt:subscribe(<<"topo/", ProductId/binary, "/#">>) || ProductId <- ProductIds],
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, _State) ->
    ?LOG(info, "channel ~p, ~p", [EventId, Event]),
    ok.

handle_message({deliver, _Topic, Msg}, #state{id = ChannelId} = State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    dgiot_bridge:send_log(ChannelId, "Topic ~p DTU revice from  ~p", [dgiot_mqtt:get_topic(Msg), dgiot_utils:binary_to_hex(Payload)]),
    case binary:split(dgiot_mqtt:get_topic(Msg), <<$/>>, [global, trim]) of
%%接收task汇聚过来的整个dtu物模型采集的数据 发送组态
        [<<"topo">>, ProductId, DtuAddr, <<"post">>] ->
            Data = jsx:decode(Payload, [{labels, binary}, return_maps]),
            DeviceId = dgiot_parse:get_deviceid(ProductId, DtuAddr),
            Thingdata = maps:get(<<"thingdata">>, Data, #{}),
            dgiot_topo:send_topo(ProductId, DeviceId, Thingdata);
        Other ->
            ?LOG(info, "Other ~p", [Other]),
            pass
    end,
    {ok, State};

handle_message(Message, State) ->
    ?LOG(info, "channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(info, "channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.

get_prodcutids(Products) ->
    lists:foldl(fun({ProdcutId, _}, Acc) ->
        Acc ++ [ProdcutId]
                end, [], Products).

get_newenv(Args) ->
    maps:without([
        <<"behaviour">>,
        <<"MaxOverFlow">>,
        <<"Size">>,
        <<"applicationtText">>,
        <<"product">>], Args).
