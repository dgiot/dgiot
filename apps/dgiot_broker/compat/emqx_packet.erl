%% @doc 同名承接：emqx_packet（MQTT 包 → 消息转换 + 打印）。
%% 插件 emqx_sn 网关用 to_message/2 把 MQTT-SN 发布转成 #message{} 交给
%% emqx_broker:publish；format/1 仅用于 debug 日志。契约按 EMQX 4.4。
-module(emqx_packet).

-include("emqx.hrl").
-include("emqx_mqtt.hrl").

-export([to_message/2, to_message/3, format/1]).

%% @doc Publish 包 → #message{}（默认空 headers）
to_message(Packet, ClientId) ->
    to_message(Packet, ClientId, #{}).

%% @doc Publish 包 → #message{}：flags 带 dup/retain，headers 带 properties。
to_message(#mqtt_packet{
              header = #mqtt_packet_header{
                          type = ?PUBLISH,
                          retain = Retain,
                          qos = QoS,
                          dup = Dup},
              variable = #mqtt_packet_publish{
                            topic_name = Topic,
                            properties = Props},
              payload = Payload}, ClientId, Headers) ->
    Msg = emqx_message:make(ClientId, QoS, Topic, Payload),
    Msg#message{flags = #{dup => Dup, retain => Retain},
                headers = Headers#{properties => Props}};
to_message(Packet, _ClientId, _Headers) ->
    erlang:error({not_publish_packet, Packet}).

%% @doc 包 → iolist()（debug 打印用）
format(#mqtt_packet{header = #mqtt_packet_header{
                                  type = Type,
                                  dup = Dup,
                                  qos = QoS,
                                  retain = Retain},
                    variable = _Variable,
                    payload = _Payload}) ->
    io_lib:format("~s(Q~p, R~p, D~p)", [type_name(Type), QoS, i(Retain), i(Dup)]).

type_name(?RESERVED) -> "RESERVED";
type_name(?CONNECT) -> "CONNECT";
type_name(?CONNACK) -> "CONNACK";
type_name(?PUBLISH) -> "PUBLISH";
type_name(?PUBACK) -> "PUBACK";
type_name(?PUBREC) -> "PUBREC";
type_name(?PUBREL) -> "PUBREL";
type_name(?PUBCOMP) -> "PUBCOMP";
type_name(?SUBSCRIBE) -> "SUBSCRIBE";
type_name(?SUBACK) -> "SUBACK";
type_name(?UNSUBSCRIBE) -> "UNSUBSCRIBE";
type_name(?UNSUBACK) -> "UNSUBACK";
type_name(?PINGREQ) -> "PINGREQ";
type_name(?PINGRESP) -> "PINGRESP";
type_name(?DISCONNECT) -> "DISCONNECT";
type_name(Other) -> io_lib:format("~p", [Other]).

i(true) -> 1;
i(false) -> 0;
i(Other) -> Other.
