%% @doc 同名承接头文件：emqx_mqtt.hrl（刀 5）。
%%
%% dgiot 侧用到 `#mqtt_packet{}` 6 处、`#mqtt_msg{}` 3 处；插件侧还会碰
%% 包体记录与常量。这里提供同名同形定义 + 必要常量（值按 MQTT 3.1.1 与
%% EMQX 4.4 对齐），并在需要处显式标注"未实现的构造器"。
-ifndef(EMQX_MQTT_HRL).
-define(EMQX_MQTT_HRL, true).

%% ---- 包类型常量（MQTT 3.1.1） ----
-define(RESERVED, 0).
-define(CONNECT, 1).
-define(CONNACK, 2).
-define(PUBLISH, 3).
-define(PUBACK, 4).
-define(PUBREC, 5).
-define(PUBREL, 6).
-define(PUBCOMP, 7).
-define(SUBSCRIBE, 8).
-define(SUBACK, 9).
-define(UNSUBSCRIBE, 10).
-define(UNSUBACK, 11).
-define(PINGREQ, 12).
-define(PINGRESP, 13).
-define(DISCONNECT, 14).

%% ---- QoS 与协议版本 ----
-define(QOS_0, 0).
-define(QOS_1, 1).
-define(QOS_2, 2).
-define(QOS_FAIL, 16#80).
-define(MQTT_PROTO_V3, 3).
-define(MQTT_PROTO_V4, 4).

%% ---- 记录：形状对齐 EMQX 4.4（去类型标注） ----
-record(mqtt_packet_header, {
          type = ?RESERVED,
          dup = false,
          qos = ?QOS_0,
          retain = false
        }).

-record(mqtt_packet_connect, {
          proto_name = <<"MQTT">>,
          proto_ver = ?MQTT_PROTO_V4,
          is_bridge = false,
          clean_start = true,
          will_flag = false,
          will_qos = ?QOS_0,
          will_retain = false,
          keepalive = 0,
          properties = #{},
          clientid = <<>>,
          will_props = #{},
          will_topic = undefined,
          will_payload = undefined,
          username = undefined,
          password = undefined
        }).

-record(mqtt_packet_connack, {ack_flags, code, properties = #{}}).
-record(mqtt_packet_publish, {topic_name, packet_id, properties = #{}}).
-record(mqtt_packet_puback, {packet_id, reason_code = 0, properties = #{}}).
-record(mqtt_packet_subscribe, {packet_id, properties = #{}, topic_filters}).
-record(mqtt_packet_suback, {packet_id, reason_codes, properties = #{}}).
-record(mqtt_packet_unsubscribe, {packet_id, properties = #{}, topic_filters}).
-record(mqtt_packet_unsuback, {packet_id, reason_codes, properties = #{}}).

-record(mqtt_packet, {
          header,
          variable,
          payload
        }).

%% 兼容别名：老代码里出现的 #mqtt_msg{}
-record(mqtt_msg, {
          id,
          qos = 0,
          retain = false,
          dup = false,
          packet_id,
          topic,
          payload,
          timestamp
        }).

-endif.
