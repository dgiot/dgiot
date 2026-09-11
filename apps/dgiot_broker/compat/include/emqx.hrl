%% @doc 同名承接头文件：emqx.hrl（刀 5）。
%%
%% 为什么必须有：dgiot 侧 8 处、插件侧 44 处**直接匹配记录**
%% （如 dgiot_mqtt_message.erl 的 `#message{topic = <<"$dg/thing/", _/binary>>, payload = P}`），
%% 25 个文件 include 本头。只提供访问器不够——记录必须同名同字段同序，
%% 这些调用点才能一行不改地编译。
%%
%% 字段名/顺序/默认值对齐 EMQX 4.4 的 emqx.hrl（Apache-2.0），**去掉类型标注**
%% 以免依赖 emqx_types 的远程类型；记录兼容只需要名字/顺序/默认值。
%%
%% 仅在无 EMQX 模式编译（同名冲突）。
-ifndef(EMQX_HRL).
-define(EMQX_HRL, true).

-record(subscription, {topic, subid, subopts}).

-record(message, {
          id,
          qos = 0,
          from,
          flags = #{},
          headers = #{},
          topic,
          payload,
          timestamp
         }).

-record(delivery, {sender, message}).

-record(route, {topic, dest}).

-record(plugin, {name, dir, descr, vendor, active = false, info = #{}, type}).

-record(command, {name, action, args = [], opts = [], usage, descr}).

-record(banned, {who, by, reason, at, until}).

-endif.
