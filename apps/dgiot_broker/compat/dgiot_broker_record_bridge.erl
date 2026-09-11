%% @doc 记录桥（刀 5）：把线上 map 转成 EMQX 语义的 #message{} 记录，
%% 交付给 VM 内订阅者（dgiot channel/插件用的就是 emqx_broker:subscribe）。
%%
%% 只在无 EMQX 模式编译；核心侧用 function_exported/3 运行时探测，
%% 因此 EMQX 在位时该模块不存在也不会影响任何行为。
%%
%% 交付形态与 EMQX 一致：订阅进程收到 {deliver, #message{}}。
-module(dgiot_broker_record_bridge).

-include("emqx.hrl").

-export([deliver/2, to_message/1, from_message/1]).

-spec deliver(pid(), map()) -> ok.
deliver(Pid, Packet) ->
    Pid ! {deliver, to_message(Packet)},
    ok.

%% 线上 map → #message{}
to_message(Packet) when is_map(Packet) ->
    Id = case maps:get(packet_id, Packet, undefined) of
             undefined -> emqx_guid:gen();
             Existing -> Existing
         end,
    #message{id = Id,
             qos = maps:get(qos, Packet, 0),
             from = maps:get(from, Packet, <<"inproc">>),
             flags = #{dup => maps:get(dup, Packet, false),
                       retain => maps:get(retain, Packet, false),
                       sys => false},
             headers = maps:get(headers, Packet, #{}),
             topic = maps:get(topic, Packet, <<>>),
             payload = maps:get(payload, Packet, <<>>),
             timestamp = maps:get(timestamp, Packet,
                                  erlang:system_time(millisecond))}.

%% #message{} → 线上 map（反向，供内部转投递）
from_message(#message{id = Id, qos = Qos, from = From, flags = Flags,
                      headers = Headers, topic = Topic, payload = Payload,
                      timestamp = Ts}) ->
    #{type => publish,
      packet_id => Id,
      qos => Qos,
      from => From,
      dup => maps:get(dup, Flags, false),
      retain => maps:get(retain, Flags, false),
      headers => Headers,
      topic => Topic,
      payload => Payload,
      timestamp => Ts}.
