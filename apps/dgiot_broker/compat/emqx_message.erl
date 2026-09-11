%% @doc 同名承接：emqx_message（刀 5，记录形态）。
%%
%% 刀 4 曾用 map；实测发现 dgiot 侧 8 处、插件侧 44 处**直接匹配 #message{} 记录**，
%% 故回归 EMQX 的**记录形态**作为 VM 内规范表示：
%%   #message{id, qos, from, flags, headers, topic, payload, timestamp}
%% 线上仍用我们的 map（frame 编解码），两个边界由 dgiot_broker_record_bridge 转换。
-module(emqx_message).

-include("emqx.hrl").

-export([make/1, make/2, make/3, make/4,
         id/1, qos/1, flags/1, headers/1, topic/1, payload/1, from/1, timestamp/1,
         set_topic/2, set_payload/2, set_qos/2, set_headers/2,
         is_message/1, to_map/1]).

%% make/1：已是记录则原样返回（补默认）；二进制视为只有主题的消息
make(Msg) when is_record(Msg, message) ->
    Msg#message{id = ensure_id(Msg#message.id),
                flags = ensure_flags(Msg#message.flags),
                headers = ensure_headers(Msg#message.headers),
                timestamp = ensure_ts(Msg#message.timestamp)};
make(Topic) when is_binary(Topic) ->
    make(Topic, <<>>).

make(Topic, Payload) -> make(?MODULE, 0, Topic, Payload).

make(From, Topic, Payload) -> make(From, 0, Topic, Payload).

make(From, Qos, Topic, Payload) ->
    #message{id = emqx_guid:gen(),
             qos = Qos,
             from = to_bin(From),
             flags = #{dup => false, retain => false, sys => false},
             headers = #{},
             topic = to_bin(Topic),
             payload = to_payload(Payload),
             timestamp = erlang:system_time(millisecond)}.

id(#message{id = Id}) -> Id;
id(_) -> undefined.

qos(#message{qos = Q}) -> Q;
qos(_) -> 0.

flags(#message{flags = F}) -> F;
flags(_) -> #{}.

headers(#message{headers = H}) -> H;
headers(_) -> #{}.

topic(#message{topic = T}) -> T;
topic(_) -> <<>>.

payload(#message{payload = P}) -> P;
payload(_) -> <<>>.

from(#message{from = F}) -> F;
from(_) -> <<"inproc">>.

timestamp(#message{timestamp = T}) -> T;
timestamp(_) -> 0.

set_topic(Msg, Topic) -> Msg#message{topic = to_bin(Topic)}.
set_payload(Msg, Payload) -> Msg#message{payload = to_payload(Payload)}.
set_qos(Msg, Qos) -> Msg#message{qos = Qos}.
set_headers(Msg, Headers) -> Msg#message{headers = Headers}.

is_message(Msg) -> is_record(Msg, message).

to_map(Msg) when is_record(Msg, message) -> dgiot_broker_record_bridge:from_message(Msg);
to_map(Other) -> {error, {bad_message, Other}}.

ensure_id(undefined) -> emqx_guid:gen();
ensure_id(Id) -> Id.

ensure_flags(F) when is_map(F) -> F;
ensure_flags(_) -> #{dup => false, retain => false, sys => false}.

ensure_headers(H) when is_map(H) -> H;
ensure_headers(_) -> #{}.

ensure_ts(T) when is_integer(T) -> T;
ensure_ts(_) -> erlang:system_time(millisecond).

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> iolist_to_binary(L);
to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_bin(Other) -> iolist_to_binary(io_lib:format("~p", [Other])).

to_payload(B) when is_binary(B) -> B;
to_payload(M) when is_map(M) -> dgiot_json:encode(M);
to_payload(Other) -> to_bin(Other).
