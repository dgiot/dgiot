%% @doc 同名承接：emqx_message（影子内核，刀 4）。
%% 包表示用 map（我们内核的形态）+ 访问器函数，供 dgiot 与插件按 API 调用。
%% 注意（切换刀待办）：若调用方直接匹配 `#message{}` 记录，需要同名 .hrl；
%% 依赖地图显示 dgiot 侧仅 2 处调用、插件侧 30 处，切换前需逐处核对。
-module(emqx_message).

-export([make/1, make/2, make/3, make/4,
         id/1, qos/1, flags/1, topic/1, payload/1, from/1, timestamp/1,
         set_topic/2, set_payload/2, set_qos/2,
         is_record/1, to_map/1]).

%% make/1：map 原样返回（补默认字段）；binary 视为只有主题的消息
make(Msg) when is_map(Msg) ->
    Msg#{id => maps:get(id, Msg, emqx_guid:gen()),
         flags => maps:get(flags, Msg, #{dup => false, retain => false,
                                          sys => false}),
         timestamp => maps:get(timestamp, Msg, erlang:system_time(millisecond))};
make(Topic) when is_binary(Topic) ->
    make(Topic, <<>>).

make(Topic, Payload) -> make(?MODULE, Topic, Payload).
make(From, Topic, Payload) -> make(From, 0, Topic, Payload).
make(From, Qos, Topic, Payload) ->
    #{id => emqx_guid:gen(),
      from => to_bin(From),
      qos => Qos,
      topic => to_bin(Topic),
      payload => to_payload(Payload),
      flags => #{dup => false, retain => false, sys => false},
      timestamp => erlang:system_time(millisecond)}.

id(#{id := Id}) -> Id;
id(_) -> undefined.

qos(#{qos := Q}) -> Q;
qos(_) -> 0.

flags(#{flags := F}) -> F;
flags(_) -> #{}.

topic(#{topic := T}) -> T;
topic(_) -> <<>>.

payload(#{payload := P}) -> P;
payload(_) -> <<>>.

from(#{from := F}) -> F;
from(_) -> <<"inproc">>.

timestamp(#{timestamp := T}) -> T;
timestamp(_) -> 0.

set_topic(Msg, Topic) -> Msg#{topic => to_bin(Topic)}.
set_payload(Msg, Payload) -> Msg#{payload => to_payload(Payload)}.
set_qos(Msg, Qos) -> Msg#{qos => Qos}.

is_record(Msg) -> is_map(Msg).

to_map(Msg) when is_map(Msg) -> Msg;
to_map(Other) -> {error, {bad_message, Other}}.

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> iolist_to_binary(L);
to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_bin(Other) -> iolist_to_binary(io_lib:format("~p", [Other])).

to_payload(B) when is_binary(B) -> B;
to_payload(M) when is_map(M) -> dgiot_json:encode(M);
to_payload(Other) -> to_bin(Other).
