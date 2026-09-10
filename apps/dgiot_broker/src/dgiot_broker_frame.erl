%% @doc MQTT 3.1.1 编解码（刀 2，纯函数模块，无进程）。
%%
%% 设计口径：
%%   * 只做 3.1.1——dgiot 与我们的 SDK/模拟器全用 paho 3.1.1（实测 QoS2/will 零使用）；
%%   * 流式安全：decode/1 对半包返回 {more, _}，绝不猜、绝不吞；
%%   * 失败必响：畸形包一律 {error, Reason}，无任何静默降级；
%%   * 对拍口径：编码结果必须被真实 EMQX 接受（EMQX 在位期当裁判），
%%     解码必须能吃 paho 真实产生的字节（见 test/ 与 scripts 对拍）。
%%
%% 包表示（map，字段名即语义）：
%%   connect:     #{type, proto_name, proto_level, clean_start, keepalive,
%%                  client_id, will, username, password}
%%   connack:     #{type, session_present, return_code}
%%   publish:     #{type, dup, qos, retain, topic, packet_id, payload}
%%   puback:      #{type, packet_id}
%%   subscribe:   #{type, packet_id, topics}   %% topics = [{Filter, Qos}]
%%   suback:      #{type, packet_id, return_codes}
%%   unsubscribe: #{type, packet_id, topics}
%%   unsuback:    #{type, packet_id}
%%   pingreq | pingresp | disconnect: #{type}
-module(dgiot_broker_frame).

-export([decode/1, encode/1, packet_type/1,
         encode_remaining_length/1, decode_remaining_length/1,
         type_code/1, code_type/1]).

-define(MAX_REMAINING, 268435455).  %% 4 字节 varint 上限 (256MB-1)

%% ==================================================================
%% 解码
%% ==================================================================

-spec decode(binary()) ->
    {ok, map(), binary()} | {more, non_neg_integer()} | {error, term()}.
decode(<<>>) ->
    {more, 2};   %% 至少需要固定头首字节 + 1 字节变长长度
decode(<<_Flags:8, Rest/binary>> = Bin) ->
    case decode_remaining_length(Rest) of
        {more, Need} ->
            {more, Need};
        {error, Reason} ->
            {error, Reason};
        {ok, Len, AfterLen} ->
            case AfterLen of
                <<Body:Len/binary, Tail/binary>> ->
                    case decode_packet(Bin, Body) of
                        {ok, Packet} -> {ok, Packet, Tail};
                        {error, R} -> {error, R}
                    end;
                _ ->
                    {more, Len - byte_size(AfterLen)}
            end
    end.

%% 固定头首字节 + 变长头 + 体 → 包 map
decode_packet(<<TypeCode:4, Flags:4, _/binary>>, Body) ->
    case code_type(TypeCode) of
        unknown ->
            {error, {unknown_packet_type, TypeCode}};
        Type ->
            decode_body(Type, Flags, Body)
    end.

decode_body(connect, _Flags, Body) ->
    case Body of
        <<4:16, "MQTT", Level:8, Rest/binary>> ->
            decode_connect(Level, Rest);
        _ ->
            {error, {bad_connect, Body}}
    end;
decode_body(connack, _Flags, <<AckFlags:8, Code:8>>) ->
    {ok, #{type => connack,
           session_present => (AckFlags band 16#01) =:= 1,
           return_code => Code}};
decode_body(connack, _F, Body) ->
    {error, {bad_connack, Body}};
decode_body(publish, Flags, Body) ->
    Dup = (Flags band 16#08) =/= 0,
    Qos = (Flags band 16#06) bsr 1,
    Retain = (Flags band 16#01) =:= 1,
    case Qos of
        3 -> {error, {bad_publish_qos, Qos}};
        _ -> decode_publish(Dup, Qos, Retain, Body)
    end;
decode_body(puback, _F, <<Id:16>>) ->
    {ok, #{type => puback, packet_id => Id}};
decode_body(puback, _F, Body) ->
    {error, {bad_puback, Body}};
decode_body(subscribe, _F, <<Id:16, Rest/binary>>) ->
    decode_topic_qos_list(Id, Rest, [], subscribe);
decode_body(subscribe, _F, Body) ->
    {error, {bad_subscribe, Body}};
decode_body(suback, _F, <<Id:16, Codes/binary>>) ->
    {ok, #{type => suback, packet_id => Id, return_codes => binary_to_list(Codes)}};
decode_body(suback, _F, Body) ->
    {error, {bad_suback, Body}};
decode_body(unsubscribe, _F, <<Id:16, Rest/binary>>) ->
    decode_topic_list(Id, Rest, []);
decode_body(unsubscribe, _F, Body) ->
    {error, {bad_unsubscribe, Body}};
decode_body(unsuback, _F, <<Id:16>>) ->
    {ok, #{type => unsuback, packet_id => Id}};
decode_body(unsuback, _F, Body) ->
    {error, {bad_unsuback, Body}};
decode_body(pingreq, _F, <<>>) ->
    {ok, #{type => pingreq}};
decode_body(pingresp, _F, <<>>) ->
    {ok, #{type => pingresp}};
decode_body(disconnect, _F, <<>>) ->
    {ok, #{type => disconnect}};
decode_body(Type, _F, Body) ->
    {error, {bad_body, Type, Body}}.

decode_connect(Level, <<Flags:8, Keepalive:16, Payload/binary>>) ->
    CleanStart = (Flags band 16#02) =/= 0,
    WillFlag = (Flags band 16#04) =/= 0,
    WillQos = (Flags band 16#18) bsr 3,
    WillRetain = (Flags band 16#20) =/= 0,
    HasUser = (Flags band 16#80) =/= 0,
    HasPass = (Flags band 16#40) =/= 0,
    case take_string(Payload) of
        {error, R} -> {error, R};
        {ok, ClientId, Rest1} ->
            case decode_will(WillFlag, WillQos, WillRetain, Rest1) of
                {error, R} -> {error, R};
                {ok, Will, Rest2} ->
                    case decode_creds(HasUser, HasPass, Rest2) of
                        {error, R} -> {error, R};
                        {ok, User, Pass, _Rest3} ->
                            {ok, #{type => connect,
                                   proto_name => <<"MQTT">>,
                                   proto_level => Level,
                                   clean_start => CleanStart,
                                   keepalive => Keepalive,
                                   client_id => ClientId,
                                   will => Will,
                                   username => User,
                                   password => Pass}}
                    end
            end
    end;
decode_connect(_Level, Body) ->
    {error, {bad_connect_flags, Body}}.

decode_will(false, _Qos, _Retain, Rest) ->
    {ok, undefined, Rest};
decode_will(true, Qos, Retain, Rest) ->
    case take_string(Rest) of
        {error, R} -> {error, R};
        {ok, Topic, Rest1} ->
            case take_string(Rest1) of
                {error, R} -> {error, R};
                {ok, Payload, Rest2} ->
                    {ok, #{topic => Topic, payload => Payload,
                           qos => Qos, retain => Retain}, Rest2}
            end
    end.

decode_creds(false, false, Rest) -> {ok, undefined, undefined, Rest};
decode_creds(true, false, Rest) ->
    case take_string(Rest) of
        {ok, U, R} -> {ok, U, undefined, R};
        {error, E} -> {error, E}
    end;
decode_creds(false, true, Rest) ->
    case take_string(Rest) of
        {ok, P, R} -> {ok, undefined, P, R};
        {error, E} -> {error, E}
    end;
decode_creds(true, true, Rest) ->
    case take_string(Rest) of
        {ok, U, R1} ->
            case take_string(R1) of
                {ok, P, R2} -> {ok, U, P, R2};
                {error, E} -> {error, E}
            end;
        {error, E} -> {error, E}
    end.

decode_publish(Dup, 0, Retain, Body) ->
    case take_string(Body) of
        {error, R} -> {error, R};
        {ok, Topic, Payload} ->
            {ok, #{type => publish, dup => Dup, qos => 0, retain => Retain,
                   topic => Topic, packet_id => undefined, payload => Payload}}
    end;
decode_publish(Dup, Qos, Retain, Body) ->
    case take_string(Body) of
        {error, R} -> {error, R};
        {ok, Topic, <<Id:16, Payload/binary>>} ->
            {ok, #{type => publish, dup => Dup, qos => Qos, retain => Retain,
                   topic => Topic, packet_id => Id, payload => Payload}};
        {ok, _Topic, _Short} ->
            {error, {bad_publish_id, Body}}
    end.

decode_topic_qos_list(Id, <<>>, Acc, subscribe) ->
    case Acc of
        [] -> {error, empty_subscribe};
        _ -> {ok, #{type => subscribe, packet_id => Id,
                    topics => lists:reverse(Acc)}}
    end;
decode_topic_qos_list(Id, Bin, Acc, subscribe) ->
    case take_string(Bin) of
        {error, R} -> {error, R};
        {ok, Filter, <<Qos:8, Rest/binary>>} when Qos =< 2 ->
            decode_topic_qos_list(Id, Rest, [{Filter, Qos} | Acc], subscribe);
        {ok, _F, <<Qos:8, _/binary>>} ->
            {error, {bad_subscribe_qos, Qos}};
        {ok, _F, _} ->
            {error, {bad_subscribe_entry, Bin}}
    end.

decode_topic_list(Id, <<>>, Acc) ->
    case Acc of
        [] -> {error, empty_unsubscribe};
        _ -> {ok, #{type => unsubscribe, packet_id => Id,
                    topics => lists:reverse(Acc)}}
    end;
decode_topic_list(Id, Bin, Acc) ->
    case take_string(Bin) of
        {error, R} -> {error, R};
        {ok, Filter, Rest} -> decode_topic_list(Id, Rest, [Filter | Acc])
    end.

%% 长度前缀字符串（MQTT 用 2 字节长度）
take_string(<<Len:16, Str:Len/binary, Rest/binary>>) ->
    {ok, Str, Rest};
take_string(Bin) when byte_size(Bin) < 2 ->
    {error, {truncated_string, Bin}};
take_string(<<Len:16, Rest/binary>>) ->
    {error, {truncated_string_body, Len, byte_size(Rest)}}.

%% ==================================================================
%% 编码
%% ==================================================================

-spec encode(map()) -> iodata().
encode(#{type := Type} = P) ->
    Flags = flags_of(P),
    Body = encode_body(Type, P),
    [<<(type_code(Type)):4, Flags:4>>, encode_remaining_length(iolist_size(Body)), Body].

flags_of(#{type := publish} = P) ->
    Dup = case maps:get(dup, P, false) of true -> 1; _ -> 0 end,
    Qos = maps:get(qos, P, 0),
    Retain = case maps:get(retain, P, false) of true -> 1; _ -> 0 end,
    (Dup bsl 3) bor (Qos bsl 1) bor Retain;
flags_of(#{type := T}) when T =:= subscribe; T =:= unsubscribe -> 2;
flags_of(_) -> 0.

encode_body(connect, P) ->
    CleanStart = case maps:get(clean_start, P, true) of true -> 1; _ -> 0 end,
    Will = maps:get(will, P, undefined),
    User = maps:get(username, P, undefined),
    Pass = maps:get(password, P, undefined),
    {WillFlag, WillQos, WillRetain, WillBody} =
        case Will of
            undefined -> {0, 0, 0, []};
            W -> Q = maps:get(qos, W, 0),
                 R = case maps:get(retain, W, false) of true -> 1; _ -> 0 end,
                 {1, Q, R, [encode_string(maps:get(topic, W)),
                            encode_string(maps:get(payload, W, <<>>))]}
        end,
    UserFlag = case User of undefined -> 0; _ -> 1 end,
    PassFlag = case Pass of undefined -> 0; _ -> 1 end,
    Flags = (UserFlag bsl 7) bor (PassFlag bsl 6) bor (WillRetain bsl 5)
            bor (WillQos bsl 3) bor (WillFlag bsl 2) bor (CleanStart bsl 1),
    [encode_string(<<"MQTT">>),
     <<4:8, Flags:8, (maps:get(keepalive, P, 60)):16>>,
     encode_string(maps:get(client_id, P, <<>>)), WillBody,
     case User of undefined -> []; _ -> encode_string(User) end,
     case Pass of undefined -> []; _ -> encode_string(Pass) end];
encode_body(connack, P) ->
    Session = case maps:get(session_present, P, false) of true -> 1; _ -> 0 end,
    [<<Session:8, (maps:get(return_code, P, 0)):8>>];
encode_body(publish, P) ->
    Id = case maps:get(qos, P, 0) of
             0 -> [];
             _ -> [<<(maps:get(packet_id, P, 1)):16>>]
         end,
    [encode_string(maps:get(topic, P)), Id, maps:get(payload, P, <<>>)];
encode_body(puback, P) -> [<<(maps:get(packet_id, P)):16>>];
encode_body(unsuback, P) -> [<<(maps:get(packet_id, P)):16>>];
encode_body(subscribe, P) ->
    Entries = [[encode_string(F), <<Q:8>>] || {F, Q} <- maps:get(topics, P)],
    [<<(maps:get(packet_id, P)):16>>, Entries];
encode_body(suback, P) ->
    [<<(maps:get(packet_id, P)):16>>,
     [<<C:8>> || C <- maps:get(return_codes, P)]];
encode_body(unsubscribe, P) ->
    [<<(maps:get(packet_id, P)):16>>,
     [encode_string(F) || F <- maps:get(topics, P)]];
encode_body(T, _P) when T =:= pingreq; T =:= pingresp; T =:= disconnect -> [].

encode_string(Bin) when is_binary(Bin) ->
    [<<(byte_size(Bin)):16>>, Bin];
encode_string(List) when is_list(List) ->
    encode_string(unicode:characters_to_binary(List)).

%% ==================================================================
%% 变长长度（1-4 字节）
%% ==================================================================

-spec encode_remaining_length(non_neg_integer()) -> binary().
encode_remaining_length(Len) when Len >= 0, Len =< ?MAX_REMAINING ->
    encode_rl(Len).
encode_rl(Len) when Len < 128 ->
    <<Len>>;
encode_rl(Len) ->
    <<(Len rem 128 bor 128), (encode_rl(Len div 128))/binary>>.

-spec decode_remaining_length(binary()) ->
    {ok, non_neg_integer(), binary()} | {more, pos_integer()} | {error, term()}.
decode_remaining_length(Bin) ->
    decode_rl(Bin, 0, 0, 0).

decode_rl(_Bin, _Shift, _Acc, 4) ->
    {error, remaining_length_too_long};
decode_rl(<<>>, _Shift, _Acc, _N) ->
    {more, 1};
decode_rl(<<Byte:8, Rest/binary>>, Shift, Acc, N) ->
    Value = Acc bor ((Byte band 16#7F) bsl Shift),
    case Byte band 16#80 of
        0 -> {ok, Value, Rest};
        _ -> decode_rl(Rest, Shift + 7, Value, N + 1)
    end.

%% ==================================================================
%% 类型码
%% ==================================================================

type_code(connect) -> 1;
type_code(connack) -> 2;
type_code(publish) -> 3;
type_code(puback) -> 4;
type_code(subscribe) -> 8;
type_code(suback) -> 9;
type_code(unsubscribe) -> 10;
type_code(unsuback) -> 11;
type_code(pingreq) -> 12;
type_code(pingresp) -> 13;
type_code(disconnect) -> 14.

code_type(1) -> connect;
code_type(2) -> connack;
code_type(3) -> publish;
code_type(4) -> puback;
code_type(8) -> subscribe;
code_type(9) -> suback;
code_type(10) -> unsubscribe;
code_type(11) -> unsuback;
code_type(12) -> pingreq;
code_type(13) -> pingresp;
code_type(14) -> disconnect;
code_type(_) -> unknown.

%% 只读首字节即可判类型（用于流式分发与观测）
-spec packet_type(binary()) -> atom() | {error, term()}.
packet_type(<<TypeCode:4, _:4, _/binary>>) -> code_type(TypeCode);
packet_type(_) -> {error, too_short}.
