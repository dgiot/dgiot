%% @doc 同名承接：emqx_guid（影子内核，刀 4）。
%% EMQX 的 guid 是 128bit 二进制；我们保持同形态（二进制/十六进制/Base62）。
-module(emqx_guid).

-export([gen/0, gen_hex/0, gen_timestamp/0, to_base62/1, to_hex/1,
         from_hex/1, is_guid/1]).

gen() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    %% 版本/变体位按 RFC4122 v4 摆放，便于与外部工具互认
    <<A:32, B:16, 4:4, (C band 16#0FFF):12, 2:2, (D band 16#3FFF):14, E:48>>.

gen_hex() ->
    to_hex(gen()).

gen_timestamp() ->
    erlang:system_time(millisecond).

to_hex(Bin) when is_binary(Bin) ->
    %% OTP 24：encode_hex/1 只接收二进制且输出大写，转小写保持一致
    string:lowercase(binary:encode_hex(Bin)).

from_hex(Hex) when is_binary(Hex) ->
    binary:decode_hex(Hex).

to_base62(<<Bin:128>>) ->
    base62(Bin, <<>>).

is_guid(B) when is_binary(B), byte_size(B) =:= 16 -> true;
is_guid(_) -> false.

base62(0, Acc) -> Acc;
base62(N, Acc) ->
    Digit = N rem 62,
    Ch = case Digit of
             D when D < 10 -> $0 + D;
             D when D < 36 -> $A + (D - 10);
             D -> $a + (D - 36)
         end,
    base62(N div 62, <<Ch, Acc/binary>>).
