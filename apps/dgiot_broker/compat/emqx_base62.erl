%% @doc 同名承接：emqx_base62（整数↔Base62）→ 自包含实现。
-module(emqx_base62).

-export([encode/1, decode/1]).

encode(Int) when is_integer(Int), Int >= 0 ->
    encode(Int, []).

decode(Bin) when is_binary(Bin) ->
    decode(Bin, 0).

encode(0, []) -> <<"0">>;
encode(0, Acc) -> iolist_to_binary(Acc);
encode(N, Acc) ->
    encode(N div 62, [digit(N rem 62) | Acc]).

decode(<<>>, Acc) -> Acc;
decode(<<C, Rest/binary>>, Acc) ->
    decode(Rest, Acc * 62 + val(C)).

digit(D) when D < 10 -> $0 + D;
digit(D) when D < 36 -> $A + (D - 10);
digit(D) -> $a + (D - 36).

val(C) when C >= $0, C =< $9 -> C - $0;
val(C) when C >= $A, C =< $Z -> C - $A + 10;
val(C) when C >= $a, C =< $z -> C - $a + 36.
