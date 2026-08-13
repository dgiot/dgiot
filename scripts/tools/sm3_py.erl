-module(sm3_py).
-export([test/0, hash/1]).

%% SM3 - verified against Python reference

rotl(X, N) ->
    ((X bsl N) bor (X bsr (32 - N))) band 16#FFFFFFFF.

p0(X) -> X bxor rotl(X, 9) bxor rotl(X, 17).
p1(X) -> X bxor rotl(X, 15) bxor rotl(X, 23).

ffj(X, Y, Z, J) when J < 16 -> X bxor Y bxor Z;
ffj(X, Y, Z, _J) -> (X band Y) bor (X band Z) bor (Y band Z).

ggj(X, Y, Z, J) when J < 16 -> X bxor Y bxor Z;
ggj(X, Y, Z, _J) -> (X band Y) bor (((bnot X) band 16#FFFFFFFF) band Z).

sm3_pad(Data) ->
    Len = byte_size(Data),
    BitLen = Len * 8,
    K = case (BitLen + 1) rem 512 =< 448 of
        true -> 448 - (BitLen + 1) rem 512;
        false -> 512 + 448 - (BitLen + 1) rem 512
    end,
    Padded = <<Data/bitstring, 1:1, 0:K, BitLen:64/big>>,
    Size = bit_size(Padded) div 8,
    <<Bin:Size/binary>> = Padded,
    Bin.

expand_w(Block) ->
    W0 = [W || <<W:32/big>> <= Block],
    W = W0 ++ lists:duplicate(52, 0),
    W1 = fill_w(W, 16),
    W1.

fill_w(W, 68) -> W;
fill_w(W, J) ->
    W16 = lists:nth(J-16+1, W),
    W9  = lists:nth(J-9+1, W),
    W3  = lists:nth(J-3+1, W),
    W13 = lists:nth(J-13+1, W),
    W6  = lists:nth(J-6+1, W),
    Val = p1(W16 bxor W9 bxor rotl(W3, 15)) bxor rotl(W13, 7) bxor W6,
    fill_w(lists:sublist(W, J) ++ [Val band 16#FFFFFFFF], J+1).

w1(W) ->
    [lists:nth(J+1, W) bxor lists:nth(J+4+1, W) || J <- lists:seq(0, 63)].

compress(<<Block:64/binary, Rest/binary>>, V) ->
    W = expand_w(Block),
    W1 = w1(W),
    [A0,B0,C0,D0,E0,F0,G0,H0] = [X || <<X:32/big>> <= V],
    {A,B,C,D,E,F,G,H} = compress_rounds(A0,B0,C0,D0,E0,F0,G0,H0, W, W1, 0),
    V1 = <<(A0 bxor A):32/big, (B0 bxor B):32/big, (C0 bxor C):32/big, (D0 bxor D):32/big,
           (E0 bxor E):32/big, (F0 bxor F):32/big, (G0 bxor G):32/big, (H0 bxor H):32/big>>,
    compress(Rest, V1);
compress(<<>>, V) -> V.

compress_rounds(A,B,C,D,E,F,G,H, _W, _W1, 64) -> {A,B,C,D,E,F,G,H};
compress_rounds(A,B,C,D,E,F,G,H, W, W1, J) ->
    Tj = case J < 16 of true -> 16#79CC4519; false -> 16#7A879D8A end,
    SS1 = rotl((rotl(A, 12) + E + rotl(Tj, J)) band 16#FFFFFFFF, 7),
    SS2 = SS1 bxor rotl(A, 12),
    TT1 = (ffj(A,B,C,J) + D + SS2 + lists:nth(J+1, W1)) band 16#FFFFFFFF,
    TT2 = (ggj(E,F,G,J) + H + SS1 + lists:nth(J+1, W)) band 16#FFFFFFFF,
    compress_rounds(TT1, A, rotl(B,9), C, p0(TT2), E, rotl(F,19), G, W, W1, J+1).

hash(Data) ->
    Padded = sm3_pad(Data),
    IV = <<16#7380166F:32, 16#4914B2B9:32, 16#172442D7:32, 16#DA8A0600:32,
           16#A96F30BC:32, 16#163138AA:32, 16#E38DEE4D:32, 16#B0FB0E4E:32>>,
    compress(Padded, IV).

test() ->
    R = hash(<<"abc">>),
    H = lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= R]),
    Expected = <<"66c7f0f462eeedd9d1f2d46bdc10e4e24167c4875cf2f7a2297da02b8f4ba8e0">>,
    io:format("SM3(abc) = ~s~n", [H]),
    io:format("Expected = ~s~n", [Expected]),
    Match = case list_to_binary(H) =:= Expected of true -> "YES!"; false -> "NO" end,
    io:format("MATCH: ~s~n", [Match]).
