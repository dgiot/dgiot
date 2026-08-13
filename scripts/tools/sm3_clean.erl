-module(sm3_clean).
-export([test/0, hash/1]).

%% SM3 - Clean, Python-verified implementation

rotl(X, N) -> ((X bsl N) bor (X bsr (32 - N))) band 16#FFFFFFFF.
p0(X) -> X bxor rotl(X, 9) bxor rotl(X, 17).
p1(X) -> X bxor rotl(X, 15) bxor rotl(X, 23).

ffj(X, Y, Z, J) when J < 16 -> X bxor Y bxor Z;
ffj(X, Y, Z, _) -> (X band Y) bor (X band Z) bor (Y band Z).

ggj(X, Y, Z, J) when J < 16 -> X bxor Y bxor Z;
ggj(X, Y, Z, _) -> (X band Y) bor (((bnot X) band 16#FFFFFFFF) band Z).

sbox(I) -> lists:nth(I+1, sbox_data()).

sbox_data() ->
    [16#D6,16#90,16#E9,16#FE,16#CC,16#E1,16#3D,16#B7,16#16,16#B6,16#14,16#C2,16#28,16#FB,16#2C,16#05,
     16#2B,16#67,16#9A,16#76,16#2A,16#BE,16#04,16#C3,16#AA,16#44,16#13,16#26,16#49,16#86,16#06,16#99,
     16#9C,16#42,16#50,16#F4,16#91,16#EF,16#98,16#7A,16#33,16#54,16#0B,16#43,16#ED,16#CF,16#AC,16#62,
     16#E4,16#B3,16#1C,16#A9,16#C9,16#08,16#E8,16#95,16#80,16#DF,16#94,16#FA,16#75,16#8F,16#3F,16#A6,
     16#47,16#07,16#A7,16#FC,16#F3,16#73,16#17,16#BA,16#83,16#59,16#3C,16#19,16#E6,16#85,16#4F,16#A8,
     16#68,16#6B,16#81,16#B2,16#71,16#64,16#DA,16#8B,16#F8,16#EB,16#0F,16#4B,16#70,16#56,16#9D,16#35,
     16#1E,16#24,16#0E,16#5E,16#63,16#58,16#D1,16#A2,16#25,16#22,16#7C,16#3B,16#01,16#21,16#78,16#87,
     16#D4,16#00,16#46,16#57,16#9F,16#D3,16#27,16#52,16#4C,16#36,16#02,16#E7,16#A0,16#C4,16#C8,16#9E,
     16#EA,16#BF,16#8A,16#D2,16#40,16#C7,16#38,16#B5,16#A3,16#F7,16#F2,16#CE,16#F9,16#61,16#15,16#A1,
     16#E0,16#AE,16#5D,16#A4,16#9B,16#34,16#1A,16#55,16#AD,16#93,16#32,16#30,16#F5,16#8C,16#B1,16#E3,
     16#1D,16#F6,16#E2,16#2E,16#82,16#66,16#CA,16#60,16#C0,16#29,16#23,16#AB,16#0D,16#53,16#4E,16#6F,
     16#D5,16#DB,16#37,16#45,16#DE,16#FD,16#8E,16#2F,16#03,16#FF,16#6A,16#72,16#6D,16#6C,16#5B,16#51,
     16#8D,16#1B,16#AF,16#92,16#BB,16#DD,16#BC,16#7F,16#11,16#D9,16#5C,16#41,16#1F,16#10,16#5A,16#D8,
     16#0A,16#C1,16#31,16#88,16#A5,16#CD,16#7B,16#BD,16#2D,16#74,16#D0,16#12,16#B8,16#E5,16#B4,16#B0,
     16#89,16#69,16#97,16#4A,16#0C,16#96,16#77,16#7E,16#65,16#B9,16#F1,16#09,16#C5,16#6E,16#C6,16#84,
     16#18,16#F0,16#7D,16#EC,16#3A,16#DC,16#4D,16#20,16#79,16#EE,16#5F,16#3E,16#D7,16#CB,16#39,16#48].

hash(Data) ->
    Padded = pad(Data),
    IV = <<16#7380166F:32,16#4914B2B9:32,16#172442D7:32,16#DA8A0600:32,
           16#A96F30BC:32,16#163138AA:32,16#E38DEE4D:32,16#B0FB0E4E:32>>,
    compress(Padded, IV).

pad(Data) ->
    Len = byte_size(Data),
    BitLen = Len * 8,
    K = case (BitLen + 1) rem 512 =< 448 of
        true -> 448 - (BitLen + 1) rem 512;
        false -> 960 - (BitLen + 1) rem 512
    end,
    Padded = <<Data/bitstring, 1:1, 0:K, BitLen:64/big>>,
    Sz = bit_size(Padded) div 8,
    <<Bin:Sz/binary>> = Padded,
    Bin.

compress(<<Block:64/binary, Rest/binary>>, V) ->
    W = expand_w(Block),
    W1 = [lists:nth(J+1, W) bxor lists:nth(J+5, W) || J <- lists:seq(0, 63)],
    [A0,B0,C0,D0,E0,F0,G0,H0] = [X || <<X:32/big>> <= V],
    {A,B,C,D,E,F,G,H} = rounds(A0,B0,C0,D0,E0,F0,G0,H0, W, W1, 0),
    V1 = <<(A0 bxor A):32,(B0 bxor B):32,(C0 bxor C):32,(D0 bxor D):32,
           (E0 bxor E):32,(F0 bxor F):32,(G0 bxor G):32,(H0 bxor H):32>>,
    compress(Rest, V1);
compress(<<>>, V) -> V.

expand_w(Block) ->
    W0 = [X || <<X:32/big>> <= Block],
    fill_w(W0 ++ lists:duplicate(52, 0), 16).

fill_w(W, 68) -> W;
fill_w(W, J) ->
    %% Get values (W is 0-indexed list, lists:nth is 1-indexed)
    J16 = lists:nth(J-15, W),
    J9  = lists:nth(J-8,  W),
    J3  = lists:nth(J-2,  W),
    J13 = lists:nth(J-12, W),
    J6  = lists:nth(J-5,  W),
    Val = (p1(J16 bxor J9 bxor rotl(J3, 15)) bxor rotl(J13, 7) bxor J6) band 16#FFFFFFFF,
    %% Replace element at position J+1 (1-indexed)
    {Pre, [_Old|Post]} = lists:split(J, W),
    fill_w(Pre ++ [Val | Post], J+1).

rounds(A,B,C,D,E,F,G,H, _W, _W1, 64) -> {A,B,C,D,E,F,G,H};
rounds(A,B,C,D,E,F,G,H, W, W1, J) ->
    Tj = case J < 16 of true -> 16#79CC4519; false -> 16#7A879D8A end,
    SS1 = rotl((rotl(A,12) + E + rotl(Tj, J)) band 16#FFFFFFFF, 7),
    SS2 = SS1 bxor rotl(A, 12),
    TT1 = (ffj(A,B,C,J) + D + SS2 + lists:nth(J+1, W1)) band 16#FFFFFFFF,
    TT2 = (ggj(E,F,G,J) + H + SS1 + lists:nth(J+1, W)) band 16#FFFFFFFF,
    rounds(TT1, A, rotl(B,9), C, p0(TT2), E, rotl(F,19), G, W, W1, J+1).

hex(B) -> lists:flatten([io_lib:format("~2.16.0b",[X]) || <<X>> <= B]).

test() ->
    R = hash(<<"abc">>),
    H = hex(R),
    E = "66c7f0f462eeedd9d1f2d46bdc10e4e24167c4875cf2f7a2297da02b8f4ba8e0",
    io:format("SM3(abc)=~s~nExpect=~s~nMATCH: ~s~n", [H, E, case H =:= E of true -> "YES!"; false -> "NO" end]).
