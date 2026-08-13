-module(sm4_py_verified).
-export([test/0, sm4_encrypt_ecb/2, sm4_decrypt_ecb/2]).
%% SM4 - Verified against Python reference implementation
%% GB/T 32907-2016

%% ===== SM4 S-box (verified) =====
sm4_sbox() ->
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

%% S-box lookup (0-indexed list)
sbox(I) -> lists:nth(I+1, sm4_sbox()).

%% 32-bit rotate left
rotl(X, N) -> ((X bsl N) bor (X bsr (32 - N))) band 16#FFFFFFFF.

%% Round function T (L transform)
sm4_L(Y) ->
    Y bxor rotl(Y, 2) bxor rotl(Y, 10) bxor rotl(Y, 18) bxor rotl(Y, 24).

%% Key schedule T' (L' transform)
sm4_Lp(Y) ->
    Y bxor rotl(Y, 13) bxor rotl(Y, 23).

%% tau: S-box substitution on 4 bytes, returns 32-bit word
tau(X) ->
    A0 = (X band 16#FF000000) bsr 24,
    A1 = (X band 16#00FF0000) bsr 16,
    A2 = (X band 16#0000FF00) bsr 8,
    A3 = X band 16#000000FF,
    (sbox(A0) bsl 24) bor (sbox(A1) bsl 16) bor (sbox(A2) bsl 8) bor sbox(A3).

%% T(.) = L(tau(.))
sm4_T(X) -> sm4_L(tau(X)).

%% T'(.) = L'(tau(.))
sm4_Tp(X) -> sm4_Lp(tau(X)).

%% Key schedule
sm4_key_schedule(MK) ->
    FK = [16#A3B1BAC6, 16#56AA3350, 16#677D9197, 16#B27022DC],
    CK = [16#00070E15,16#1C232A31,16#383F464D,16#545B6269,
          16#70777E85,16#8C939AA1,16#A8AFB6BD,16#C4CBD2D9,
          16#E0E7EEF5,16#FC030A11,16#181F262D,16#343B4249,
          16#50575E65,16#6C737A81,16#888F969D,16#A4ABB2B9,
          16#C0C7CED5,16#DCE3EAF1,16#F8FF060D,16#141B2229,
          16#30373E45,16#4C535A61,16#686F767D,16#848B9299,
          16#A0A7AEB5,16#BCC3CAD1,16#D8DFE6ED,16#F4FB0209,
          16#10171E25,16#2C333A41,16#484F565D,16#646B7279],
    K0 = lists:nth(1, MK) bxor lists:nth(1, FK),
    K1 = lists:nth(2, MK) bxor lists:nth(2, FK),
    K2 = lists:nth(3, MK) bxor lists:nth(3, FK),
    K3 = lists:nth(4, MK) bxor lists:nth(4, FK),
    ks_loop(K0, K1, K2, K3, CK, 0, []).

ks_loop(_K0, _K1, _K2, _K3, _CK, 32, Acc) -> lists:reverse(Acc);
ks_loop(K0, K1, K2, K3, [CKi|Rest], I, Acc) ->
    T = sm4_Tp(K1 bxor K2 bxor K3 bxor CKi),
    K4 = K0 bxor T,
    ks_loop(K1, K2, K3, K4, Rest, I+1, [K4|Acc]).

%% Encrypt one 128-bit block
sm4_encrypt_block(X, RK) ->
    sm4_rounds_enc(lists:nth(1,X), lists:nth(2,X), lists:nth(3,X), lists:nth(4,X), RK, 0).

sm4_rounds_enc(X0, X1, X2, X3, _RK, 32) ->
    (X3 bsl 96) bor (X2 bsl 64) bor (X1 bsl 32) bor X0;
sm4_rounds_enc(X0, X1, X2, X3, [_RK0|Rest], I) ->
    T = sm4_T(X1 bxor X2 bxor X3 bxor _RK0),
    sm4_rounds_enc(X1, X2, X3, X0 bxor T, Rest, I+1).

%% Public API
sm4_encrypt_ecb(Key, Plaintext) ->
    KeyInts = plain_to_ints(Key),
    Plaintext2Ints = plain_to_ints(Plaintext),
    RK = sm4_key_schedule(KeyInts),
    CipherInt = sm4_encrypt_block(Plaintext2Ints, RK),
    int_to_bin(CipherInt).

sm4_decrypt_ecb(Key, Ciphertext) ->
    KeyInts = plain_to_ints(Key),
    CipherInts = plain_to_ints(Ciphertext),
    RK = sm4_key_schedule(KeyInts),
    PlainInt = sm4_encrypt_block(CipherInts, lists:reverse(RK)),
    int_to_bin(PlainInt).

plain_to_ints(<<A:32/big, B:32/big, C:32/big, D:32/big>>) -> [A,B,C,D].

int_to_bin(X) -> <<X:128/big>>.

%% Test
test() ->
    Key = <<16#01,16#23,16#45,16#67,16#89,16#ab,16#cd,16#ef,
           16#fe,16#dc,16#ba,16#98,16#76,16#54,16#32,16#10>>,
    Plain = <<16#01,16#23,16#45,16#67,16#89,16#ab,16#cd,16#ef,
              16#fe,16#dc,16#ba,16#98,16#76,16#54,16#32,16#10>>,
    Expected = <<16#68,16#1e,16#df,16#34,16#d2,16#06,16#96,16#5e,
                 16#86,16#b3,16#e9,16#4f,16#53,16#6e,16#42,16#46>>,

    Cipher = sm4_encrypt_ecb(Key, Plain),
    H = fun(B) -> lists:flatten([io_lib:format("~2.16.0b",[X]) || <<X>> <= B]) end,

    io:format("Plain:  ~s~n", [H(Plain)]),
    io:format("Cipher: ~s~n", [H(Cipher)]),
    io:format("Expect: ~s~n", [H(Expected)]),
    io:format("Match: ~s~n", [if Cipher =:= Expected -> "YES!"; true -> "NO" end]).
