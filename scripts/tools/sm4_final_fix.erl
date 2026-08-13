-module(sm4_final_fix).
-export([test/0]).

test() ->
    %% Load dgiot_sm
    {ok, _, Bin} = compile:file("/root/gitee/dgiot/apps/dgiot_device/src/dgiot_sm.erl", [binary, return_errors]),
    {module, dgiot_sm} = code:load_binary(dgiot_sm, "dgiot_sm.erl", Bin),

    %% Verify S-box value S[0] = 0xD6
    %% The S-box is created via array:from_list
    %% array:get(0, S) should return 0xD6

    %% Direct SM4 test: encrypt one block
    %% Use Python-style: SM4 ECB without padding
    Key = <<16#01,16#23,16#45,16#67,16#89,16#ab,16#cd,16#ef,
           16#fe,16#dc,16#ba,16#98,16#76,16#54,16#32,16#10>>,
    Plain = <<16#01,16#23,16#45,16#67,16#89,16#ab,16#cd,16#ef,
              16#fe,16#dc,16#ba,16#98,16#76,16#54,16#32,16#10>>,

    %% Encrypt - should NOT pad since input is exactly 16 bytes
    Enc = dgiot_sm:sm4_encrypt(Key, Plain),
    EncOssl = dgiot_sm:sm4_openssl_encrypt(Key, Plain),

    %% Decrypt
    Dec = dgiot_sm:sm4_decrypt(Key, Enc),

    H = fun(B) -> lists:flatten([io_lib:format("~2.16.0b",[X]) || <<X>> <= B]) end,

    io:format("Key:     ~s~n", [H(Key)]),
    io:format("Plain:   ~s~n", [H(Plain)]),
    io:format("Erlang:  ~s (~B bytes)~n", [H(Enc), byte_size(Enc)]),
    io:format("OpenSSL: ~s~n", [H(EncOssl)]),
    io:format("Decrypt: ~s~n", [if Dec =:= Plain -> "MATCH"; true -> "WRONG" end]),
    io:format("Encrypt: ~s~n", [if Enc =:= EncOssl -> "MATCH"; true ->
        %% Show byte-by-byte diff
        Diffs = [if A =:= B -> $-; true -> $X end || <<A, B>> <- list_to_binary([Enc, EncOssl])],
        io:format("  Diff:   ~s~n", [Diffs])
    end]).

hex(B) -> lists:flatten([io_lib:format("~2.16.0b",[X]) || <<X>> <= B]).
