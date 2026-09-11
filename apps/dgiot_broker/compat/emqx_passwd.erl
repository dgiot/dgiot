%% @doc 同名承接：emqx_passwd → 对齐 EMQX 4.4 真 API（hash(算法, 口令)）。
%% 算法：plain | md5 | sha | sha256 | sha512 | pbkdf2（+ 兼容 3 参调用）。
-module(emqx_passwd).

-export([hash/2, hash/3, check_password/2, check_password/3]).

hash(plain, Password) -> iolist_to_binary(Password);
hash(md5, Password) -> hex(crypto:hash(md5, Password));
hash(sha, Password) -> hex(crypto:hash(sha, Password));
hash(sha256, Password) -> hex(crypto:hash(sha256, Password));
hash(sha512, Password) -> hex(crypto:hash(sha512, Password));
hash(pbkdf2, {Salt, Password, Macfun, Iterations, Dklen}) ->
    hex(crypto:pbkdf2_hmac(Macfun, Password, Salt, Iterations, Dklen));
hash(Algo, Password) ->
    {error, {unsupported_hash_algo, Algo, Password}}.

%% 兼容 3 参调用（(Algo, Password, Salt)）：忽略第三参
hash(Algo, Password, _Salt) -> hash(Algo, Password).

check_password(Password, Hash) ->
    check_password(sha256, Password, Hash).

check_password(Algo, Password, Hash) ->
    try hash(Algo, Password) =:= Hash
    catch _:_ -> false
    end.

hex(Bin) -> binary:encode_hex(Bin).
