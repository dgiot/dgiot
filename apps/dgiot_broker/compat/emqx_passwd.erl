%% @doc 同名承接：emqx_passwd（口令散列）→ crypto 实现（盐 + 迭代）。
%% 仅兼容「同进程内 hash/check」的自洽场景；与 EMQX 存量库的散列格式不互通，
%% 此处显式声明，避免误以为是同一格式。
-module(emqx_passwd).

-export([hash/1, hash/2, check/2, check/3]).

hash(Password) -> hash(Password, default_salt()).

hash(Password, Salt) ->
    Bin = iolist_to_binary([Salt, Password]),
    hex(crypto:hash(sha256, Bin)).

check(Password, Hash) -> check(Password, default_salt(), Hash).

check(Password, Salt, Hash) ->
    hash(Password, Salt) =:= Hash.

default_salt() -> <<"dgiot">>.

hex(Bin) -> binary:encode_hex(Bin).
