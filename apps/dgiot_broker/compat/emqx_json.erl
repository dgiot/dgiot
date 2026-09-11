%% @doc 同名承接：emqx_json（影子内核，刀 4）。
%% 转发到 dgiot 既有的 json 封装（dgiot_json），保证同一套编解码语义。
%% 未支持的可选参数显式报错，不静默忽略。
-module(emqx_json).

-export([encode/1, encode/2, decode/1, decode/2,
         safe_encode/1, safe_encode/2, safe_decode/1, safe_decode/2]).

encode(Term) -> dgiot_json:encode(Term).

encode(Term, Opts) when is_list(Opts) ->
    %% 本项目只用默认选项；显式列出被忽略的选项而不是假装支持
    case Opts of
        [] -> dgiot_json:encode(Term);
        _ -> {error, {unsupported_options, Opts}}
    end.

decode(Bin) -> dgiot_json:decode(Bin).

decode(Bin, Opts) when is_list(Opts) ->
    case Opts of
        [] -> dgiot_json:decode(Bin);
        _ -> {error, {unsupported_options, Opts}}
    end.

%% safe_*：永不抛，失败回 {error, _}（EMQX 语义）
safe_encode(Term) ->
    try {ok, dgiot_json:encode(Term)} catch C:R -> {error, {C, R}} end.
safe_encode(Term, _Opts) -> safe_encode(Term).

safe_decode(Json) ->
    try {ok, dgiot_json:decode(Json)} catch C:R -> {error, {C, R}} end.
safe_decode(Json, _Opts) -> safe_decode(Json).
