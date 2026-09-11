%% @doc 同名承接：emqx_misc（刀 6 批次 2）。工具函数，自包含实现。
-module(emqx_misc).

-export([is_sane_id/1, start_timer/1, start_timer/2, cancel_timer/1,
         proc_stats/0, merge_opts/2, ipv6_probe/0,
         hexstr2bin/1, bin2hexstr_a_f_upper/1, bin2hexstr_a_f_lower/1,
         rand_seed/0, now_ms/0]).

%% 合法的 ClientId：非空且不含控制字符（EMQX 的宽松版本）
is_sane_id(Id) when is_binary(Id), byte_size(Id) > 0 ->
    not lists:any(fun(C) -> C < 16#20 orelse C =:= 16#7F end,
                  binary_to_list(Id));
is_sane_id(_) -> false.

start_timer(Sec) -> start_timer(Sec, undefined).
start_timer(Sec, Msg) when is_integer(Sec), Sec > 0 ->
    erlang:send_after(Sec * 1000, self(), Msg);
start_timer(infinity, _Msg) -> undefined.

cancel_timer(undefined) -> ok;
cancel_timer(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref),
    ok;
cancel_timer(_) -> ok.

proc_stats() ->
    {_, Mem} = erlang:process_info(self(), memory),
    {Reductions, _} = case erlang:process_info(self(), reductions) of
                          {reductions, R} -> {R, ok};
                          _ -> {0, ok}
                      end,
    #{memory => Mem, reductions => Reductions}.

%% 选项合并（EMQX 语义：第二个覆盖第一个）
merge_opts(New, Old) when is_map(New), is_map(Old) -> maps:merge(Old, New);
merge_opts(New, _Old) when is_map(New) -> New;
merge_opts(_, Old) when is_map(Old) -> Old.

%% IPv6 可用性探测（无网络时返回 false，不抛）
ipv6_probe() ->
    case gen_tcp:listen(0, [inet6, {ip, {0, 0, 0, 0, 0, 0, 0, 1}}]) of
        {ok, S} -> gen_tcp:close(S), true;
        {error, _} -> false
    end.

%% 十六进制字符串 ↔ 二进制（EMQX 的 misc 里常用）
hexstr2bin(Str) when is_list(Str) ->
    hexstr2bin(list_to_binary(Str));
hexstr2bin(Bin) when is_binary(Bin) ->
    case binary:decode_hex(Bin) of
        B when is_binary(B) -> B;
        _ -> {error, {bad_hex, Bin}}
    end.

bin2hexstr_a_f_upper(Bin) when is_binary(Bin) -> binary:encode_hex(Bin).
bin2hexstr_a_f_lower(Bin) when is_binary(Bin) ->
    string:lowercase(binary:encode_hex(Bin)).

rand_seed() -> rand:seed(exsplus).

now_ms() -> erlang:system_time(millisecond).
