%% @doc 同名承接：emqx_vm（刀 6 批次 2）。VM 负载与内存。
-module(emqx_vm).

-export([get_otp_version/0, loads/0, mem_info/0, cpu_info/0, used_memory/0,
         process_count/0]).

get_otp_version() ->
    list_to_integer(erlang:system_info(otp_release)).

loads() ->
    case erlang:statistics(run_queue) of
        RQ when is_integer(RQ) -> {RQ, RQ, RQ};
        _ -> {0, 0, 0}
    end.

mem_info() ->
    case erlang:memory() of
        L when is_list(L) -> maps:from_list(L);
        _ -> #{}
    end.

used_memory() ->
    proplists:get_value(total, erlang:memory(), 0).

cpu_info() ->
    #{logical_processors => erlang:system_info(logical_processors),
      schedulers => erlang:system_info(schedulers)}.

process_count() ->
    erlang:system_info(process_count).
