%% @doc 同名承接：emqx_sys（刀 6 批次 2）。系统信息来自本节点。
-module(emqx_sys).

-export([info/0, version/0, sysdescr/0, cluster_name/0, uptime/0, datetime/0]).

info() ->
    #{version => version(),
      sysdescr => sysdescr(),
      cluster_name => cluster_name(),
      uptime => uptime(),
      otp_release => erlang:system_info(otp_release),
      node => node()}.

version() ->
    case application:get_key(dgiot_broker, vsn) of
        {ok, V} -> V;
        _ -> <<"dgiot_broker">>
    end.

sysdescr() ->
    iolist_to_binary(
      io_lib:format("dgiot broker core (EMQX-compatible) on OTP ~s",
                    [erlang:system_info(otp_release)])).

cluster_name() ->
    case application:get_env(dgiot_broker, cluster_name) of
        {ok, N} -> N;
        _ -> <<"dgiot">>
    end.

uptime() ->
    {Up, _} = erlang:statistics(wall_clock),
    Up div 1000.

datetime() ->
    calendar:universal_time().
