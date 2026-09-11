%% @doc 同名承接：emqx_alarm_handler（告警日志处理器）→ 卸载即 ok。
%% EMQX 的 alarm_handler 是 SASL 告警处理器；我们无 SASL 告警源，
%% unload 幂等返回 ok（显式声明）。
-module(emqx_alarm_handler).

-export([unload/0]).

unload() -> ok.
