%% @doc 同名承接：emqx_acl_cache（ACL 结果缓存）。
%% 本 broker 的 ACL 在连接进程内即时判定，未物化跨进程缓存，故 drain 为
%% 语义兼容的 no-op（管理端 reload 时调用，结果被丢弃，仅要求不 crash）。
-module(emqx_acl_cache).

-export([drain_cache/0]).

drain_cache() -> ok.
