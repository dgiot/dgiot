%% @doc 同名承接：emqx_types（刀 6 第一批）。
%%
%% 关键认识：分析器把 `emqx_types:topic()` 这类**类型引用**也算成"调用"（arity 0）。
%% 所以这里除了函数，还要 `-export_type` 把插件/业务代码引用的类型名补齐——
%% 否则它们的 `-spec` 编译不过。
-module(emqx_types).

%% 占位函数：某些老代码会调用它们取"默认值"
-export([topic/0, message/0, clientid/0, subopts/0, subid/0,
         startlink_ret/0, sockstate/0, qos/0, payload/0, flags/0, headers/0]).

%% 类型导出：与 EMQX 4.4 同名
-export_type([topic/0, message/0, clientid/0, subopts/0, subid/0,
              startlink_ret/0, sockstate/0, qos/0, payload/0, flags/0,
              headers/0, packet_id/0, peers/0]).

-type topic() :: binary().
-type message() :: tuple().
-type clientid() :: binary().
-type subopts() :: map().
-type subid() :: binary().
-type startlink_ret() :: {ok, pid()} | ignore | {error, term()}.
-type sockstate() :: atom().
-type qos() :: 0 | 1 | 2.
-type payload() :: binary().
-type flags() :: map().
-type headers() :: map().
-type packet_id() :: non_neg_integer().
-type peers() :: list().

%% 默认值访问器（EMQX 里存在同名零元函数）
topic() -> <<>>.
message() -> undefined.
clientid() -> <<>>.
subopts() -> #{}.
subid() -> <<>>.
startlink_ret() -> ignore.
sockstate() -> closed.
qos() -> 0.
payload() -> <<>>.
flags() -> #{}.
headers() -> #{}.
