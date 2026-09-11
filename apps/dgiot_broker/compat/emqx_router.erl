%% @doc 同名承接：emqx_router（影子内核，刀 4）。
%% 转发到我们的路由簿。注意语义差异并**显式返回**而不是照抄 EMQX 的静默：
%% EMQX 的 has_routes/1 对只有通配订阅者的主题返回 false（我们实测过的
%% 静默丢弃源头之一），这里按"能否被匹配"回答，且提供 match_routes/1 供
%% 调用方拿到真实订阅者。
-module(emqx_router).

-include("emqx.hrl").

-export([match_routes/1, has_routes/1, do_match/1, lookup_routes/1]).

%% 真实订阅者（我们的路由簿）
match_routes(Topic) when is_binary(Topic) ->
    [Pid || {_Cid, Pid, _Q} <- dgiot_broker_router:match(Topic)];
match_routes(_) ->
    [].

%% 有订阅者能收到即为 true（通配订阅也算，修正 EMQX 的 false 语义）
has_routes(Topic) when is_binary(Topic) ->
    case dgiot_broker_router:match(Topic) of
        [] -> false;
        _ -> true
    end;
has_routes(_) -> false.

do_match(Topic) -> match_routes(Topic).

%% @doc 管理端路由查询：返回命中 Topic 的路由（#route{}，单节点 dest=node()）。
%% EMQX 这里查 exact 主题表；我们按"会收到 Topic 的订阅过滤器"给出，语义
%% 更贴近可观测意图，形态对齐 #route{topic, dest} 供管理 API 序列化。
lookup_routes(Topic) when is_binary(Topic) ->
    [#route{topic = Filter, dest = node()}
     || {Filter, _ClientId, _V} <- dgiot_broker_router:subscriptions(),
        dgiot_broker_router:topic_matches(Filter, Topic)];
lookup_routes(_) ->
    [].
