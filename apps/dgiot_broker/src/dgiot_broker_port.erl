%% @doc dgiot_broker 端口（behaviour）：dgiot 自有 broker 的唯一调用面。
%%
%% 设计要点（见 docs/EMQX-REPLACEMENT-PLAN.md）：
%%   * dgiot 的 150 个 emqx_* 调用点不重贴，靠"同名门面"在切换刀承接；
%%   * 本端口是**自有**调用面，新代码一律走这里，不再直接调 emqx_*；
%%   * 未实现的能力显式返回 {error, {not_implemented, Cut}}，
%%     **绝不静默返回 ok**（EMQX 内部发布静默丢弃正是要根除的缺陷类）。
-module(dgiot_broker_port).

%% 生命周期
-callback start_listener(Name :: atom(), Opts :: map()) ->
    {ok, pid()} | {error, term()}.
-callback stop_listener(Name :: atom()) -> ok | {error, term()}.

%% 数据面
-callback publish(ClientId :: binary(), Topic :: binary(), Payload :: term()) ->
    ok | {error, term()}.
-callback subscribe(ClientId :: binary(), TopicFilter :: binary(), Opts :: map()) ->
    ok | {error, term()}.
-callback unsubscribe(ClientId :: binary(), TopicFilter :: binary()) ->
    ok | {error, term()}.

%% 观测面（生命周期可观测铁律）
-callback routes(Topic :: binary()) -> [pid()] | {error, term()}.
-callback sessions() -> [map()] | {error, term()}.
-callback capabilities() -> map().
-callback backend_info() -> map().
