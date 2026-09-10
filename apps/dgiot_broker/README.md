# hub_apps/dgiot_broker — dgiot 自有 broker（去 EMQX）第一刀

> 立项与八刀计划：`docs/EMQX-REPLACEMENT-PLAN.md`（本地权威）+ Mnemon `08d0c83d`。
> 本目录是**仓库源头**（版本受控），编译前同步进中枢源码树 `/opt/dgiot-4.4/apps/`。

## 现状（刀 1 已完成）

| 项 | 状态 |
|---|---|
| 端口（behaviour） | ✅ `dgiot_broker_port` + 派发层 `dgiot_broker` |
| EMQX 后端适配器 | ✅ `dgiot_broker_backend_emqx`（现网委托，行为与今天一致） |
| 自研后端骨架 | ✅ `dgiot_broker_native`（全部 `{error, {not_implemented, Cut}}`） |
| 同名门面 20 个 | ⏸ `compat/`（**不参与当前 build**，刀 6 启用） |
| 调用点映射表 | ✅ `doc/DEPENDENCY-MAP.md`（自动生成：68 组合 / 125 次调用 / 27 模块） |
| 编译 | ✅ `./rebar3 as emqx compile` |
| 应用是否启动 | ❌ 未启动、未进 release（**刀 1 刻意不改任何现网行为**） |

## 关键约束：同名门面不能与 EMQX 同时在位

Erlang 同名模块在代码路径中二义，因此 `compat/` **不参与当前 build**。
刀 6 切换时三步：

1. `rebar.config.erl` 的 `src_dirs` 加入 `compat`；
2. `relx_plugin_apps_per_rel/1` 剔除 15 个 emqx 应用（并把 `dgiot_broker` 加入）；
3. `broker.backend=dgiot`，异常即回滚（开关保留 EMQX 路径）。

## 铁律（沿用 TD 链与插件化两轮排障）

- **失败必响**：未实现一律 `{error, {not_implemented, Cut}}`，绝不静默返回 `ok`
  （EMQX 内部发布 `publish/4 check_route` 静默丢弃正是要根除的缺陷类）。
- **生命周期可观测**：端口提供 `sessions/0`、`routes/1`、`backend_info/0`。
- **embedded 模式**：中枢是 embedded release，改代码须重新打包；
  单独验证模块用 `rpc code:load_file/1`（code path 变更被忽略）。

## 用法

```bash
# 生成映射表与门面（在中枢树内或有 --src 时）
python3 hub_apps/tools/gen_dependency_map.py --src /opt/dgiot-4.4/apps \
        --dest hub_apps/dgiot_broker

# 同步进中枢树并编译（脚本见 D:\ai\github\scripts\wsl-deploy-broker-app.sh）
cd /opt/dgiot-4.4 && ./rebar3 as emqx compile

# 端口活体自检（不启动应用、不改现网行为）
escript hub_broker_accept1.erl
```

## 端口 API

```erlang
dgiot_broker:backend/0            %% emqx | dgiot
dgiot_broker:capabilities/0       %% 能力矩阵（供测试/运维断言）
dgiot_broker:publish/3            %% 数据面
dgiot_broker:subscribe/3 dgiot_broker:unsubscribe/2
dgiot_broker:start_listener/2 stop_listener/1
dgiot_broker:routes/1 sessions/0  %% 观测面
dgiot_broker:backend_info/0
```

新代码一律走端口，不再直接调 `emqx_*`——这样切换刀一到，dgiot 侧零改动。
