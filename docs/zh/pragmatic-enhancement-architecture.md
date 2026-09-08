# DGAIoT 务实增强架构方案（成熟组件集成，不依赖克隆项目）

> 姊妹文档：[comparison.md](../comparison.md)（DGIOT vs. Palantir 定位：大脑与四肢）、
> [open-source-palantir-landscape.md](../open-source-palantir-landscape.md)（已核实的开源生态调查）。
>
> 原则：用生产级成熟开源组件构建数据融合与智能分析能力——**不依赖任何未经验证的"开源 Palantir"克隆**。
> 生态调查显示该赛道真正有社区规模的项目只有 WorldMonitor 与 Semantica，且都不面向工业边缘层——我们不需要等它们成熟。

## 核心原则

1. **DGAIoT 保持不变** —— 设备连接、协议转换、数据采集能力是基石，不做重构。
2. **分层解耦** —— 采集 / 存储 / 分析 / 可视化分层，每层选型成熟组件。
3. **渐进式增强** —— 先跑通基础链路，再逐步引入规则与 AI。

## 推荐组件组合

| 层级 | 组件 | 用途 | 说明 |
| :--- | :--- | :--- | :--- |
| **数据总线** | **Apache Kafka** | 异步解耦，削峰填谷 | DGAIoT 将设备数据推入 Kafka，下游消费者自由扩展。 |
| **时序数据库** | **TimescaleDB**（PostgreSQL 扩展） | 存储设备时序数据 | 完整 SQL，可与关系型元数据自然 JOIN，适合分析型查询。 |
| **关系型元数据** | **PostgreSQL** | 对象定义、设备档案、工单、人员等"本体"数据 | 外键即可实现轻量"本体论"，无需额外图数据库。 |
| **规则引擎** | **Node-RED** 或 **Drools** | 实时阈值 → 告警 → 动作 | 覆盖约 90% 的工业阈值场景，低延迟、高可靠。Node-RED 需经其 Kafka 扩展节点订阅 Kafka。 |
| **任务调度** | **Apache Airflow** | 编排 ETL 与定期报表 | 离线数据聚合与模型训练触发。 |
| **可视化** | **Grafana** + **Apache Superset** | 仪表盘 + 自助 BI | Grafana 强于实时监控，Superset 强于拖拽分析，二者可并存。 |
| **可选 AI/ML** | **MLflow** + PyTorch / XGBoost | 预测模型 | 规则覆盖不了时（故障预测、质量优化）；模型输出可回调规则引擎。 |

## 与现有 DGIOT 栈的衔接

DGIOT 已内置本方案将引入的部分组件——须有意识地复用，避免双轨制：

- **MQTT/EMQX 桥接**：阶段 1 的"转发设备数据至 Kafka"= 在现有 `dgiot_bridge` 上新增一个 sink，无新增协议工作。
- **时序库**：DGIOT 自带 **TDengine**。需明确取舍：高吞吐遥测继续用 TDengine，仅在与元数据关系 JOIN 需求强的分析场景引入 TimescaleDB，或二者择一标准化——**不要盲目双跑**。
- **元数据 / "本体"**：DGIOT 的 Parse Server（23 Classes）+ PostgreSQL 已承载设备档案数据。阶段 2 的业务表应**扩展现有 Classes**，不要另起平行 schema。

## 实施路线图（4 个阶段，每阶段 2~4 周）

### 阶段 1：数据管道搭建（第 1~3 周）

- 部署 Kafka（起步单机 KRaft 模式即可）。
- 扩展 DGIOT 的 MQTT 桥接，将设备数据同时转发至 Kafka。
- 启动 TimescaleDB，创建 `device_telemetry` 超表（时间戳、设备 ID、指标键值）。
- 编写简单的 Kafka Consumer（Python/Java）将数据写入 TimescaleDB。

### 阶段 2：轻量"本体"建模（第 4~6 周）

- 在 PostgreSQL 中创建业务表：`devices`、`product_lines`、`maintenance_orders`、`personnel`（已有 Parse Class 覆盖的实体优先扩展）。
- 建立外键关联（例如 `maintenance_orders.device_id → devices.id`）。
- 利用 TimescaleDB 超表与 `devices` 表 JOIN，形成统一的设备视图。

### 阶段 3：实时规则与告警（第 7~8 周）

- 部署 Node-RED（含 Kafka 扩展节点）或 Drools 服务。
- 订阅遥测主题，按阈值规则（如温度 > 85℃ 持续 5 分钟）触发告警。
- 告警自动创建工单（写回 PostgreSQL），或发送企业微信/邮件通知。

### 阶段 4：可视化与自助分析（第 9~12 周）

- Grafana 连接 TimescaleDB：实时监控面板（各设备指标曲线、告警状态）。
- Superset 连接 PostgreSQL（元数据）+ TimescaleDB（时序）：业务人员拖拽式自助分析。

### 可选增强：AI 预测（按需单独立项）

- 汇聚历史时序数据，训练轻量级 LSTM 或 XGBoost 模型（MLflow 管理）。
- 模型部署为 REST API，规则引擎定时调用，预测结果写入数据库并触发预警。

## 与 Palantir 的对比（诚实版）

| 能力 | Palantir 做法 | 本方案做法 | 优劣 |
| :--- | :--- | :--- | :--- |
| 数据集成 | 自研连接器 + 本体映射 | DGAIoT + Kafka + 自写 Consumer | 更轻量，但需自行维护 |
| 本体论 | 专有语义层，动态对象关系 | PG 外键 + 视图 | 功能有限但稳定，适合中小规模 |
| 分析决策 | AIP 大模型 + 模拟推演 | 规则引擎 + 可选 ML | 无法处理极端复杂问题，但满足 90% 工业场景 |
| 应用开发 | Workshop 低代码 | Grafana / Superset | 灵活性稍差，学习成本低得多 |

## 总结

- 全部采用**生产级开源组件**，无任何"玩具"依赖（为何避开克隆项目，见生态调查文档）。
- 成本极低：最小组合（Kafka + PostgreSQL + Grafana）单台 4C16G 服务器即可运行。
- 扩展性强：后续可引入 Spark/Flink 做实时流计算。
- **不需要等待任何"开源 Palantir"成熟——现在即可落地。**
