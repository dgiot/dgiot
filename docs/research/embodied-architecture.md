# DLAS 2.0 — 具身智能·无人化·智能管控架构

> 基于 dgaiot DLAS v1.0 扩展

---

## 六层架构

```
┌──────────────────────────────────────────────────────────────────┐
│                    EDGE: iotStudio (Python+Vue)                   │
│           DeviceAccess(9) → UnifiedPipeline → StreamEngine(15)   │
└──────────────────────────────────────────────────────────────────┘
                              │ MQTT / HTTP
┌─────────────────────────────▼────────────────────────────────────┐
│  SAFETY     watchdog · redundancy · compliance · human-in-loop   │
│             IEC 61508 SIL2 · OPC UA Safety · 双通道校验          │
├──────────────────────────────────────────────────────────────────┤
│  AI ENGINE  model inference · RL feedback · multi-modal · RAG   │
│             Ollama/Llama · ONNX Runtime · Edge-Cloud split       │
├──────────────────────────────────────────────────────────────────┤
│  SECURITY   auth · role · ACL/CLP · X.509 · audit trail         │
├──────────────────────────────────────────────────────────────────┤
│  ACTION     Shadow(gen_statem) · Bridge · MQTT · Rule Engine    │
│             + command validator · execution feedback loop         │
├──────────────────────────────────────────────────────────────────┤
│  LOGIC      Ontology Engine · Model Registry · Reasoner          │
│             + policy optimizer · A/B testing · digital twin      │
├──────────────────────────────────────────────────────────────────┤
│  DATA       Parse/PG · TDengine · Mnesia/ETS · EMQX              │
│             + vector DB (RAG) · model registry · feature store   │
└──────────────────────────────────────────────────────────────────┘
```

## 各层职责

### 1. DATA — 新增 AI 数据基础设施

```
原有:  Parse/PG · TDengine · EMQX · ETS
新增:
  Vector DB     → 知识库·历史案例·故障模式检索 (RAG)
  Feature Store → 模型特征缓存 (实时+离线)
  Model Registry → AI模型版本管理 (ONNX/TorchScript)
  Event Bus     → 跨层事件流 (状态变更·告警·动作)
```

### 2. LOGIC — 从规则引擎升级为智能决策引擎

```
原有:  dgiot_ontology:load_model → spawn_instance → evaluate(rules)
新增:
  Policy Optimizer   → 多目标优化求解 (安全·效率·能耗)
  A/B Testing Engine → 影子模式对比 (Shadow vs Optimized)
  Digital Twin Sync  → 物理模型→数字模型双向同步
  Causal Graph       → 因果推断 (根因分析·预测)
```

```erlang
%% 增强后的 evaluate:
evaluate(Shadow, Props) ->
    Rules  = dgiot_ontology:eval_rules(Shadow, Props),    %% 规则层
    Model  = dgiot_ai:inference(Shadow, Props),            %% AI推理层
    Policy = dgiot_policy:optimize(Shadow, Rules, Model), %% 策略优化
    Safety = dgiot_safety:validate(Shadow, Policy),        %% 安全校验
    Action = build_action(Safety),
    
    %% 记录→更新→通知
    dgiot_feedback:log(Shadow, Action),
    dgiot_shadow:transition(Shadow, Action),
    dgiot_bridge:publish(Shadow, Action).
```

### 3. ACTION — 加入安全校验闭环

```
原有:  Shadow → Rule → State Transition → Bridge → MQTT
新增:  Shadow → Rule → AI Inference → Policy → Safety Check → Command → MQTT
                                               ↑                    │
                                               └── execution feedback ─┘
```

**指令校验链：**

```
AI决策 → Policy → Safety Check:
  ├── Range Check:   值在物理范围内?
  ├── Rate Check:    变化率合理?
  ├── Conflict Check: 与其它指令冲突?
  ├── Authority:      操作权限?
  └── Echo Check:     执行后状态确认
         ↓
  全部通过 → gen_statem:cast(shadow, {command, Cmd})
  任一失败 → 拒绝 + 记录 + 告警
```

### 4. AI ENGINE — 具身智能核心

```
┌────────────────────────────────────────────────┐
│              AI Engine Layer                    │
│                                                 │
│  ┌──────────────┐  ┌──────────────┐            │
│  │ Edge Models   │  │ Cloud Models  │            │
│  │ Ollama/Llama  │  │ GPT-4/Claude  │            │
│  │ ONNX Runtime  │  │ Fine-tuned    │            │
│  │ < 100ms       │  │ < 2s (RTT)    │            │
│  └──────┬───────┘  └──────┬───────┘            │
│         │                 │                     │
│  ┌──────▼─────────────────▼───────┐            │
│  │      Model Router              │            │
│  │  latency < 50ms → Edge         │            │
│  │  complex reasoning → Cloud     │            │
│  │  safety critical → Edge+Cloud  │            │
│  └────────────────────────────────┘            │
│                                                 │
│  Capabilities:                                  │
│  ├── Anomaly Detection   (实时异常检测)          │
│  ├── Predictive Control  (预测控制·MPC)          │
│  ├── Visual Inspection   (视觉巡检·缺陷检测)      │
│  ├── NLP Interface       (自然语言指令)           │
│  └── RL Policy           (强化学习策略)           │
└────────────────────────────────────────────────┘
```

### 5. SAFETY — 无人化安全层

```
┌────────────────────────────────────────────────┐
│              Safety Layer                       │
│                                                 │
│  ┌───────────┐  ┌──────────┐  ┌──────────────┐ │
│  │ Watchdog  │  │ Redundancy│  │Human-in-Loop │ │
│  │ timer     │  │ dual-path│  │ escalation   │ │
│  │ heartbeat │  │ voting   │  │ override     │ │
│  │ deadman   │  │ compare  │  │ confirmation │ │
│  └─────┬─────┘  └────┬─────┘  └──────┬───────┘ │
│        │             │               │          │
│  ┌─────▼─────────────▼───────────────▼───────┐  │
│  │            Safety Validator               │  │
│  │  IEC 61508 SIL2 · OPC UA Safety           │  │
│  │  ISO 13849 PLd · dual-channel check       │  │
│  └───────────────────────────────────────────┘  │
│                                                 │
│  Escalation Matrix:                             │
│  L1 (info)   → auto log                         │
│  L2 (warning) → notify + shadow                 │
│  L3 (alarm)  → auto stop + human confirm        │
│  L4 (critical)→ immediate shutdown + lockout    │
└────────────────────────────────────────────────┘
```

## 端到端流程：具身智能巡检

```
1. 任务下发 (NLP/AI):
   "巡检油井 rtu_001 的振动异常"

2. 感知 (Edge Vision):
   Camera → ONNX Runtime → "振动频率 15Hz, 振幅 3.2mm"
   → MQTT: $dg/thing/2de1b3e1b8/rtu_001/properties/report

3. 推理 (AI Engine):
   dgiot_ai:inference(Shadow, Props)
   → 对比历史数据 (TDengine vector search)
   → RAG 检索: "轴承故障模式 #3"
   → 结论: 轴承磨损概率 87%, 建议更换

4. 策略优化 (Logic):
   dgiot_policy:optimize(Shadow, Rules, Model)
   → 约束: 生产不能停
   → 方案: 降速至 60% → 维持生产 → 安排检修

5. 安全校验 (Safety):
   dgiot_safety:validate(Shadow, Policy)
   → range: 60% > 50% minimum → OK
   → rate: 100%→60% within limits → OK
   → authority: operator level → OK
   → echo: pending execution feedback

6. 执行 (Action):
   Shadow → {command, set_speed, 60} → MQTT
   → iotStudio → Modbus write register
   → Echo: speed=60 confirmed → feedback loop closed

7. 记录 (Data):
   TDengine: INSERT vibration 15Hz, action: speed_reduce
   Vector DB: similar-case: "2024-Q2 well-K1 bearing replacement"
   Parse: update maintenance schedule
```

## 与传统方案对比

| 维度 | DLAS 2.0 | 传统 SCADA | 云 AI 方案 |
|------|---------|-----------|-----------|
| 推理延迟 | Edge <100ms | N/A | Cloud >2s |
| 安全校验 | 五重门 | 限值报警 | 无 |
| 反馈闭环 | MQTT echo | 无 | 异步 |
| 模型管理 | Model Registry | 无 | 云端 |
| 合规 | IEC 61508 路径 | IEC 61131 | 不适用 |
| 数字孪生 | gen_statem 1:1 | 无 | 云端模型 |
| 人机回环 | L1-L4 升级矩阵 | 人工检查 | 云端确认 |

## 实施路径

```
Phase 1 (当前):     DLAS v1.0 管道验证 ✅
Phase 2 (下季度):    + Safety Layer (watchdog + echo check)
Phase 3 (半年):      + AI Engine (Ollama/ONNX edge inference)
Phase 4 (一年):      + Policy Optimizer + Digital Twin Sync
Phase 5 (远期):      + IEC 61508 认证 + 多机器人协同
```

## 技术栈新增

```
当前 (v1.0)                    v2.0 新增
─────────────                  ─────────
ETS + Parse + PG               + Milvus/Chroma (Vector DB)
EMQX MQTT                      + Kafka/Pulsar (Event Bus)
gen_statem                     + ONNX Runtime (Edge AI)
dgiot_tdengine                 + Feast (Feature Store)
Rule Engine                    + Optuna (Policy Optimizer)
                               + IEC 61508 Toolchain (Safety)
                               + MLflow (Model Registry)
```
