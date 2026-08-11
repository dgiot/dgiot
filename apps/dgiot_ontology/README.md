# dgiot_ontology — 三层实时本体引擎

> **本体 = 知识图谱(骨架) + SWRL推理(大脑) + 实时执行(神经)**
> 
> 本体不是地图，是神经。不是存储，是过程。不是名词，是动词。

## 本体是什么

```
本体 ⊃ 知识图谱。知识图谱是本体的第一层。

┌──────────────────────────────────────────────┐
│ 第一层: 知识图谱 (Gruber 1993)                │
│   骨架 — 245+类·58属性·43约束               │
│   OWL RDF/XML · Neo4j                       │
│   "概念化的形式规范"——静态地图                │
├──────────────────────────────────────────────┤
│ 第二层: SWRL推理 (Guarino 1995)              │
│   大脑 — 20条规则·L0-L3分层推理·50条安全判据   │
│   gen_statem 模式匹配 · 事件驱动推理          │
│   "可能世界的逻辑理论"——会思考但不动手         │
├──────────────────────────────────────────────┤
│ 第三层: 实时执行 (本方案 2026)                │
│   神经 — 传感器驱动·gen_statem·<2s闭环       │
│   Actor进程隔离·热升级·99.999%可用           │
│   "物理世界的实时数字镜像"——知行合一           │
└──────────────────────────────────────────────┘
```

**与知识图谱的本质区别**: 知识图谱 = 静态的、人来编辑的、描述"曾经是什么"。本体 = 动态的、传感器驱动的、反映"此刻是什么"——并基于此刻状态触发实时执行。

### GET /api/ontology/definition

返回上述三层定义的完整 JSON。

## 架构



## 模块

| 模块 | 功能 |
|------|------|
| dgiot_ontology | 核心API: start/load_model/spawn_instance |
| dgiot_ontology_model | 物模型JSON→本体解析 |
| dgiot_ontology_registry | gen_server实体注册表 |
| dgiot_ontology_rule | SWRL规则编译/评估/匹配 |
| dgiot_rule_engine | 通用规则引擎(零EMQX依赖) |
| dgiot_ontology_bridge | 设备数据→语义检查→规则触发 |
| dgiot_ontology_owl | OWL/RDF标准导出 |
| dgiot_ontology_demo | 制造业/能源/楼宇三场景演示 |
| dgiot_ontology_pg | PostgreSQL持久化 |

## 快速开始

Eshell V12.3  (abort with ^G)
[ONTOLOGY] Started (ETS + Registry)
1> *** Terminating erlang (nonode@nohost)

=== Manufacturing - Machine Health ===
Eshell V12.3  (abort with ^G)
1> *** Terminating erlang (nonode@nohost)

=== Energy - Solar O&M ===
Eshell V12.3  (abort with ^G)
1> *** Terminating erlang (nonode@nohost)

=== Building - Smart Energy ===
Eshell V12.3  (abort with ^G)
1> *** Terminating erlang (nonode@nohost)

## 数据库

- PostgreSQL: ontology_models/instances/relations/audit_log (4表)
- TDengine: 设备遥测时序数据

## 相关文档

- 项目README: /root/gitee/dgaiot/README-CN.md
- 本体方法论: ~/.claude/skills/ontology-builder/SKILL.md
