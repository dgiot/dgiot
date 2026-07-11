# FDE Ontology — 多源本体自动提取与审核

多源输入→本体检出→AI审核→场景升级→代码生成 自动化管线

## 触发条件

- "提取本体" / "生成本体" / "梳理本体"
- "自动创建本体" / "审核本体"
- "从 Excel/PPT/DOCX 提取设备"
- "协议解析生成本体"
- 编辑 thing_model.json / io_ontology.json 后自动审核
- 对接 fde-toolkit / fde-iot / fde-deploy 输入

## 输入源→提取器

| 输入 | 格式 | 提取内容 | 输出 |
|------|------|---------|------|
| 寄存器表 | .xlsx | Modbus 地址·类型·单位·阈值 | thing_model.properties[] |
| 设备清单 | .docx/.ppt | 设备名·型号·数量·协议 | Device[] |
| 网络抓包 | .pcap | A11 5a5a 帧·Modbus TCP·OPC DCOM | protocol.json |
| 配置文件 | .ini/.conf | 通道配置·DTU参数·数据库连接 | Channel[] |
| 架构图 | .ppt/.drawio | 网络拓扑·服务器·数据流 | Site/Gateway[] |
| 进程清单 | .csv/.txt | 进程名·版本·内存·心跳 | Process[] |
| 运行日志 | .log/.zio | 设备地址·测点名·实时值 | Thing[] |
| 投标文件 | .docx | 设备材料清单·技术参数 | Product[] |

## 工作流

```
Phase 1: EXTRACT — 多源提取
  └── scripts/extract_*.py (per source type)

Phase 2: MERGE — 本体合并
  └── scripts/merge_ontology.py
      去重·冲突检测·补全缺失字段

Phase 3: AUDIT — 7项审核
  └── scripts/audit_ontology.py
      1.寄存器地址冲突  2.数据类型不匹配  3.告警阈值不合理
      4.devaddr重复     5.协议端口冲突    6.TDengine表名长度
      7.ACL规则覆盖检查

Phase 4: UPGRADE — 场景智能升级
  └── scripts/scene_upgrade.py
      读TDengine历史 → 优化阈值·关联规则·预测模型建议

Phase 5: DEPLOY — 入库+生成
  └── 本体 → dgaiot Parse (Site/Gateway/Device/Point)
      ↓
      AI code gen → gen_statem + MQTT + TDengine schema
```

## RULE

1. **本体是唯一真相源** — 所有输入最终合并为单一本体文件
2. **审核不可跳过** — 7项检查通过才可进入下一步
3. **人工确认在审核后、部署前** — AI 建议，人决策
4. **生产环境运行确定性代码** — 生成的 .erl 编译后直接部署，无 AI 参与
5. **修正本体不修正代码** — 发现错误改本体，重新生成

## 输出物

```
output/
  thing_model.json         合并后的物模型 (带审核标记)
  devices.json             设备注册表
  ontology_report.md       审核报告 (问题清单+处理建议)
  upgrade_plan.md          智能升级方案
  generated/               AI 生成的确定性代码
    dgiot_shadow_guard.erl  编译后的 gen_statem guard
    bridge_config.yaml      MQTT桥接配置
    tdengine_schema.sql     TDengine建表语句
```

## 协作技能

```
fde-ontology ← fde-toolkit (PPT→DLAS)
             ← fde-iot      (pcap→protocol)
             ← fde-deploy   (config→channel)
             ← docx-gen     (投标书→设备清单)
             ↓
             → dgaiot       (本体→代码→部署)
```
