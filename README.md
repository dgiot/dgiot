# dgaiot — 工业物联网大汇聚引擎

Erlang/OTP 高性能物联网平台。海量设备汇聚，影子设备 + 本体论 + 状态机。

## 核心

| 组件 | 说明 |
|------|------|
| 影子设备 | 物理设备 1:1 gen_statem 进程 |
| 本体论 | Site > Gateway > Device > Point 4层 |
| 状态机 | init -> auth -> online -> {normal, alarm, offline} |

## 架构

iotStudio(边缘) --MQTT--> dgaiot(汇聚) <--HTTP-- iotStudio(应用)

## apps

```
dgiot/           核心引擎 (EMQX + 规则 + 告警)
dgiot_ontology/  4层本体论
dgiot_parse/     Parse Server 客户端
dgiot_task/      影子设备 / 任务调度
dgiot_device/    设备管理 / 物模型
dgiot_bridge/    桥接框架
dgiot_dlink/     数据链路
dgiot_api/       管理 API
dgiot_http/      HTTP 服务
dgiot_tdengine/  TDengine 时序
```

## 构建

```
export PATH=/usr/local/erlang_24.3/bin:$PATH
make
```

## 许可证

Apache 2.0
