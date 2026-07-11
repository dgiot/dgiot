# DGAIOT-面向边缘智能的轻量级人机物地融合智能物联网开源软件

[![GitHub Release](https://img.shields.io/github/release/dgiot/dgiot?color=brightgreen)](https://github.com/dgiot/dgiot/releases)
[![Docker Pulls](https://img.shields.io/docker/pulls/dgiot/dgiot)](https://hub.docker.com/r/dgiot/dgiot)
[![Community](https://img.shields.io/badge/Community-DGIOT-yellow)](https://www.dgiotcloud.cn/)
[![QQ群346566935](https://img.shields.io/badge/QQ群-346566935-brightgreen)](https://jq.qq.com/?_wv=1027&k=LipWZvDe)

[English](./README.md) | [中文](./README-CN.md) | [日本語](./README-JP.md) | [русский](./README-RU.md)|
[github](https://github.com/dgiot)|[gitee](https://gitee.com/dgiiot)|[官网](https://www.dgiotcloud.cn/)|[博客](https://www.dgiotcloud.cn/?cat=19)|[体验](https://prod.dgiotcloud.cn)|[微信群](#jump)

## DGIOT简介
dgaiot是一款基于dgiot的开源软件，致力于打造一个面向边缘智能的轻量级、人机物地融合的智能物联网解决方案。通过整合物联网、人工智能、云计算和大数据技术，dgaiot为各类物联网应用场景提供了高效、安全、稳定的智能服务。

> **FDE工程师的必备系统** — 现场数字工程师(Field Digital Engineer)日常工作台：物模型定义设备、本体语义统一数据标准、SWRL规则驱动自动闭环、时序数据实时监控、拓扑关系可视化。一平台打通从设备接入到智能决策的全链路。


## 系统部署
构建 *dgaiot* 需要 openEuler 24.03, 在Windows下用安装[openEuler 24.03 Installer.exe](https://gitee.com/dgaiot/dgaiot/blob/master/wsl/openEuler%2024.03%20Installer.exe),
wsl镜像管理工具 [easyWSL Installer.exe](https://gitee.com/dgaiot/dgaiot/blob/master/wsl/easyWSL%20Installer.exe)。

然后在wsl环境下执行一键式部署命令
```
wget -qO dgiot_install.sh https://gitee.com/dgaiot/dgaiot/blob/master/dgiot_install.sh  && sh dgiot_install.sh
```
<img src="https://dgiot-wsl-1306147891.cos.ap-guangzhou.myqcloud.com/wsl_devops.png" width = "100%" />

安装完成在本地访问 http://127.0.0.1:3000
<img src="https://dgiot-wsl-1306147891.cos.ap-guangzhou.myqcloud.com/dify_plugin.png" width = "100%" />

<img src="https://dgiot-wsl-1306147891.cos.ap-guangzhou.myqcloud.com/dify_flow.png" width = "100%" />


## FDE工程师六步工作流

| 步骤 | 工作 | 使用的插件 | 产出 |
|------|------|-----------|------|
| 1. 建物模型 | 定义设备属性/服务/事件 | dgiot_device, dgiot_thing | JSON物模型 |
| 2. 定语义 | 定义实体身份/关系/约束 | **dgiot_ontology** | 本体模型+SWRL规则 |
| 3. 接入设备 | 多协议数据采集 | dgiot_bridge, dgiot_dlink | MQTT/Modbus/HTTP |
| 4. 存时序 | 高频遥测数据落盘 | dgiot_tdengine | 超级表+子表 |
| 5. 配规则 | 条件→动作自动闭环 | dgiot_rule_engine, dgiot_ontology_rule | L1/L2/L3分级响应 |
| 6. 看驾驶舱 | 拓扑图+趋势图+告警 | iotStudio Vue3 | 34页交互看板 |

**三行业已验证：制造业(设备健康)·能源(光伏运维)·楼宇(智能能耗)**

## 快速体验与微信群
| 微信技术支持群 | [QQ技术支持群](https://jq.qq.com/?_wv=1027&k=LipWZvDe)   | 小程序体验 | 官网案例 |
|:---:|:---:|:---:|:---:|
|<img src="https://dgiot-web-1306147891.cos.ap-nanjing.myqcloud.com/wechat.png" width = "100%" />|<img src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/QQ%E6%8A%80%E6%9C%AF%E7%BE%A4%E4%BA%8C%E7%BB%B4%E7%A0%81.png" width = "100%" /> |<img src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_release/dgiot_wechat.jpg" width = "100%" /> | [官网体验地址](https://prod.dgiotcloud.cn)  </br> </br> [智慧园区](https://www.dgiotcloud.cn/smartpark/) </br></br> [平安校园](https://www.dgiotcloud.cn/smartcampus/)|

## 核心特色
+ **完全开源**：前后端完全开源，无收费商业版
+ **快速部署**：6分钟私有化[一键式部署](https://doc.dgiotcloud.cn/docs/product_doc/docs/deployment_details/)，快速拥有自己的物联网平台
+ **专业可靠**：千万级设备接入与管理，电信级稳定性
+ **兼容并包**：兼容工业领域多行业的常见协议类型
+ **全流程低代码**：物模型-规则引擎-数据通道-组态页面全流程可视化低代码开发
+ **本体语义引擎**：首创 OWL 本体 + OTP 状态机执行架构，让物联网数据具备业务语义。**定身份**（设备≠订单）、**定关系**（执行≠归属）、**定动作**（故障触发重排），从"连接设备"升级到"理解业务"

## 本体先行 — DGAIOT 的语义核心

> **智能不是连接的产物，是共识的产物。** 传统物联网平台能告诉你"温度 142°C"，DGAIOT 告诉你"烘丝温度超标 → 影响这批烟的质量 → 调低蒸汽阀门 10%"。

### 本体 ≠ 知识图谱

| | 知识图谱 | 本体 |
|---|---------|------|
| 本质 | 数据关系的可视化载体 | 业务语义规则说明书 |
| 能力 | 展示"设备A连工单B" | 解读"设备A故障将导致订单延迟" |
| 关系 | 静态连线 | 消息驱动的因果链 |
| 规则 | 简单的阈值告警 | SWRL级别的条件→动作推理 |

**智能三支柱：** 定身份（设备≠订单）→ 定关系（执行≠归属）→ 定动作（故障触发重排）。三者缺一不可。

### dgiot_ontology 插件

```erlang
%% 1. 用物模型方式定义本体（类似 IoT 平台的 TSL）
Model = #{
  <<"modelId">> => <<"CigaretteMaker-v1">>,
  <<"class">> => <<"equipment">>,
  <<"relations">> => [#{<<"relation">> => <<"monitored_by">>, <<"target">> => <<"quality">>}],
  <<"rules">> => [
    #{<<"id">> => <<"P2">>, <<"severity">> => <<"L1">>,
      <<"when">> => #{<<"property">> => <<"health_score">>, <<"op">> => <<"<">>, <<"value">> => 60},
      <<"then">> => #{<<"state">> => <<"fault">>, <<"action">> => <<"emergency_stop">>}}
  ]
}.
%% 2. 加载并实例化
dgiot_ontology:load_model(Model).
dgiot_ontology:spawn_instance(MId, #{<<"id">> => <<"ZJ116B">>, <<"health_score">> => 98.7}).

%% 3. 规则自动评估
dgiot_ontology_rule:evaluate(Rules, #{<<"health_score">> => 55.0}, #{}) → [P2触发!].
```

| 模块 | 功能 |
|------|------|
| `dgiot_ontology` | 核心 API — 加载模型、实例化实体 |
| `dgiot_ontology_model` | 物模型 → 本体模型解析 |
| `dgiot_ontology_registry` | 实体注册表 — 类层次索引、关系连接 |
| `dgiot_ontology_rule` | SWRL 规则引擎 — 编译、条件匹配、触发评估 |
| `dgiot_ontology_owl` | OWL/RDF 标准本体文件导出 |

### 架构集成

```
┌──────────────────────────────────────┐
│       dgiot_ontology (本体语义层)      │
│   定身份 · 定关系 · 定动作 · OWL导出   │
└──────┬──────┬──────┬──────┬─────────┘
       ▼      ▼      ▼      ▼
  dgiot_device  dgiot_topo  dgiot_task  emqx_rule_engine
  (物模型=属性) (拓扑=关系图) (任务=动作) (规则=触发)
```


## 对标 Palantir AIP (Artificial Intelligence Platform)

| Palantir AIP | DGAIOT | 状态 |
|-------------|--------|------|
| **Ontology** — 业务语义层 | **dgiot_ontology** — 五维四层本体引擎 | ✅ 12模块 |
| **Pipeline Builder** — 可视化工作流 | **dgiot_rule_engine** — 规则构建器 | ✅ |
| **LLM Integration** — 自然语言交互 | **dgiot_openai** — DeepSeek/LLM集成 | 🔄 规划中 |
| **Operational Backbone** — 数据→决策→执行 | **dgiot_bridge + hook** — 数据管道 | ✅ |
| **Object-to-Object** — 跨实体关系追溯 | **dgiot_ontology_registry** — gen_server注册表 | ✅ |
| **Scenario Studio** — 场景建模 | **dgiot_ontology_demo** — 制造业/能源/楼宇 | ✅ |
| **AIP Logic** — 无代码规则 | **dgiot_ontology_rule** — SWRL规则引擎 | ✅ |

> **核心差异**: Palantir是万亿级企业平台，DGAIOT是开源轻量级边缘智能平台。相同的本体驱动理念，不同的部署场景。

## 项目背景

随着物联网技术的飞速发展，越来越多的设备和应用场景需要接入物联网平台。然而，传统物联网平台往往存在部署复杂、资源消耗大、扩展性差等问题。为了解决这些问题，dgiot应运而生，并以其高并发、高安全、低时延和低带宽等技术优势，在物联网领域取得了显著成效。在此基础上，dgaiot进一步拓展和优化，特别针对边缘智能场景进行了深度定制和优化。

## 项目特点

1. **轻量级设计**：dgaiot采用了轻量级架构，能够在资源有限的边缘设备上高效运行，降低了物联网应用的部署和运维成本。

2. **人机物地融合**：dgaiot支持人机物地多元数据的接入、处理和融合，为各类应用场景提供了丰富的数据资源和分析手段。

3. **边缘智能**：通过集成先进的边缘计算技术，dgaiot能够在边缘设备上实现实时的数据处理和智能分析，提高了物联网应用的响应速度和智能化水平。

4. **开源开放**：dgaiot遵循开源开放的原则，所有代码和文档均公开透明，方便开发者进行二次开发和定制。

5. **安全可靠**：dgaiot内置了完善的安全防护机制，能够保障物联网设备和数据的安全性和隐私性。

## 技术架构







































