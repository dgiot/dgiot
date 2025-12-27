# Modbus插件命名规范（极简数据中台版）

## 概述

基于DG-IoT作为数据中台的定位，本规范定义了极简的Modbus插件文件命名和架构设计，所有文件都放在src目录下，专注于数据汇聚场景。

## 设计原则

### 1. 极简主义
- **一切在src下**：所有文件都放在src根目录或直接子目录
- **无多余层级**：删除common等中间目录
- **直观查找**：文件位置直观，便于快速定位

### 2. 数据中台定位
- **数据汇聚中心**：作为数据接收方，汇聚外部设备数据
- **被动接收为主**：外部设备主动上报，我们被动接收
- **协议适配**：根据外部设备角色适配通信模式

### 3. 协议角色匹配
```
外部设备角色  →  DG-IoT角色  →  文件命名
-------------------------------------------
Modbus RTU Client → RTU Server → dgiot_modbus_rtu_server_*
Modbus TCP Server → TCP Client → dgiot_modbus_tcp_client_*
```

## 文件命名规范

### 核心文件命名模式
```
dgiot_modbus_{协议}_{角色}_{功能}.erl
```

### 必需文件列表（全部在src下）

#### 1. 通道文件（数据入口）
- `dgiot_modbus_rtu_server_channel.erl` - RTU服务器通道
- `dgiot_modbus_tcp_client_channel.erl` - TCP客户端通道

#### 2. 主模块文件
- `dgiot_modbus_rtu_server.erl` - RTU服务器主模块
- `dgiot_modbus_tcp_client.erl` - TCP客户端主模块

#### 3. 通用模块
- `dgiot_modbus.erl` - 插件主模块
- `dgiot_modbus_app.erl` - 应用模块
- `dgiot_modbus_sup.erl` - 监控树
- `dgiot_modbus_handler.erl` - HTTP处理器

#### 4. 协议解析工具
- `modbus/` - 协议解析目录
  - `modbus.erl` - 通用Modbus模块
  - `modbus_tcp.erl` - TCP协议模块
  - `modbus_util.erl` - 工具函数
  - `modbus_rtu/` - RTU协议解析
    - `modbus_rtu.erl` - RTU主模块
    - `modbus_rtu_decoder.erl` - 解码器
    - `modbus_rtu_encoder.erl` - 编码器

## 目录结构

```
src/
├── dgiot_modbus_rtu_server_channel.erl    # RTU服务器通道
├── dgiot_modbus_tcp_client_channel.erl    # TCP客户端通道
├── dgiot_modbus.erl                       # 插件主模块
├── dgiot_modbus_app.erl                   # 应用模块
├── dgiot_modbus_sup.erl                   # 监控树
├── dgiot_modbus_handler.erl               # HTTP处理器
├── modbus_rtu/server/dgiot_modbus_rtu_server.erl  # RTU服务器
├── modbus_tcp/client/dgiot_modbus_tcp_client.erl  # TCP客户端
└── modbus/                                # 协议解析工具
    ├── modbus.erl
    ├── modbus_tcp.erl
    ├── modbus_util.erl
    └── modbus_rtu/
        ├── modbus_rtu.erl
        ├── modbus_rtu_decoder.erl
        ├── modbus_rtu_encoder.erl
        └── ...
```

## 通道设计说明

### 1. RTU服务器通道 (`dgiot_modbus_rtu_server_channel.erl`)
- **位置**：src根目录
- **功能**：监听串口端口，接收RTU设备数据
- **场景**：外部RTU设备作为Client主动上报数据

### 2. TCP客户端通道 (`dgiot_modbus_tcp_client_channel.erl`)
- **位置**：src根目录
- **功能**：主动连接TCP服务器，拉取数据
- **场景**：外部TCP设备作为Server，我们主动拉取数据

## 实施历史

- **2025-12-26**：极简数据中台架构
  - 删除common目录，所有文件在src下
  - 进一步简化结构，便于维护
  - 明确数据汇聚定位

## 维护指南

1. **新增文件**：直接放在src下或适当子目录
2. **命名规范**：遵循`dgiot_modbus_{协议}_{角色}_{功能}`模式
3. **代码审查**：检查是否符合极简架构
4. **测试验证**：确保数据汇聚功能正常
