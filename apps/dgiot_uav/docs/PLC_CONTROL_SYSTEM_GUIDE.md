# DGIOT UAV PLC 控制系统使用指南

## 概述

这套系统是为无人机自动化测试设计的PLC控制系统，支持6个工位的Modbus TCP通信，每个工位都有特定的测试功能。

## 工位配置

### 工位映射
- **1100**: 桁行架工位 - 物料搬运
- **1200**: 拷机测试1工位 - 拷机测试
- **1300**: 拷机测试2工位 - 拷机测试
- **1500**: 总测1工位 - 总装测试
- **1600**: 总测2工位 - 总装测试
- **1700**: 磁航向测试工位 - 磁航向校准

## 核心功能

### 1. 指令下发

#### 发送单个指令
```erlang
% 向磁航向工位发送顺时针360度旋转指令
dgiot_uav_plc_integrated:send_command(1700, 1).

% 向桁行架工位发送取料指令
dgiot_uav_plc_integrated:send_command(1100, 1).
```

#### 获取指令描述
```erlang
% 获取指令含义
{ok, Desc} = dgiot_uav_plc_integrated:get_instruction_description(1700, 51, 1).
```

### 2. 连续指令测试

#### 启动连续指令测试
```erlang
% 启动磁航向工位连续指令测试，从第1个指令开始
dgiot_uav_plc_integrated:start_continuous_instruction(1700, 1).
```

#### 获取测试状态
```erlang
% 获取当前测试状态
{ok, Status} = dgiot_uav_plc_integrated:get_continuous_instruction_status(1700).
```

#### 停止连续指令测试
```erlang
% 停止测试
dgiot_uav_plc_integrated:stop_continuous_instruction(1700).
```

### 3. 7步校验流程

系统实现了严格的7步校验流程：

1. **步骤1**: 读取D0，等待PLC就绪（D0=1）
2. **步骤2**: 写D51，写入指令码
3. **步骤3**: 读D11，确认指令接收（D11=指令码）
4. **步骤4**: 写D0=0，清除就绪标志
5. **步骤5**: 写D11=0，清除指令接收标志
6. **步骤6**: 写D60，写入指令码（最后一次握手）
7. **步骤7**: 写D61=1，表示测试完成（只在所有指令完成后执行一次）

## 测试功能

### 运行完整测试
```erlang
% 运行所有工位的完整测试序列
dgiot_uav_plc_test_runner:run_complete_test().
```

### 测试单个工位
```erlang
% 测试指定工位
dgiot_uav_plc_test_runner:test_single_station(1700).
```

### 测试所有工位连通性
```erlang
% 测试所有工位的基本连通性
dgiot_uav_plc_test_runner:test_all_stations().
```

### 测试7步校验序列
```erlang
% 测试7步校验序列
dgiot_uav_plc_test_runner:test_verification_sequence().
```

## 数据存储

### 存储初始化
```erlang
% 初始化PLC数据存储
dgiot_uav_plc_storage:init_ets().
dgiot_uav_plc_storage:init_register_block_ets().
```

### 数据查询
```erlang
% 获取工位状态
{ok, Status} = dgiot_uav_plc_storage:get_station_status(1700).

% 获取所有工位状态
AllStatus = dgiot_uav_plc_storage:get_all_station_status().
```

## 指令码参考

### 桁行架工位 (1100)
- 1: 桁行架去拷机架取料
- 2: 向右上旋转30度
- 3: 向右下旋转30度
- 4: 左上旋转30度
- 5: 左下旋转30度
- 6: 走到水平位
- 7: 测试完成

### 拷机工位 (1200, 1300)
- 1: 成品下料

### 总测工位 (1500, 1600)
- 1: 保持水平
- 2: 右滚90°指令
- 3: 抬头90°指令
- 4: 操作机械臂按预设步长，从H0依次升至H1、H2…H5
- 5: 操作机械臂按预设步长，从H5依次升至H6、H7…H9
- 6: 绕X轴方向转动90°指令
- 7: 操作机械臂运动速度为2°/s，共10秒运动20°，抬头途中读取左前翼舵面传感器
- 8: 操作机械臂抬头，抬头途中读取左前翼舵面传感器
- 9: 操作机械臂低头，抬头途中读取左前翼舵面传感器
- 10: 操作机械臂左滚，抬头途中读取左前翼舵面传感器
- 11: 操作机械臂右滚，抬头途中读取左前翼舵面传感器
- 12: 操作机械臂左偏航，抬头途中读取左垂尾舵面传感器
- 13: 操作机械臂右偏航，抬头途中读取左垂尾舵面传感器
- 14: 折翼指令
- 15: 噪音检测
- 16: 转速检测

### 磁航向测试工位 (1700)
- 1: 顺时针360度
- 2: 复位指令，磁航向测试辅具带动无人机逆时针旋转360度
- 3: 机翼方向翻转90度
- 4: 辅具向机翼方向反向翻转90度
- 5: 成品下料

## 故障排除

### 连接问题
1. 确认Python Modbus TCP服务器正在运行
2. 检查IP地址和端口配置
3. 验证网络连通性

### 指令执行失败
1. 检查PLC是否处于就绪状态（D0=1）
2. 验证指令码是否正确
3. 查看日志获取详细错误信息

### 7步校验失败
1. 检查每步的响应是否符合预期
2. 验证寄存器地址和值
3. 调整重试次数和超时设置

## API调用示例

```bash
# 通过EMQX命令行执行测试
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_plc_test_runner:run_complete_test().'

# 测试单个指令
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_plc_integrated:send_command(1700, 1).'

# 查看所有工位状态
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_plc_storage:get_all_station_status().'
```

## 架构优势

1. **统一入口**: 通过工位地址统一管理所有指令下发
2. **动态绑定**: 治具和无人机指令集可以动态绑定到工位地址
3. **灵活扩展**: 新增工位只需绑定新的地址，无需修改代码
4. **易于维护**: 指令转发逻辑集中管理，便于调试和维护
5. **严格校验**: 实现7步校验确保指令执行的可靠性