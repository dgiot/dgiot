# DGIOT UDP 测试套件

## 概述

本测试套件提供了完整的UDP通信测试功能，包括单播、广播和多播测试。所有测试都经过精心设计，确保在各种网络环境下都能可靠运行。

## 测试模块结构

### 核心测试模块

- **`dgiot_udp_test_runner.erl`** - 统一测试运行器
- **`dgiot_udp_test_unicast.erl`** - 单播基础测试
- **`dgiot_udp_test_broadcast.erl`** - 广播基础测试
- **`dgiot_udp_test_multicast.erl`** - 多播基础测试

### 综合测试模块

- **`unicast_comprehensive_test.erl`** - 单播综合测试
- **`broadcast_comprehensive_test.erl`** - 广播综合测试
- **`dgiot_udp_test_utils_multicast.erl`** - 多播综合测试

### 辅助工具模块

- **`dgiot_udp_test_utils.erl`** - 测试工具函数
- **`direct_broadcast_verification.erl`** - 直接广播验证

## 快速开始

### 方法1：使用测试脚本（推荐）

```bash
# 运行所有UDP测试
./apps/dgiot/src/transport/udp_test/run_all_udp_tests.escript

# 仅运行单播测试
./apps/dgiot/src/transport/udp_test/run_all_udp_tests.escript unicast

# 仅运行广播测试
./apps/dgiot/src/transport/udp_test/run_all_udp_tests.escript broadcast

# 仅运行多播测试
./apps/dgiot/src/transport/udp_test/run_all_udp_tests.escript multicast

# 显示帮助信息
./apps/dgiot/src/transport/udp_test/run_all_udp_tests.escript help
```

### 方法2：在Erlang Shell中运行

```erlang
% 启动Erlang Shell
erl -pa ./_build/emqx/lib/*/ebin ./apps/*/ebin

% 运行所有测试
dgiot_udp_test_runner:run_all_tests().

% 运行特定测试套件
dgiot_udp_test_runner:run_unicast_tests().
dgiot_udp_test_runner:run_broadcast_tests().
dgiot_udp_test_runner:run_multicast_tests().

% 运行单个测试模块
unicast_comprehensive_test:run_all_tests().
dgiot_udp_test_broadcast:run_tests().
dgiot_udp_test_multicast:run_tests().
```

### 方法3：热编译测试

```bash
# 在DGIOT环境中热编译测试模块
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_udp_test).'

# 运行特定测试
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_udp_test_runner:run_all_tests().'
```

## 测试类型说明

### 单播测试
- **基础通信测试**：验证客户端到服务器的单向通信
- **性能测试**：测试消息吞吐量和延迟
- **可靠性测试**：验证不同大小消息的传输可靠性
- **多客户端测试**：验证服务器处理多个客户端的能力
- **错误处理测试**：验证异常情况的处理

### 广播测试
- **本地回环广播**：在本地环境中测试广播功能
- **网络广播**：在网络环境中测试广播功能
- **tcpdump验证**：使用网络抓包工具验证广播包传输

### 多播测试
- **多播组管理**：测试多播组的加入和离开
- **多播消息传输**：验证多播消息的发送和接收
- **多客户端多播**：测试多个客户端接收多播消息

## 测试配置

### 默认端口配置
- **单播服务器端口**：18888
- **单播客户端端口**：18889
- **广播端口**：18999
- **多播端口**：19000

### 测试参数
- **性能测试消息数**：1000条
- **测试超时时间**：30秒
- **消息大小范围**：小消息(5字节)到大消息(5000字节)

## 网络要求

### 单播测试
- 无需特殊网络配置
- 使用本地回环地址(127.0.0.1)

### 广播测试
- 需要网络接口支持广播
- 可能需要管理员权限运行tcpdump

### 多播测试
- 需要网络支持多播
- 可能需要配置多播路由
- 使用标准多播地址范围(224.0.0.0 - 239.255.255.255)

## 故障排除

### 常见问题

1. **权限问题**
   ```bash
   # 为测试脚本添加执行权限
   chmod +x apps/dgiot/src/transport/udp_test/run_all_udp_tests.escript
   ```

2. **依赖模块未加载**
   ```erlang
   % 确保模块已编译并加载
   code:ensure_loaded(dgiot_udp_test_runner).
   ```

3. **端口被占用**
   - 检查是否有其他进程占用测试端口
   - 修改测试配置中的端口号

4. **网络配置问题**
   - 确认网络接口支持广播/多播
   - 检查防火墙设置
   - 验证网络路由配置

### 调试技巧

1. **启用详细日志**
   ```erlang
   % 在测试前设置日志级别
   logger:set_primary_config(level, debug).
   ```

2. **使用tcpdump抓包**
   ```bash
   # 监控特定端口的网络流量
   sudo tcpdump -i any -n port 18888
   ```

3. **检查进程状态**
   ```erlang
   % 查看UDP相关进程
   erlang:processes().
   ```

## 测试报告

测试完成后会生成详细的测试报告，包括：
- 每个测试套件的执行状态
- 性能指标（吞吐量、延迟）
- 错误统计和详细信息
- 测试耗时汇总

## 扩展开发

### 添加新的测试

1. 在相应测试模块中添加测试函数
2. 导出测试函数API
3. 在测试运行器中注册新测试
4. 更新测试脚本支持

### 自定义测试配置

修改测试模块中的宏定义来自定义：
- 测试端口
- 消息内容
- 性能测试参数
- 超时设置

## 技术支持

如有问题请参考：
- DGIOT官方文档
- Erlang/OTP网络编程指南
- 测试代码中的详细注释
