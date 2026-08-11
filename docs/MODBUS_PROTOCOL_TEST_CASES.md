# Modbus协议测试用例正确分类

## 基于代码分析的正确架构：

### 1. Modbus协议类型
1. **MODBUSRTU** - Modbus RTU协议（串口通信）
   - 文件：`modbus_rtu.erl`, `dgiot_modbusrtu_tcp.erl`
   - 特点：基于串口，使用CRC校验

2. **MODBUSTCP** - Modbus TCP协议（网络通信）
   - 文件：`modbus_tcp.erl`, `dgiot_modbusc_tcp.erl`
   - 特点：基于TCP/IP，使用事务标识符

### 2. 通信角色
- **Server/从站**：`dgiot_modbusrtu_tcp.erl` - 作为服务器接收设备连接
- **Client/主站**：`dgiot_modbusc_tcp.erl` - 作为客户端主动连接设备

### 3. 通道类型
- **通用通道类型**：`<<"MODBUS">>`（定义在`dgiot_modbus_channel.erl`）
- **具体实现**：通过配置区分RTU和TCP

## 需要修正的测试用例命名：

### 原有不准确的命名：
- ❌ `register_by_port` → ✅ `modbus_rtu_server_register`
- ❌ `register_by_ip` → ✅ `modbus_tcp_client_connect`
- ❌ `register_by_regular` → ✅ `modbus_rtu_regular_register`

## 新的测试用例分类框架：

### 一、Modbus RTU Server测试（从站/服务器端）
1. **modbus_rtu_server_connection** - RTU服务器连接测试
2. **modbus_rtu_server_register** - RTU设备注册测试
3. **modbus_rtu_data_parsing** - RTU数据解析测试
4. **modbus_rtu_crc_validation** - RTU CRC校验测试

### 二、Modbus TCP Client测试（主站/客户端）
5. **modbus_tcp_client_connection** - TCP客户端连接测试
6. **modbus_tcp_data_exchange** - TCP数据交换测试
7. **modbus_tcp_transaction_id** - TCP事务ID测试

### 三、通用功能测试
8. **modbus_function_codes** - 功能码测试（01, 03, 04, 05, 06, 15, 16等）
9. **modbus_register_access** - 寄存器访问测试
10. **modbus_data_consistency** - 数据一致性测试

### 四、性能与稳定性测试
11. **modbus_pressure_test** - 压力测试
12. **modbus_concurrent_test** - 并发测试
13. **modbus_polling_test** - 轮询机制测试

### 五、异常与容错测试
14. **modbus_timeout_reconnect** - 超时重连测试
15. **modbus_error_handling** - 错误处理测试
16. **modbus_invalid_parameters** - 无效参数测试

### 六、互操作性测试
17. **modbus_interoperability** - 互操作性测试
18. **modbus_gateway_test** - 网关协议转换测试

### 七、安全性测试（针对Modbus TCP）
19. **modbus_tcp_security** - TCP安全性测试
20. **modbus_network_resilience** - 网络韧性测试

## 测试用例总数：20个

## 测试重点：

### Modbus RTU重点：
- 串口通信稳定性
- CRC校验正确性
- 设备注册和识别
- 数据解析准确性

### Modbus TCP重点：
- 网络连接可靠性
- 事务ID管理
- 并发处理能力
- 网络安全防护

## 测试工具建议：
1. **Modbus Poll** - 主站模拟工具
2. **Modbus Slave** - 从站模拟工具
3. **自定义测试脚本** - 针对特定需求
4. **真实设备联调** - 最终验证

## 实施步骤：
1. 先修正现有测试用例命名
2. 添加缺失的测试用例
3. 创建针对性的测试脚本
4. 集成到测试框架中
