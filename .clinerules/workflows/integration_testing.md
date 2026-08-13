# 集成测试工作流

## 概述

本工作流定义了DG-IoT平台集成测试的完整流程，包括测试环境搭建、登录发包、后端日志检查、数据库/API验证、代码修改和热编译循环。

## 核心原则

### 1. 完整测试循环
```
搭建测试环境 → 登录发包 → 检查后端日志 → 验证数据库/API → 发现问题 → 修改代码 → 热编译 → 重新测试
```

### 2. 自动化优先
- **脚本化测试**：所有测试步骤必须脚本化
- **自动化验证**：自动检查日志、数据库、API
- **持续集成**：支持快速迭代和持续测试

## 完整测试工作流程

### 1. 搭建测试环境

#### 1.1 环境要求
```bash
# 1. 启动完整系统
make run

# 2. 检查服务状态
netstat -tlnp | grep :20000

# 3. 清理测试环境
_build/emqx/rel/emqx/bin/emqx eval '
    % 清理测试设备
    DeviceAddr = <<"wrj_dm-zqy-20000">>,
    ProductId = <<"feeb43bffb">>,
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
    dgiot_device:delete(DeviceId).
'
```

#### 1.2 环境验证
- [ ] 系统服务正常运行
- [ ] 必要端口监听正常
- [ ] 日志文件可访问
- [ ] 数据库连接正常

### 2. 登录发包测试

#### 2.1 测试报文发送
```bash
# 发送注册报文
echo "wrj_dm-zqy" | nc -w 5 127.0.0.1 20000

# 发送Modbus数据
python3 -c "
import socket
import struct

# 构建Modbus RTU帧
frame = struct.pack('BB', 0x01, 0x03) + b'\x00\x00\x00\x00' + b'\xC4\x0B'

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
    s.connect(('127.0.0.1', 20000))
    s.sendall(frame)
    print('发送Modbus数据:', frame.hex())
"
```

#### 2.2 测试验证
- [ ] TCP连接成功建立
- [ ] 报文正确发送
- [ ] 系统正确接收报文

### 3. 检查后端日志

#### 3.1 日志监控命令
```bash
# 实时监控日志
tail -f _build/emqx/rel/emqx/log/emqx.log.1 | grep -E "(tcp|RegisterByPort|wrj_dm-zqy|error|DEBUG)"

# 检查特定时间段的日志
LOG_FILE="_build/emqx/rel/emqx/log/emqx.log.1"
TEST_START_TIME=$(date +%s)
grep -n "time\":$(($TEST_START_TIME * 1000000))" "$LOG_FILE"
```

#### 3.2 日志检查要点
- [ ] **TCP连接日志**：`{tcp,#Port<...>,<<"wrj_dm-zqy">>}`
- [ ] **注册方式日志**：`<<"RegisterByPort">>`
- [ ] **产品ID日志**：`<<"feeb43bffb">>`
- [ ] **错误日志**：`error`级别的任何日志
- [ ] **时间戳验证**：日志时间戳必须与测试时间匹配

#### 3.3 时间戳验证
```bash
# 时间戳验证脚本
validate_timestamps() {
    LOG_FILE="$1"
    TEST_START_TIME="$2"
    
    # 提取日志时间戳
    TIMESTAMPS=$(grep -o '"time":[0-9]*' "$LOG_FILE" | cut -d: -f2 | tail -5)
    
    for TS in $TIMESTAMPS; do
        # 纳秒转秒
        LOG_TIME_SEC=$((TS / 1000000))
        TIME_DIFF=$((TEST_START_TIME - LOG_TIME_SEC))
        
        if [ $TIME_DIFF -lt 60 ]; then
            echo "✅ 时间戳匹配: $TS (差异: ${TIME_DIFF}秒)"
        else
            echo "⚠️  时间戳不匹配: $TS (差异: ${TIME_DIFF}秒)"
        fi
    done
}
```

### 4. 验证数据库或API

#### 4.1 数据库验证
```bash
# 验证设备是否注册成功
_build/emqx/rel/emqx/bin/emqx eval '
    DeviceAddr = <<"wrj_dm-zqy-20000">>,
    ProductId = <<"feeb43bffb">>,
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
    
    case dgiot_device:lookup(DeviceId) of
        {ok, Device} ->
            io:format("✅ 设备存在~n"),
            io:format("  设备信息: ~p~n", [Device]);
        {error, not_found} ->
            io:format("❌ 设备未找到~n");
        {error, Reason} ->
            io:format("❌ 设备查询错误: ~p~n", [Reason])
    end.
'
```

#### 4.2 API验证
```bash
# 通过API验证设备状态
curl -X GET "http://127.0.0.1/iotapi/devicecard/wrj_dm-zqy-20000" \
  -H "Authorization: Bearer <redacted>" \
  -H "Content-Type: application/json"
```

#### 4.3 验证标准
- [ ] 设备在数据库中可查询
- [ ] 设备信息完整正确
- [ ] API返回正确数据格式
- [ ] 数据内容符合预期

### 5. 发现问题处理流程

#### 5.1 问题诊断
```bash
# 1. 收集错误信息
ERROR_LOG=$(grep -n "error.*case_clause\|error.*fasle" _build/emqx/rel/emqx/log/emqx.log.1 | tail -1)

# 2. 分析错误原因
echo "错误日志: $ERROR_LOG"

# 3. 定位问题代码
# 根据错误信息定位到具体文件和行号
```

#### 5.2 常见问题类型
1. **编译错误**：语法错误、类型错误
2. **运行时错误**：函数调用错误、模式匹配错误
3. **逻辑错误**：业务逻辑错误、数据处理错误
4. **配置错误**：配置文件错误、环境变量错误

### 6. 修改代码和热编译

#### 6.1 代码修改原则
- **最小修改**：只修改必要部分，避免影响其他功能
- **保持兼容**：不轻易改动对外接口
- **添加日志**：在关键位置添加调试日志
- **遵循规范**：符合编码规范和架构原则

#### 6.2 热编译流程
```bash
# 1. 编译特定插件
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'

# 2. 热加载插件
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_modbus).'

# 3. 验证编译结果
# 检查编译警告和错误
```

#### 6.3 热编译质量要求
- [ ] 零编译警告
- [ ] 无语法错误
- [ ] 类型匹配正确
- [ ] 函数调用正确

### 7. 重新测试循环

#### 7.1 快速测试脚本
```bash
#!/bin/bash
# quick_test_cycle.sh

echo "=== 快速测试循环开始 ==="
echo "时间: $(date)"

# 1. 发送测试报文
echo "1. 发送测试报文..."
echo "wrj_dm-zqy" | nc -w 5 127.0.0.1 20000
sleep 2

# 2. 检查日志
echo "2. 检查后端日志..."
tail -n 20 _build/emqx/rel/emqx/log/emqx.log.1 | grep -E "(tcp|RegisterByPort|error)"

# 3. 验证结果
echo "3. 验证设备注册..."
_build/emqx/rel/emqx/bin/emqx eval '
    DeviceAddr = <<"wrj_dm-zqy-20000">>,
    ProductId = <<"feeb43bffb">>,
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
    
    case dgiot_device:lookup(DeviceId) of
        {ok, _} -> io:format("✅ 设备注册成功~n");
        _ -> io:format("❌ 设备注册失败~n")
    end.
'

echo "=== 测试循环完成 ==="
```

#### 7.2 循环测试策略
- **快速迭代**：每次修改后立即测试
- **增量验证**：只验证修改的部分
- **回归测试**：确保修改不影响现有功能
- **持续监控**：监控系统状态和性能

## 自动化测试脚本模板

### 完整测试循环脚本
```bash
#!/bin/bash
# integration_test_full_cycle.sh

echo "=== 集成测试完整循环 ==="
echo "开始时间: $(date)"

# 配置参数
TEST_DEVICE="wrj_dm-zqy"
TEST_PORT=20000
TEST_PRODUCT="feeb43bffb"
LOG_FILE="_build/emqx/rel/emqx/log/emqx.log.1"

# 1. 记录测试开始时间
TEST_START_TIME=$(date +%s)
echo "测试开始时间戳: $TEST_START_TIME"

# 2. 清理测试环境
cleanup_test_environment() {
    echo "1. 清理测试环境..."
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$TEST_DEVICE-$TEST_PORT\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        dgiot_device:delete(DeviceId),
        io:format(\"清理设备: ~p~n\", [DeviceId]).
    "
}

# 3. 发送测试报文
send_test_packet() {
    echo "2. 发送测试报文..."
    echo "$TEST_DEVICE" | nc -w 5 127.0.0.1 $TEST_PORT
    sleep 3
}

# 4. 检查后端日志
check_backend_logs() {
    echo "3. 检查后端日志..."
    
    if [ ! -f "$LOG_FILE" ]; then
        echo "❌ 日志文件不存在: $LOG_FILE"
        return 1
    fi
    
    # 检查关键日志
    echo "检查关键日志项:"
    
    # TCP连接日志
    if grep -q "tcp.*$TEST_DEVICE" "$LOG_FILE"; then
        echo "✅ TCP连接日志存在"
    else
        echo "❌ TCP连接日志不存在"
    fi
    
    # RegisterByPort日志
    if grep -q "RegisterByPort" "$LOG_FILE"; then
        echo "✅ RegisterByPort日志存在"
    else
        echo "❌ RegisterByPort日志不存在"
    fi
    
    # 错误日志
    ERROR_COUNT=$(grep -c "error" "$LOG_FILE")
    if [ "$ERROR_COUNT" -eq 0 ]; then
        echo "✅ 无错误日志"
    else
        echo "❌ 发现 $ERROR_COUNT 个错误日志"
        grep "error" "$LOG_FILE" | tail -5
        return 1
    fi
    
    return 0
}

# 5. 验证数据库
verify_database() {
    echo "4. 验证数据库..."
    
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$TEST_DEVICE-$TEST_PORT\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        
        case dgiot_device:lookup(DeviceId) of
            {ok, Device} ->
                io:format(\"✅ 设备注册成功~n\"),
                io:format(\"  设备ID: ~p~n\", [DeviceId]),
                io:format(\"  设备信息: ~p~n\", [Device]);
            {error, not_found} ->
                io:format(\"❌ 设备未找到~n\");
            {error, Reason} ->
                io:format(\"❌ 设备查询错误: ~p~n\", [Reason])
        end.
    "
}

# 6. 测试循环
test_cycle() {
    echo ""
    echo "=== 执行测试循环 ==="
    
    cleanup_test_environment
    send_test_packet
    
    if check_backend_logs; then
        verify_database
        echo "✅ 测试循环成功"
        return 0
    else
        echo "❌ 测试循环失败，需要检查问题"
        return 1
    fi
}

# 7. 问题处理循环
problem_solving_cycle() {
    echo ""
    echo "=== 问题处理循环 ==="
    
    local attempt=1
    local max_attempts=3
    
    while [ $attempt -le $max_attempts ]; do
        echo "尝试 $attempt/$max_attempts"
        
        if test_cycle; then
            echo "✅ 问题解决"
            return 0
        else
            echo "❌ 测试失败，等待修改代码..."
            echo "请修改代码后按回车继续..."
            read -r
            
            # 热编译
            echo "执行热编译..."
            _build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'
            _build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_modbus).'
            
            attempt=$((attempt + 1))
        fi
    done
    
    echo "❌ 超过最大尝试次数，问题未解决"
    return 1
}

# 主函数
main() {
    echo "=== 集成测试完整工作流程 ==="
    
    # 检查系统状态
    if ! netstat -tlnp | grep ":$TEST_PORT" > /dev/null; then
        echo "❌ 系统未运行在端口 $TEST_PORT"
        echo "请先启动系统: make run"
        return 1
    fi
    
    # 执行测试
    if test_cycle; then
        echo "✅ 集成测试成功"
    else
        echo "⚠️  测试失败，进入问题处理循环"
        problem_solving_cycle
    fi
    
    echo "=== 测试完成 ==="
    echo "结束时间: $(date)"
}

main "$@"
```

## 检查清单

### 测试环境检查清单
- [ ] 系统服务正常运行
- [ ] 端口监听正常
- [ ] 日志文件可访问
- [ ] 数据库连接正常
- [ ] 测试环境已清理

### 测试执行检查清单
- [ ] 测试报文正确发送
- [ ] 系统正确接收报文
- [ ] 后端日志记录完整
- [ ] 时间戳匹配验证通过
- [ ] 无错误日志

### 验证检查清单
- [ ] 数据库数据正确
- [ ] API响应正确
- [ ] 功能符合预期
- [ ] 性能满足要求

### 问题处理检查清单
- [ ] 错误信息收集完整
- [ ] 问题原因分析清楚
- [ ] 代码修改符合规范
- [ ] 热编译无警告
- [ ] 重新测试验证通过

## 最佳实践

### 1. 测试脚本管理
- **版本控制**：测试脚本纳入版本管理
- **文档完整**：每个脚本有完整的使用说明
- **参数化**：使用参数提高脚本灵活性
- **错误处理**：完善的错误处理和日志记录

### 2. 团队协作
- **统一流程**：所有成员使用相同的测试流程
- **知识共享**：测试经验和问题解决方案团队共享
- **持续改进**：根据测试经验不断优化测试流程

### 3. 自动化集成
- **CI/CD集成**：将测试流程集成到持续集成系统
- **自动化报告**：自动生成测试报告
- **监控告警**：测试失败自动告警

## 更新记录

- **2026-01-26**：创建集成测试工作流
- **基于用户需求**：完整的测试环境搭建、登录发包、后端日志检查、数据库/API验证、代码修改和热编译循环
- **核心价值**：提供标准化的集成测试流程，支持快速迭代和问题解决

---

**使用说明**：所有集成测试必须遵循此工作流程，确保测试的规范性和有效性。对于复杂问题，使用问题处理循环进行快速迭代和验证。