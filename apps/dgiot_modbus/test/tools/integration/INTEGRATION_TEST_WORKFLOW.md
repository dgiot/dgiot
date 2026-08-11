# 系统集成测试工作流程规范

## 概述

本文档定义了DG-IoT平台系统集成测试的标准工作流程，确保测试的规范性和有效性。

## 1. 集成测试核心原则

### 1.1 测试目标
- **验证功能正确性**：确保功能按预期工作
- **验证系统集成**：确保各组件协同工作
- **验证错误处理**：确保系统能正确处理异常
- **验证性能指标**：确保系统满足性能要求

### 1.2 测试环境要求
- **完整系统环境**：必须搭建完整的DG-IoT平台
- **真实网络环境**：使用实际TCP/UDP连接
- **后端日志监控**：必须监控后端系统日志
- **数据库验证**：必须验证数据持久化

## 2. 标准测试工作流程

### 2.1 测试准备阶段
```
1. 搭建完整系统环境
2. 启动所有必要服务
3. 配置测试参数
4. 清理测试环境
```

### 2.2 测试执行阶段
```
1. 登录系统（如果需要）
2. 发送测试报文
3. 监控后端系统日志
4. 验证系统响应
```

### 2.3 问题排查阶段
```
1. 检查日志错误
2. 分析错误原因
3. 修改代码
4. 重新编译
5. 重新测试
```

### 2.4 验证完成阶段
```
1. 验证所有功能点
2. 验证数据持久化
3. 验证性能指标
4. 生成测试报告
```

## 3. RegisterByPort集成测试详细流程

### 3.1 测试准备
```bash
# 1. 启动DG-IoT平台
make run

# 2. 检查服务状态
netstat -tlnp | grep :20000

# 3. 清理测试环境
_build/emqx/rel/emqx/bin/emqx eval '
    DeviceAddr = <<"wrj_dm-zqy-20000">>,
    ProductId = <<"feeb43bffb">>,
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
    dgiot_device:delete(DeviceId).
'
```

### 3.2 测试执行
```bash
# 1. 运行集成测试脚本
bash apps/dgiot_modbus/test/tools/integration/test_runners/integration_test_registerbyport.sh

# 2. 监控后端系统日志（必须步骤）
tail -f _build/emqx/rel/emqx/log/emqx.log.1 | grep -E "(RegisterByPort|wrj_dm-zqy|20000|tcp|error)"
```

### 3.3 日志检查要点

#### 必须检查的日志项
1. **TCP连接日志**：`{tcp,#Port<...>,<<"wrj_dm-zqy">>}`
2. **注册方式日志**：`<<"RegisterByPort">>`
3. **产品ID日志**：`<<"feeb43bffb">>`
4. **错误日志**：`error`级别的任何日志
5. **时间戳验证**：日志时间戳必须与测试时间匹配

#### 日志时间戳验证方法
```bash
# 获取当前时间戳
date +%s

# 查看日志时间戳
grep -o '"time":[0-9]*' _build/emqx/rel/emqx/log/emqx.log.1 | tail -5

# 时间戳转换（示例）
# 1766578871398646 → 2025-12-26 02:21:11
# 当前时间：1766730691 → 2025-12-26 14:31:31
```

### 3.4 问题排查流程

#### 发现错误时的处理流程
```
1. 记录错误信息
2. 分析错误原因
3. 定位问题代码
4. 修改代码
5. 重新编译
6. 重新测试
7. 验证修复
```

#### 常见错误类型
1. **编译错误**：语法错误、类型错误
2. **运行时错误**：函数调用错误、模式匹配错误
3. **逻辑错误**：业务逻辑错误、数据处理错误
4. **性能错误**：内存泄漏、CPU占用过高

## 4. 测试验证标准

### 4.1 功能验证标准
- [ ] TCP连接成功建立
- [ ] 注册报文正确接收
- [ ] 设备地址正确生成（注册报文 + "-" + 服务器端口）
- [ ] 设备成功保存到数据库
- [ ] 设备信息完整（devaddr, productid, status, isEnable等）

### 4.2 日志验证标准
- [ ] 后端系统日志中有TCP连接记录
- [ ] 后端系统日志中有注册报文记录
- [ ] 后端系统日志时间戳与测试时间匹配
- [ ] 后端系统日志无错误记录（error级别）

### 4.3 数据验证标准
- [ ] 设备在数据库中可查询
- [ ] 设备信息完整正确
- [ ] 设备状态正确设置
- [ ] 设备密钥正确生成

## 5. 测试脚本模板

### 5.1 基础测试脚本模板
```bash
#!/bin/bash
# integration_test_template.sh

echo "=== 系统集成测试开始 ==="
echo "测试时间: $(date)"

# 1. 检查系统状态
check_system_status() {
    echo "1. 检查系统状态..."
    # 检查服务是否运行
    # 检查端口是否监听
    # 检查日志文件是否存在
}

# 2. 执行测试操作
execute_test() {
    echo "2. 执行测试操作..."
    # 发送测试报文
    # 执行测试命令
}

# 3. 检查后端日志
check_backend_logs() {
    echo "3. 检查后端日志..."
    LOG_FILE="_build/emqx/rel/emqx/log/emqx.log.1"
    if [ -f "$LOG_FILE" ]; then
        echo "检查日志文件: $LOG_FILE"
        # 检查相关日志
        # 验证时间戳
        # 检查错误信息
    else
        echo "❌ 日志文件不存在: $LOG_FILE"
    fi
}

# 4. 验证测试结果
verify_test_results() {
    echo "4. 验证测试结果..."
    # 验证数据库数据
    # 验证系统状态
    # 验证功能正确性
}

# 执行测试流程
main() {
    check_system_status
    execute_test
    check_backend_logs
    verify_test_results
    
    echo "=== 测试完成 ==="
}

main "$@"
```

### 5.2 RegisterByPort测试脚本增强版
```bash
#!/bin/bash
# integration_test_registerbyport_enhanced.sh

echo "=== RegisterByPort增强版集成测试 ==="
echo "测试开始时间: $(date)"
echo "当前时间戳: $(date +%s)"

# 记录测试开始时间
TEST_START_TIME=$(date +%s)

# 1. 检查系统状态
check_system_status() {
    echo "1. 检查系统状态..."
    
    # 检查服务器端口
    if netstat -tlnp | grep :20000 > /dev/null; then
        echo "✅ 服务器正在监听端口20000"
    else
        echo "❌ 服务器未监听端口20000"
        return 1
    fi
    
    # 检查日志文件
    LOG_FILE="_build/emqx/rel/emqx/log/emqx.log.1"
    if [ -f "$LOG_FILE" ]; then
        echo "✅ 日志文件存在: $LOG_FILE"
        # 备份当前日志
        cp "$LOG_FILE" "${LOG_FILE}.backup_$(date +%Y%m%d_%H%M%S)"
    else
        echo "⚠️  日志文件不存在: $LOG_FILE"
    fi
    
    return 0
}

# 2. 执行TCP连接测试
execute_tcp_test() {
    echo "2. 执行TCP连接测试..."
    
    # 记录测试前日志位置
    LOG_FILE="_build/emqx/rel/emqx/log/emqx.log.1"
    if [ -f "$LOG_FILE" ]; then
        LOG_LINES_BEFORE=$(wc -l < "$LOG_FILE")
        echo "测试前日志行数: $LOG_LINES_BEFORE"
    fi
    
    # 执行TCP连接
    echo "连接到服务器端口20000..."
    echo "发送注册报文: wrj_dm-zqy"
    
    # 使用nc发送注册报文
    echo "wrj_dm-zqy" | nc -w 5 127.0.0.1 20000
    
    # 等待处理
    sleep 2
    
    # 记录测试后日志位置
    if [ -f "$LOG_FILE" ]; then
        LOG_LINES_AFTER=$(wc -l < "$LOG_FILE")
        echo "测试后日志行数: $LOG_LINES_AFTER"
        NEW_LOG_LINES=$((LOG_LINES_AFTER - LOG_LINES_BEFORE))
        echo "新增日志行数: $NEW_LOG_LINES"
    fi
    
    return 0
}

# 3. 检查后端日志（关键步骤）
check_backend_logs() {
    echo "3. 检查后端日志..."
    
    LOG_FILE="_build/emqx/rel/emqx/log/emqx.log.1"
    if [ ! -f "$LOG_FILE" ]; then
        echo "❌ 日志文件不存在: $LOG_FILE"
        return 1
    fi
    
    # 检查测试期间的日志
    echo "检查测试期间的后端日志..."
    
    # 查找TCP连接日志
    TCP_LOG=$(grep -n "tcp.*wrj_dm-zqy" "$LOG_FILE" | tail -1)
    if [ -n "$TCP_LOG" ]; then
        echo "✅ 找到TCP连接日志:"
        echo "   $TCP_LOG"
        
        # 提取时间戳
        TIMESTAMP=$(echo "$TCP_LOG" | grep -o '"time":[0-9]*' | cut -d: -f2)
        if [ -n "$TIMESTAMP" ]; then
            echo "   日志时间戳: $TIMESTAMP"
            echo "   测试开始时间戳: $TEST_START_TIME"
            
            # 时间戳转换（纳秒转秒）
            LOG_TIME_SEC=$((TIMESTAMP / 1000000))
            TIME_DIFF=$((TEST_START_TIME - LOG_TIME_SEC))
            
            if [ $TIME_DIFF -lt 60 ]; then
                echo "✅ 日志时间戳与测试时间匹配（差异: ${TIME_DIFF}秒）"
            else
                echo "⚠️  日志时间戳与测试时间不匹配（差异: ${TIME_DIFF}秒）"
            fi
        fi
    else
        echo "❌ 未找到TCP连接日志"
    fi
    
    # 查找RegisterByPort日志
    REGISTER_LOG=$(grep -n "RegisterByPort" "$LOG_FILE" | tail -1)
    if [ -n "$REGISTER_LOG" ]; then
        echo "✅ 找到RegisterByPort日志:"
        echo "   $REGISTER_LOG"
    else
        echo "❌ 未找到RegisterByPort日志"
    fi
    
    # 查找错误日志
    ERROR_LOG=$(grep -n "error.*case_clause\|error.*fasle" "$LOG_FILE" | tail -1)
    if [ -n "$ERROR_LOG" ]; then
        echo "❌ 找到错误日志:"
        echo "   $ERROR_LOG"
        return 1
    else
        echo "✅ 未找到错误日志"
    fi
    
    return 0
}

# 4. 验证设备注册
verify_device_registration() {
    echo "4. 验证设备注册..."
    
    # 验证设备是否注册成功
    echo "检查设备是否已注册..."
    
    DEVICE_ADDR="wrj_dm-zqy-20000"
    PRODUCT_ID="feeb43bffb"
    
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$PRODUCT_ID\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        
        io:format(\"验证设备注册:~n\"),
        io:format(\"  设备地址: ~p~n\", [DeviceAddr]),
        io:format(\"  产品ID: ~p~n\", [ProductId]),
        io:format(\"  设备ID: ~p~n\", [DeviceId]),
        
        case dgiot_device:lookup(DeviceId) of
            {ok, Device} ->
                io:format(\"✅ 设备存在~n\"),
                io:format(\"  设备信息: ~p~n\", [Device]),
                % 检查关键字段
                DevAddr = maps:get(<<\"devaddr\">>, Device, <<>>),
                Status = maps:get(<<\"status\">>, Device, false),
                IsEnable = maps:get(<<\"isEnable\">>, Device, false),
                
                io:format(\"  设备地址: ~p (预期: ~p)~n\", [DevAddr, DeviceAddr]),
                io:format(\"  设备状态: ~p (预期: true)~n\", [Status]),
                io:format(\"  启用状态: ~p (预期: true)~n\", [IsEnable]);
            {error, not_found} ->
                io:format(\"❌ 设备未找到~n\");
            {error, Reason} ->
                io:format(\"❌ 设备查询错误: ~p~n\", [Reason])
        end.
    "
}

# 执行测试流程
main() {
    echo "=== 测试开始 ==="
    
    # 检查系统状态
    if ! check_system_status; then
        echo "❌ 系统状态检查失败"
        return 1
    fi
    
    # 执行TCP测试
    if ! execute_tcp_test; then
        echo "❌ TCP连接测试失败"
        return 1
    fi
    
    # 检查后端日志
    if ! check_backend_logs; then
        echo "❌ 后端日志检查失败"
        return 1
    fi
    
    # 验证设备注册
    verify_device_registration
    
    echo "=== 测试完成 ==="
    echo "测试结束时间: $(date)"
}

main "$@"
```

## 6. 更新记录

- **2025-12-26**: 创建系统集成测试工作流程规范
- **基于用户反馈**：必须检查后端系统日志，验证时间戳匹配
- **核心改进**：将"检查后端日志"作为集成测试的必要步骤

---

**重要提示**：所有集成测试必须遵循此工作流程，特别是：
1. **必须检查后端系统日志**：`_build/emqx/rel/emqx/log/emqx.log.1`
2. **必须验证时间戳匹配**：日志时间戳与测试时间必须在合理范围内
3. **必须检查错误日志**：任何error级别的日志都需要分析处理
4. **必须遵循问题排查流程**：发现错误→修改代码→重新编译→重新测试
