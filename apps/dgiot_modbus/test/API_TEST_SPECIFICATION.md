# Modbus API测试用例规范

## 概述
本文档定义了Modbus插件API测试的规范和最佳实践，确保测试代码的一致性和可维护性。

## 目录结构
```
apps/dgiot_modbus/test/
├── tools/                          # 测试工具和脚本
│   ├── test_modbus_api_with_auth.sh  # 带认证的API测试脚本
│   └── dgiot_modbus_fix.erl       # 修复工具
├── unit/                          # 单元测试
├── eunit/                         # EUnit测试
├── ct/                            # Common Test
├── data/                          # 测试数据
└── coverage/                      # 测试覆盖率报告
```

## 测试脚本规范

### 1. 命名规范
- **Shell脚本**: `test_<功能>_<描述>.sh`
- **Erlang测试**: `*_test.erl` (单元测试), `*_SUITE.erl` (Common Test)

### 2. 脚本结构
每个测试脚本应包含以下部分：

```bash
#!/bin/bash
# 脚本描述

# 配置部分
API_BASE="http://127.0.0.1/iotapi"
USERNAME="dgiot_dev"
PASSWORD="dgiot_dev"

# 日志输出
echo "=== 测试名称 ==="
echo "开始时间: $(date)"

# 测试步骤
# 1. 准备
# 2. 执行
# 3. 验证

# 清理和总结
echo "=== 测试完成 ==="
echo "结束时间: $(date)"
```

### 3. 错误处理
- 使用明确的错误消息
- 适当的退出代码
- 清理临时资源

### 4. 输出格式
- 使用统一的前缀：`✅` 成功, `❌` 失败, `⚠️` 警告
- 结构化输出，便于解析

## API测试工作流

### 1. 认证测试
```bash
# 登录获取token
LOGIN_RESPONSE=$(curl -s -X POST "${API_BASE}/login" \
  -H "Content-Type: text/plain" \
  -d "{\"username\":\"${USERNAME}\",\"password\":\"${PASSWORD}\"}")

# 提取token
TOKEN=$(echo "$LOGIN_RESPONSE" | jq -r '.sessionToken // .access_token // ""')
```

### 2. 设备数据测试
```bash
# 调用设备卡片API
DEVICE_RESPONSE=$(curl -s -X GET "${API_BASE}/devicecard/${DEVICE_ID}" \
  -H "sessionToken: ${TOKEN}" \
  -H "Content-Type: application/json")

# 验证响应
if echo "$DEVICE_RESPONSE" | jq -e . >/dev/null 2>&1; then
    # 有效JSON响应
    DATA_COUNT=$(echo "$DEVICE_RESPONSE" | jq '.data | length')
fi
```

### 3. 数据验证
- 检查响应格式
- 验证数据完整性
- 检查时间戳
- 验证数值范围

## 示例：test_modbus_api_with_auth.sh

### 功能
1. 自动化登录获取认证token
2. 测试设备卡片API
3. 验证Modbus设备数据解析

### 使用方式
```bash
# 进入测试目录
cd /root/gitee/dgiot/apps/dgiot_modbus/test/tools

# 添加执行权限
chmod +x test_modbus_api_with_auth.sh

# 运行测试
./test_modbus_api_with_auth.sh
```

### 输出示例
```
=== Modbus API测试（带认证）===
开始时间: Wed Dec 24 15:31:46 CST 2025

1. 登录获取token...
✅ 获取token成功: r:3306b39bc1377b17c3...

2. 测试设备卡片API...
设备卡片响应(格式化):
{
  "data": [
    {
      "value": "",
      "unit": "°/s",
      "type": "double",
      "time": "2025-12-24 15:31:49",
      ...
    }
  ]
}

数据项数量: 6
⚠️  警告: 有 6 个属性的值为空

=== 测试完成 ===
结束时间: Wed Dec 24 15:31:49 CST 2025
```

## 集成到开发流程

### 1. 预提交检查
```bash
# 在git pre-commit钩子中添加测试
cd apps/dgiot_modbus/test/tools && ./test_modbus_api_with_auth.sh
```

### 2. CI/CD集成
```yaml
# GitHub Actions示例
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - name: Run API tests
        run: |
          cd apps/dgiot_modbus/test/tools
          ./test_modbus_api_with_auth.sh
```

### 3. 监控和告警
- 定期运行API测试
- 监控测试结果
- 设置告警阈值

## 维护指南

### 1. 更新测试数据
- 当API响应格式变化时更新测试
- 定期更新测试设备ID
- 维护测试账号凭证

### 2. 添加新测试
1. 在`tools/`目录下创建新脚本
2. 遵循命名规范
3. 包含完整的错误处理
4. 更新本文档

### 3. 故障排除
- 检查网络连接
- 验证API端点可用性
- 检查认证token有效性
- 查看服务器日志

## 相关文档
1. [DG-IoT开发规则](../.clinerules/development_rules.md)
2. [API管理规则](../.clinerules/api_rules.md)
3. [Modbus调试规范](../.clinerules/debug_protocol_issues.md)

## 更新记录
- 2025-12-24: 创建API测试用例规范
- 2025-12-24: 添加test_modbus_api_with_auth.sh示例
