#!/bin/bash

# 错误处理
set -euo pipefail
trap 'echo "脚本执行失败: $?" >&2' ERR
#!/bin/bash
# Modbus API测试脚本（带认证）

# 配置
API_BASE="http://127.0.0.1/iotapi"
USERNAME="dgiot_dev"
PASSWORD="dgiot_dev"
DEVICE_ID="88a27d8587"

echo "=== Modbus API测试（带认证）==="
echo "开始时间: $(date)"
echo ""

# 1. 登录获取token
echo "1. 登录获取token..."
LOGIN_RESPONSE=$(curl -s -X POST "${API_BASE}/login" \
  -H "Content-Type: text/plain" \
  -d "{\"username\":\"${USERNAME}\",\"password\":\"${PASSWORD}\"}")

echo "登录响应原始数据:"
echo "$LOGIN_RESPONSE"

# 提取token - 从响应中提取sessionToken或access_token
TOKEN=$(echo "$LOGIN_RESPONSE" | jq -r '.sessionToken // .access_token // ""' 2>/dev/null || echo "")

if [ -z "$TOKEN" ] || [ "$TOKEN" = "null" ]; then
    echo "❌ 获取token失败"
    echo "完整响应: $LOGIN_RESPONSE"
    exit 1
fi

echo "✅ 获取token成功: ${TOKEN:0:20}..."
echo ""

# 2. 测试设备卡片API
echo "2. 测试设备卡片API..."
echo "使用token: ${TOKEN:0:20}..."
DEVICE_RESPONSE=$(curl -s -X GET "${API_BASE}/devicecard/${DEVICE_ID}" \
  -H "sessionToken: ${TOKEN}" \
  -H "Content-Type: application/json")

echo "设备卡片响应状态码: $?"
echo "设备卡片响应原始数据:"
echo "$DEVICE_RESPONSE"
echo ""

# 尝试解析JSON
if echo "$DEVICE_RESPONSE" | jq -e . >/dev/null 2>&1; then
    echo "设备卡片响应(格式化):"
    echo "$DEVICE_RESPONSE" | jq '.'
else
    echo "设备卡片响应不是有效的JSON"
fi

# 检查数据
DATA_COUNT=$(echo "$DEVICE_RESPONSE" | jq '.data | length')
echo ""
echo "数据项数量: $DATA_COUNT"

if [ "$DATA_COUNT" -gt 0 ]; then
    echo "3. 检查每个属性的值:"
    echo "$DEVICE_RESPONSE" | jq -r '.data[] | "\(.name) (\(.identifier)): \(.value) \(.unit) [\(.time)]"' | while read -r line; do
        echo "  $line"
    done
    
    # 检查是否有空值
    EMPTY_COUNT=$(echo "$DEVICE_RESPONSE" | jq '[.data[] | select(.value == "" or .value == null)] | length')
    if [ "$EMPTY_COUNT" -gt 0 ]; then
        echo ""
        echo "⚠️  警告: 有 $EMPTY_COUNT 个属性的值为空"
        echo "空值属性:"
        echo "$DEVICE_RESPONSE" | jq -r '.data[] | select(.value == "" or .value == null) | "  - \(.name) (\(.identifier))"' | while read -r line; do
            echo "$line"
        done
    else
        echo ""
        echo "✅ 所有属性都有值"
    fi
else
    echo "❌ 没有获取到设备数据"
fi

echo ""
echo "=== 测试完成 ==="
echo "结束时间: $(date)"
