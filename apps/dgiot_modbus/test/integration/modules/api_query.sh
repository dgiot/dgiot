#!/bin/bash

# 错误处理
set -euo pipefail
trap 'echo "脚本执行失败: $?" >&2' ERR
#!/bin/bash
# api_query.sh - API查询和数据存储模块

# 模块4: API查询测试
module_api_query() {
    log_info "模块4: API查询测试"
    
    # 获取设备ID
    DEVICE_ID="${TEST_PRODUCT}_${DEVICE_ADDR}"
    
    echo "4.1 查询API实时值..."
    echo "设备ID: ${DEVICE_ID}"
    
    # 使用curl查询API
    API_URL="http://127.0.0.1/iotapi/devicecard/${DEVICE_ID}"
    
    # 使用预配置的Cookie（基于用户提供的调试信息）
    COOKIES="Admin-Token=r:a1d8422a576e581c20fb91a01bc19ce6; sessiontoken=r:a1d8422a576e581c20fb91a01bc19ce6; departmenttoken=r:a1d8422a576e581c20fb91a01bc19ce6"
    
    echo "API URL: ${API_URL}"
    
    # 发送请求
    RESPONSE=$(curl -s -H "Cookie: ${COOKIES}" "${API_URL}")
    
    if [ $? -eq 0 ]; then
        echo "✅ API查询成功"
        echo "响应: ${RESPONSE:0:200}..."
        
        # 检查响应是否包含数据
        if echo "$RESPONSE" | grep -q '"data"'; then
            log_success "API返回有效数据"
        else
            log_warning "API返回数据格式异常"
        fi
    else
        log_error "API查询失败"
    fi
    
    echo ""
    echo "4.2 查询设备属性..."
    
    PROPERTIES_URL="http://127.0.0.1/iotapi/device_properties"
    PROPERTIES_PARAMS="productId=${TEST_PRODUCT}&deviceAddr=${DEVICE_ADDR}"
    
    echo "URL: ${PROPERTIES_URL}?${PROPERTIES_PARAMS}"
    
    PROPERTIES_RESPONSE=$(curl -s -H "Cookie: ${COOKIES}" "${PROPERTIES_URL}?${PROPERTIES_PARAMS}")
    
    if [ $? -eq 0 ]; then
        echo "✅ 设备属性查询成功"
        echo "响应: ${PROPERTIES_RESPONSE:0:200}..."
    else
        log_error "设备属性查询失败"
    fi
    
    return 0
}

# 检查API响应
check_api_response() {
    log_info "检查API响应..."
    
    # 获取设备ID
    DEVICE_ID="${TEST_PRODUCT}_${DEVICE_ADDR}"
    API_URL="http://127.0.0.1/iotapi/devicecard/${DEVICE_ID}"
    COOKIES="Admin-Token=r:a1d8422a576e581c20fb91a01bc19ce6; sessiontoken=r:a1d8422a576e581c20fb91a01bc19ce6; departmenttoken=r:a1d8422a576e581c20fb91a01bc19ce6"
    
    # 发送请求
    RESPONSE=$(curl -s -H "Cookie: ${COOKIES}" "${API_URL}")
    
    if [ $? -ne 0 ]; then
        log_error "API请求失败"
        return 1
    fi
    
    # 检查响应状态
    if echo "$RESPONSE" | grep -q '"code":200'; then
        log_success "API响应状态正常 (code: 200)"
        return 0
    elif echo "$RESPONSE" | grep -q '"code":'; then
        local code=$(echo "$RESPONSE" | grep -o '"code":[0-9]*' | cut -d: -f2)
        log_warning "API响应状态异常 (code: $code)"
        return 1
    else
        log_error "API响应格式异常"
        return 1
    fi
}

# 模块6: 数据存储验证
module_data_storage() {
    log_info "模块6: 数据存储验证"
    
    echo "6.1 检查TDengine数据存储..."
    
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        
        dgiot_utils:safe_format("检查数据存储状态:~n", []),
        
        % 模拟TDengine查询
        case dgiot_tdengine_adapter:query(ProductId, <<\"SELECT COUNT(*) FROM \\\"device_\\\" || ? WHERE devaddr = ?\">>, [DeviceId, DeviceAddr]) of
            {ok, #{<<"data">> := [[Count]]}} when Count > 0 ->
                dgiot_utils:safe_format("✅ TDengine中有 ~p 条设备数据~n", [Count]);
            {ok, #{<<"data">> := [[0]]}} ->
                dgiot_utils:safe_format("⚠️  TDengine中暂无设备数据~n", []);
            {error, Reason} ->
                dgiot_utils:safe_format("⚠️  TDengine查询错误: ~p~n", [Reason])
        end,
        
        % 检查缓存数据
        dgiot_utils:safe_format("检查缓存数据:~n", []),
        case dgiot_data:match({last_data, '_'}) of
            [] ->
                dgiot_utils:safe_format("⚠️  缓存中没有last_data记录~n", []);
            CacheList ->
                CacheCount = length(CacheList),
                dgiot_utils:safe_format("✅ 缓存中有 ~p 条last_data记录~n", [CacheCount])
        end.
    "
    
    return 0
}
