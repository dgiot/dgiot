# Modbus块数据上报测试执行指南

## 平台状态检查

✅ **平台已启动** - 检测到3个相关进程

## 快速开始

### 1. 运行Modbus块数据测试
```bash
cd /root/gitee/dgiot
chmod +x apps/dgiot_modbus/test/tools/integration/test_runners/test_modbus_block_data.sh
./apps/dgiot_modbus/test/tools/integration/test_runners/test_modbus_block_data.sh
```

### 2. 运行Modbus环境检查
```bash
cd /root/gitee/dgiot
chmod +x apps/dgiot_modbus/test/tools/integration/test_runners/test_modbus_env_check.sh
./apps/dgiot_modbus/test/tools/integration/test_runners/test_modbus_env_check.sh
```

### 3. 运行分析脚本（不依赖平台）
```bash
cd /root/gitee/dgiot
chmod +x apps/dgiot_modbus/test/tools/integration/analysis/analyze_modbus_flow.sh
./apps/dgiot_modbus/test/tools/integration/analysis/analyze_modbus_flow.sh
```

### 4. 使用调试工具
```bash
# 运行调试分析脚本
cd /root/gitee/dgiot
chmod +x apps/dgiot_modbus/test/tools/integration/analysis/analyze_modbus_flow.sh
./apps/dgiot_modbus/test/tools/integration/analysis/analyze_modbus_flow.sh

# 或者运行自闭合调试报告
chmod +x apps/dgiot_modbus/test/tools/integration/analysis/self_closed_debug_report.sh
./apps/dgiot_modbus/test/tools/integration/analysis/self_closed_debug_report.sh
```

## 分步测试指南

### 步骤1：使用集成测试脚本
```bash
# 运行完整的Modbus环境检查
cd /root/gitee/dgiot
./apps/dgiot_modbus/test/tools/integration/test_runners/test_modbus_env_check.sh

# 或者运行特定的测试脚本
./apps/dgiot_modbus/test/tools/integration/test_runners/test_modbus_block_data.sh
```

### 步骤2：验证平台运行状态
```bash
# 检查平台进程
pgrep -f "emqx" && echo "✅ EMQX运行中" || echo "❌ EMQX未运行"

# 检查Modbus模块
_build/emqx/rel/emqx/bin/emqx eval 'code:which(dgiot_modbusrtu_tcp).'
```

### 步骤3：编译和加载Modbus模块
```bash
# 热编译Modbus模块
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'

# 热加载Modbus模块
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_modbus).'
```

### 步骤4：测试数据解析
```bash
# 运行Erlang测试用例
cd /root/gitee/dgiot
erl -pa apps/dgiot_modbus/ebin -pa apps/dgiot_task/ebin -pa /tmp \
  -eval 'test_modbus_parsing:test(), init:stop().'
```

### 步骤5：测试公式计算器
```bash
# 测试公式计算功能
_build/emqx/rel/emqx/bin/emqx eval '
Collection = <<"(%%{block_data} - 100) * 0.1">>,
Variables = #{<<"block_data">> => 150},
Prop = #{<<"dataType">> => #{<<"type">> => <<"FLOAT">>, <<"specs">> => #{<<"precision">> => 2}}},
Result = dgiot_formula_calculator_simple:calculate_formula(Collection, Variables, Prop),
io:format("公式计算结果: ~p~n", [Result]).
'
```

### 步骤6：检查缓存数据
```bash
# 检查缓存状态
_build/emqx/rel/emqx/bin/emqx eval '
DeviceId = <<"test_device_123">>,
case dgiot_data:get({last_data, DeviceId}) of
    not_find -> io:format("缓存为空~n");
    Data -> io:format("缓存数据: ~p~n", [Data])
end.
'
```

### 步骤6：测试API查询（需要认证token）
```bash
# 获取认证token（需要先登录）
# curl -X POST "http://127.0.0.1/iotapi/login" \
#   -H "Content-Type: application/json" \
#   -d '{"username":"admin","password":"admin"}'

# 查询设备实时数据
# curl -X GET "http://127.0.0.1/iotapi/devicecard/test_device_123" \
#   -H "Authorization: Bearer your_token_here"
```

## 模拟数据上报测试

### 模拟Modbus RTU数据
```erlang
%% 创建测试数据
TestData = <<16#01, 16#03, 16#04, 16#00, 16#00, 16#00, 16#00, 16#C4, 16#0B>>,

%% 模拟解析过程
State = #{<<"dtuproduct">> => <<"feeb43bffb">>, 
          <<"slaveId">> => 1, 
          <<"dtuaddr">> => <<"test_device">>, 
          <<"address">> => 0},

%% 调用解析函数
Result = modbus_rtu:parse_frame(TestData, #{}, State).
```

### 模拟数据块处理
```erlang
%% 模拟数据块缓存
DataBlockCache = #{<<"block_data">> => <<16#00, 16#00, 16#00, 16#00>>},

%% 模拟物模型属性
Props = [
    #{<<"identifier">> => <<"angular_x">>,
      <<"dataForm">> => #{<<"strategy">> => <<"计算值">>,
                         <<"collection">> => <<"(%%{block_data} - 100) * 0.1">>},
      <<"dataSource">> => #{<<"key">> => <<"block_data">>}}
],

%% 调用数据块处理（如果模块存在）
%% Result = modbus_rtu_data_blocks:process_data_blocks(DataBlockCache, Props).
```

## 问题排查

### 如果测试失败，按以下步骤排查：

1. **检查平台日志**
   ```bash
   tail -f logs/console.log | grep -E "(ERROR|WARNING|modbus)"
   ```

2. **检查模块加载**
   ```bash
   _build/emqx/rel/emqx/bin/emqx eval '
   io:format("Modbus模块: ~p~n", [code:which(dgiot_modbusrtu_tcp)]),
   io:format("协议模块: ~p~n", [code:which(modbus_rtu)]),
   io:format("任务模块: ~p~n", [code:which(dgiot_task)]).
   '
   ```

3. **检查产品配置**
   ```bash
   _build/emqx/rel/emqx/bin/emqx eval '
   case dgiot_product:lookup_prod(<<"feeb43bffb">>) of
       {ok, Product} ->
           Props = maps:get(<<"properties">>, maps:get(<<"thing">>, Product, #{}), []),
           io:format("产品配置加载成功，属性数量: ~p~n", [length(Props)]);
       Error ->
           io:format("产品配置加载失败: ~p~n", [Error])
   end.
   '
   ```

4. **检查缓存状态**
   ```bash
   _build/emqx/rel/emqx/bin/emqx eval '
   io:format("缓存统计:~n"),
   io:format("last_data缓存数量: ~p~n", [length(dgiot_data:match({last_data, '_'}))]),
   io:format("任务通道缓存数量: ~p~n", [length(dgiot_data:match({task_channel, '_'}))]).
   '
   ```

## 预期测试结果

### 成功指标
1. ✅ Modbus模块成功加载
2. ✅ 数据解析功能正常
3. ✅ 公式计算器工作正常
4. ✅ 缓存机制正常工作
5. ✅ 数据流各环节畅通

### 验证方法
1. **控制台输出**: 查看测试脚本输出，确认各步骤成功
2. **日志检查**: 查看系统日志，确认无错误信息
3. **功能验证**: 手动测试关键功能点
4. **数据验证**: 验证数据是否正确存储和查询

## 后续操作

### 测试完成后
1. **查看测试报告**: `docs/modbus_block_data_test_summary.md`
2. **分析测试结果**: 根据输出分析系统状态
3. **优化建议**: 参考总结文档中的优化建议
4. **问题修复**: 根据测试发现的问题进行修复

### 扩展测试
1. **性能测试**: 模拟高并发数据上报
2. **稳定性测试**: 长时间运行测试
3. **兼容性测试**: 测试不同Modbus设备
4. **集成测试**: 与前端集成测试

## 技术支持

### 文档参考
1. **架构设计**: `docs/modbus_block_data_test_summary.md`
2. **代码分析**: `scripts/analyze_modbus_flow.sh`输出
3. **问题排查**: 测试总结文档中的问题排查指南

### 联系支持
- **团队**: DG-IoT开发团队
- **文档**: 查看项目文档目录
- **日志**: 检查系统日志获取详细信息

---

**执行时间**: 2025-12-26  
**测试环境**: DG-IoT平台 v4.4.11  
**脚本位置**: `apps/dgiot_modbus/test/tools/integration/`  
**测试状态**: ✅ 准备就绪

> 注意：如果遇到任何问题，请参考集成测试目录中的README文档和测试脚本输出进行排查。
