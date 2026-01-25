# 技能架构优化方案

## 问题分析

当前技能系统存在以下问题：
1. **概念技能包太多**：dgiot_core_concepts等技能包含过多概念，导致技能过于臃肿
2. **技能粒度不合理**：部分技能过于宽泛，部分技能过于具体
3. **技能联动不足**：技能之间缺乏有效的联动机制
4. **Hook集成不够深入**：技能与Hook系统联动不够紧密

## 优化目标

1. **合理拆分技能**：将大型技能包拆分为更细粒度的技能
2. **建立技能层级**：形成核心技能、扩展技能、工具技能的多层结构
3. **强化技能联动**：通过Hook系统实现技能间的自动激活和协同工作
4. **优化技能管理**：提供技能依赖管理、版本控制和生命周期管理

## 技能拆分方案

### 1. DGIOT核心概念技能拆分

**当前状态**：`dgiot_core_concepts` 包含产品、设备、通道、用户、视图、菜单、部门、会话等所有核心概念

**拆分方案**：
```
dgiot_core_concepts/ (保留为索引技能)
├── dgiot_product_management/ (产品管理)
├── dgiot_device_management/ (设备管理)
├── dgiot_channel_management/ (通道管理)
├── dgiot_user_management/ (用户管理)
├── dgiot_view_management/ (视图管理)
├── dgiot_menu_management/ (菜单管理)
├── dgiot_department_management/ (部门管理)
└── dgiot_session_management/ (会话管理)
```

### 2. DGIOT数据存储技能拆分

**当前状态**：`dgiot_data_storage` 包含所有存储相关概念

**拆分方案**：
```
dgiot_data_storage/ (保留为索引技能)
├── dgiot_memory_storage/ (内存存储：ETS/DETS/Mnesia)
├── dgiot_business_storage/ (业务存储：Parse Server)
├── dgiot_timeseries_storage/ (时序存储：TDengine)
└── dgiot_file_storage/ (文件存储)
```

### 3. 开发工作流技能优化

**当前状态**：`development_workflow_cycle` 和 `continuous_iteration_cycle` 功能重叠

**优化方案**：
```
development_workflow/ (开发工作流主技能)
├── coding_workflow/ (编码工作流)
├── compile_workflow/ (编译工作流)
├── debug_workflow/ (调试工作流)
└── test_workflow/ (测试工作流)
```

## 技能层级架构

### 第一层：核心技能 (Core Skills)
- `erlang_chinese_utf8` - Erlang中文UTF8支持
- `erlang_include_system` - Erlang包含系统
- `erlang_logging_solution` - Erlang日志解决方案
- `dgiot_compile_debug` - DGIOT编译调试

### 第二层：领域技能 (Domain Skills)
- `dgiot_product_management` - 产品管理
- `dgiot_device_management` - 设备管理
- `dgiot_channel_management` - 通道管理
- `dgiot_user_management` - 用户管理
- `dgiot_data_api_auth_system` - 数据API和权限系统

### 第三层：工具技能 (Tool Skills)
- `uav_protocol_analyzer` - 无人机协议分析
- `tdengine_timeseries_storage` - TDengine时序存储
- `drone_test_web_interface` - 无人机测试界面

### 第四层：管理技能 (Management Skills)
- `skill_manager` - 技能管理
- `hook_manager` - Hook管理
- `main_objective_tracker` - 主目标跟踪

## 技能-Hook联动机制

### 1. 自动技能检测
- **TaskStart Hook**：在任务开始时检测相关技能
- **UserPromptSubmit Hook**：在用户提交消息时检测技能需求
- **PreToolUse Hook**：在工具使用前检查是否需要特定技能

### 2. 技能依赖管理
```bash
# 技能依赖配置文件
.skills/dependencies.json
{
  "dgiot_channel_management": {
    "requires": ["erlang_logging_solution", "dgiot_compile_debug"],
    "recommends": ["dgiot_product_management", "dgiot_device_management"]
  }
}
```

### 3. 技能上下文传递
```bash
# 技能间上下文共享
.skills/context_bridge.sh
# 允许技能A的输出作为技能B的输入
```

## 实施计划

### 第一阶段：技能拆分 (1-2天)
1. 创建新的技能目录结构
2. 拆分`dgiot_core_concepts`为多个独立技能
3. 拆分`dgiot_data_storage`为多个独立技能
4. 更新技能索引文件

### 第二阶段：Hook集成优化 (1天)
1. 更新TaskStart Hook支持新技能结构
2. 优化SkillActivation Hook的检测逻辑
3. 创建技能依赖管理Hook

### 第三阶段：测试验证 (1天)
1. 测试技能拆分后的功能完整性
2. 验证技能-Hook联动机制
3. 性能测试和优化

## 预期收益

1. **提高技能复用性**：细粒度技能更容易在不同场景下复用
2. **降低学习成本**：用户只需学习相关领域的技能，无需掌握整个大技能包
3. **提升响应速度**：小技能加载更快，响应更及时
4. **增强系统灵活性**：可以根据需要组合不同的技能
5. **改善维护性**：单个技能的维护和更新不影响其他技能

## 风险控制

1. **向后兼容性**：保留原有技能作为索引，确保现有代码不受影响
2. **渐进式迁移**：逐步拆分，边拆分边测试，确保系统稳定
3. **用户培训**：提供迁移指南和示例，帮助用户适应新结构
4. **监控机制**：监控技能使用情况，及时发现问题并调整

## 下一步行动

1. 创建技能拆分工具脚本
2. 制定详细的拆分实施计划
3. 与用户沟通确认拆分方案
4. 开始第一阶段实施
