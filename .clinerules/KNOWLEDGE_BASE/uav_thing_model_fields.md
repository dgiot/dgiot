# 超近距无人机物模型字段统计

**文档版本**: v1.0
**更新时间**: 2026-03-24 09:52
**知识库路径**: `/root/gitee/dgiot/.clinerules/KNOWLEDGE_BASE/uav_thing_model_fields.md`

---

## 1. 总体统计

### 1.1 字段总数

| 数据来源 | 字段数 | 占比 |
|---------|-------|-------|
| D1协议字段 | 45 | 20.7% |
| D2协议字段 | 26 | 12.0% |
| D3协议字段 | 17 | 7.8% |
| 舵面字段 | 10 | 4.6% |
| 测试项字段 | 17 | 7.8% |
| 噪音字段 | 3 | 1.4% |
| 版本信息字段 | 1 | 0.5% |
| 航点字段 | 5 | 2.3% |
| 舵面标定字段 | 4 | 1.8% |
| 电池字段 | 7 | 3.2% |
| 链路字段 | 16 | 7.4% |
| 其他字段 (时间戳等) | 66 | 30.4% |

**总计**: 217个字段

### 1.2 分页设置

- 每页显示: 10条
- 总页数: 22页
- 当前: 共217条

---

## 2. 详细字段统计

### 2.1 D1协议字段 (45个)

| 分类 | 字段数 | 字段列表 |
|------|-------|----------|
| 基础信息 | 5 | createdat, devaddr |
| 位置信息 | 3 | latitude, longitude, heading |
| 姿态信息 | 2 | pitch, roll |
| 高度信息 | 2 | relative_altitude, gps_altitude |
| 速度信息 | 3 | airspeed, east_velocity, north_velocity |
| 舵面角度 | 5 | elevator_angle, rudder_angle, aileron_angle, throttle_angle |
| 气压高度 | 1 | baro_altitude |
| 飞行时间 | 1 | flight_time |
| 电池加热 | 1 | battery_heating_flag |
| 复位类型 | 1 | reset_type |
| 复位次数 | 1 | reset_count |
| 起爆供电 | 1 | detonation_power_status |
| 目标航向 | 1 | target_heading |
| 目标俯仰 | 1 | target_pitch |
| 目标横滚 | 1 | target_roll |
| 目标油门 | 1 | target_throttle |
| 数据绑定执行 | 1 | data_binding_executed |
| 飞行模式 | 1 | flight_mode |
| 攻击模式 | 1 | attack_mode |
| 故障状态 | 1 | fault_status |
| 警告标志 | 1 | warning_flag |
| 铁电故障 | 1 | ferroelectric_fault |
| 其他 | 7 | (时间、日期等) |

### 2.2 D2协议字段 (26个)

| 分类 | 字段数 | 字段列表 |
|------|-------|----------|
| 姿态角 | 3 | pitch_angle, roll_angle, yaw_angle |
| 角速度 | 3 | angular_velocity_x, angular_velocity_y, angular_velocity_z |
| 加速度 | 3 | acceleration_x, acceleration_y, acceleration_z |
| 磁场 | 3 | magnetic_x, magnetic_y, magnetic_z |
| GPS信息 | 4 | gps_latitude, gps_longitude, gps_altitude, gps_satellites |
| 速度信息 | 2 | gps_speed, airspeed |
| 气压高度 | 1 | barometric_altitude |
| 地速 | 1 | ground_speed |
| 电池信息 | 4 | battery_voltage, battery_current, battery_remaining, battery_temp1, battery_temp2, battery_sequence, battery_cmd_result |
| 其他 | 2 | (辅助信息) |

### 2.3 D3协议字段 (17个)

| 分类 | 字段数 | 字段列表 |
|------|-------|----------|
| 飞行模式 | 4 | flight_mode, arm_state, gps_fix |
| 航点信息 | 4 | home_position_lat, home_position_lon, home_position_alt, waypoint_index, waypoint_total |
| 任务状态 | 2 | mission_status, rc_signal |
| 电机输出 | 4 | motor_output_1, motor_output_2, motor_output_3, motor_output_4 |
| 遥控信号 | 5 | remote_c1, remote_c2, remote_c3, remote_c4, remote_c5 |
| 其他 | 2 | (辅助信息) |

### 2.4 舵面字段 (10个)

| 分类 | 字段数 | 字段列表 |
|------|-------|----------|
| 舵面角度 | 5 | servo_1_angle, servo_2_angle, servo_3_angle, servo_4_angle, servo_5_angle |
| 舵面PWM | 5 | servo_1_pwm, servo_2_pwm, servo_3_pwm, servo_4_pwm, servo_5_pwm |

### 2.5 测试项字段 (17个)

| 分类 | 字段数 | 字段列表 |
|------|-------|----------|
| 测试项基础 | 8 | test_item_id, test_item_name, test_item_status, test_item_result, test_item_start_time, test_item_end_time, test_item_duration, test_item_message |
| 测试步骤 | 7 | step_index, step_name, step_status, step_result, step_start_time, step_end_time, step_message |
| 电阻测试 | 2 | battery_port_resistance, fuse7_8_resistance, fuse9_10_resistance |
| 电压测试 | 2 | fuse1_ground_voltage, fuse5_ground_voltage |
| 翼钉电阻 | 2 | fuse8_wing_nail_resistance, fuse7_wing_nail_resistance |

### 2.6 噪音字段 (3个)

| 分类 | 字段数 | 字段列表 |
|------|-------|----------|
| 噪音信息 | 3 | noise_level, noise_frequency, noise_decibel |

### 2.7 版本信息字段 (1个)

| 分类 | 字段数 | 字段列表 |
|------|-------|----------|
| 版本信息 | 1 | version_string |

### 2.8 航点字段 (5个)

| 分类 | 字段数 | 字段列表 |
|------|-------|----------|
| 航点位置 | 2 | waypoint_latitude, waypoint_longitude |
| 航点高度 | 1 | waypoint_altitude |
| 航点总数 | 1 | waypoint_total_count |
| 航点序号 | 1 | waypoint_index |

### 2.9 舵面标定字段 (4个)

| 分类 | 字段数 | 字段列表 |
|------|-------|----------|
| 舵面通道 | 1 | surface_channel |
| PWM中位 | 1 | surface_pwm_center |
| 上偏比例 | 1 | surface_up_ratio |
| 下偏比例 | 1 | surface_down_ratio |

### 2.10 电池字段 (7个)

| 分类 | 字段数 | 字段列表 |
|------|-------|----------|
| 电池状态 | 1 | battery_status_byte |
| 电池电压 | 1 | battery_voltage |
| 激活状态 | 1 | battery_activate_state |
| 温度信息 | 2 | battery_temp1, battery_temp2 |
| 序列编号 | 1 | battery_sequence |
| 执行结果 | 1 | battery_cmd_result |

### 2.11 链路字段 (16个)

| 分类 | 字段数 | 字段列表 |
|------|-------|----------|
| 误码率 | 2 | link_up_ber, link_down_ber |
| AGC信息 | 4 | link_air_agc, link_ground_agc1, link_ground_agc2, link_ground_agc3 |
| 频道信息 | 4 | link_work_channel, link_air_set_channel, link_ground_work_channel, link_ground_set_channel |
| 地址信息 | 3 | link_air_set_addr, link_ground_set_addr, link_ground_work_addr |
| 状态信息 | 3 | link_air_status, link_ground_status, link_ground_power |
| 其他 | 6 | link_range, link_air_temp, link_access_flag, link_node_address, link_granted_nodes, link_denied_nodes, link_online_nodes, link_latest_denied, link_crc1, link_crc2 |

---

## 3. 字段分组

### 3.1 分组定义

| 分组名称 | 说明 | 字段数 |
|---------|------|-------|
| D1 | D1协议数据 | 45 |
| D2 | D2协议数据 | 26 |
| D3 | D3协议数据 | 17 |
| SURFACE | 舵面数据 | 10 |
| TEST_ITEM | 测试项数据 | 17 |
| NOISE | 噪音数据 | 3 |
| AUXILIARY | 辅助数据（版本、航点、标定、电池） | 17 |
| LINK | 链路数据 | 16 |
| SYSTEM | 系统字段（时间戳等） | 66 |

### 3.2 分页显示

- 第1-5页: D1协议字段 (45个字段)
- 第6-8页: D2协议字段 (26个字段)
- 第9页: D3协议字段 (17个字段)
- 第10页: 舵面字段 (10个字段)
- 第11-12页: 测试项字段 (17个字段)
- 第12页: 噪音字段 (3个字段)
- 第13页: 版本信息、航点、舵面标定 (10个字段)
- 第14页: 电池字段 (7个字段)
- 第15-17页: 链路字段 (16个字段)
- 第18-22页: 系统字段 (66个字段)

---

## 4. 物模型生成

### 4.1 生成命令

```erlang
% 更新无人机产品物模型
auto_thing:update_uav_thing().

% 更新舵面传感器产品物模型
auto_thing:update_surface_device_thing().

% 更新噪音传感器产品物模型
auto_thing:update_noise_device_thing().
```

### 4.2 重建超级表

```erlang
% 重建无人机超级表
auto_thing:recreate_uav_super_table().

% 重建舵面传感器超级表
auto_thing:recreate_surface_super_table().

% 重建噪音传感器超级表
auto_thing:recreate_noise_super_table().
```

### 4.3 字段映射结构

```erlang
-record(field_map, {
    identifier :: binary(),  % 字段标识符
    name :: binary(),        % 字段名称
    type :: binary(),        % 字段类型 (enum, text, double, float, int)
    min :: number(),         % 最小值
    max :: number(),         % 最大值
    unit :: binary(),        % 单位
    step :: number(),        % 步长
    group :: binary()        % 分组
}).
```

---

## 5. 枚举类型

### 5.1 飞行模式枚举

| 值 | 说明 |
|----|------|
| 0 | 未知 |
| 16#12 | 高度保持 |
| 16#13 | 返航 |
| 16#14 | 盘旋 |
| 16#15 | 导航 |
| 16#16 | 起飞 |
| 16#17 | 降落 |
| 16#19 | 复飞 |
| 16#1B | 攻击 |
| 16#1C | 桶滚 |

### 5.2 攻击模式枚举

| 值 | 说明 |
|----|------|
| 0 | 未知 |
| 1 | 图像制导 |
| 2 | 位置制导 |

### 5.3 电池加热标志枚举

| 值 | 说明 |
|----|------|
| 0 | 正常 |
| 1 | 加热中 |

### 5.4 复位类型枚举

| 值 | 说明 |
|----|------|
| 0 | 上电复位 |
| 1 | 看门狗复位 |

---

## 6. 使用示例

### 6.1 查询物模型

```bash
# 查询无人机产品
curl -s http://127.0.0.1:8081/iotapi/classes/Product/6235befb62 | jq '.thing'

# 查询物模型字段数量
curl -s http://127.0.0.1:8081/iotapi/classes/Product/6235befb62 | jq '.thing.properties | length'
```

### 6.2 查询TDengine数据

```bash
# 查询最新数据
taos -s "SELECT * FROM _6235befb62 ORDER BY createdat DESC LIMIT 10;"

# 查询特定字段
taos -s "SELECT latitude, longitude, heading FROM _6235befb62 WHERE createdat > '2026-03-24 08:30:00' LIMIT 10;"
```

---

## 7. 相关文档

- `auto_thing.erl` - 物模型生成主模块
- `auto_thing_d1.erl` - D1协议字段映射
- `auto_thing_d2.erl` - D2协议字段映射
- `auto_thing_d3.erl` - D3协议字段映射
- `auto_thing_extra.erl` - 额外字段映射
- `auto_thing_noise.erl` - 噪音字段映射
- `auto_thing_surface.erl` - 舵面字段映射
- `UAV_THING_MODEL_STATS.md` - 无人机物模型统计
- `DATA_AGGREGATION_FLOW.md` - 数据汇聚流程

---

## 8. 总结

超近距无人机物模型包含**217个字段**，分为11个数据源和9个分组。这些字段覆盖了无人机的所有遥测数据、传感器数据、测试项数据、链路数据等。通过`auto_thing`模块可以自动生成和更新物模型，并支持重建TDengine超级表。

**核心特点**：
✅ 模块化设计 - 每个数据源独立模块
✅ 自动生成 - 通过字段映射自动生成物模型
✅ 分组管理 - 字段按功能分组
✅ 枚举支持 - 支持多种枚举类型
✅ 易于维护 - 添加字段只需修改对应模块

---

**文档位置**: `/root/gitee/dgiot/.clinerules/KNOWLEDGE_BASE/uav_thing_model_fields.md`
