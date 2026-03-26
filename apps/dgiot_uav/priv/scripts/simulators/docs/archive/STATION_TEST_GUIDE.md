# 工位测试管理系统使用指南

## 概述

这是一个完整的工位测试管理系统，支持：
- [OK] **环境清理** - 测试前后自动清理环境
- [OK] **单个工位测试** - 一个一个工位独立测试
- [OK] **MES服务器** - 端口80（通过nginx映射到801）
- [OK] **PLC服务器** - 端口502
- [OK] **设备模拟器** - TCP Client自动连接

## 架构

```
┌─────────────────────────────────────────────────────────┐
│                  工位测试架构                             │
└─────────────────────────────────────────────────────────┘

                    ┌──────────────┐
                    │  MES Server  │ ◄── 端口80 (nginx→801)
                    │  (HTTP:80)   │
                    └──────┬───────┘
                           │
┌──────────────┐    ┌──────▼───────┐    ┌──────────────┐
│ 设备 Client  │◄──►│  DG-IoT      │◄──►│ 扫码枪/其他  │
│ (TCP Client) │    │  Server      │    │ (TCP Client) │
│              │    │  (TCP:20000) │    │             │
└──────────────┘    └──────┬───────┘    └──────────────┘
                           │
                    ┌──────▼───────┐
                    │  PLC Server  │
                    │  (Modbus:502)│
                    └──────────────┘
```

## 快速开始

### 1. 磁航向工位测试（1700）

```bash
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators

# 清理环境
python3 station_test_manager.py clean 1700

# 启动测试（默认300秒）
python3 station_test_manager.py start 1700

# 启动测试（自定义时长600秒）
python3 station_test_manager.py start 1700 600

# 查看状态
python3 station_test_manager.py status 1700

# 停止测试
python3 station_test_manager.py stop 1700
```

### 2. 其他工位测试

```bash
# 总测1工位
python3 station_test_manager.py start 1500

# 总测2工位
python3 station_test_manager.py start 1600

# 拷机1工位
python3 station_test_manager.py start 1200

# 拷机2工位
python3 station_test_manager.py start 1300
```

## 完整测试流程

### 测试前（自动清理）

```
1. 停止所有测试进程
2. 清理ETS表
   - uav_ip_station_mapping
   - uav_station_status
   - dgiot_device_cache
3. 清理设备注册
4. 清理工位映射
5. 清理测试日志
```

### 测试中（自动启动）

```
1. 绑定测试IP（如192.168.100.21）
2. 启动MES服务器（端口80）
3. 启动PLC服务器（端口502）
4. 启动设备模拟器（TCP Client）
   - 地测口连接（端口10007）
   - 扫码枪连接（端口1234）
   - 发送EB90遥测数据
```

### 测试后（自动清理）

```
1. 停止设备模拟器
2. 停止MES服务器
3. 停止PLC服务器
4. 清理环境（同测试前）
```

## 命令详解

### clean - 清理环境

```bash
# 清理指定工位
python3 station_test_manager.py clean 1700

# 清理所有工位
python3 station_test_manager.py clean
```

### start - 启动测试

```bash
# 启动工位测试（默认300秒）
python3 station_test_manager.py start 1700

# 启动工位测试（自定义时长）
python3 station_test_manager.py start 1700 600
```

### stop - 停止测试

```bash
# 停止指定工位
python3 station_test_manager.py stop 1700

# 停止所有工位
python3 station_test_manager.py stop-all
```

### status - 查看状态

```bash
# 查看指定工位状态
python3 station_test_manager.py status 1700

# 查看所有工位状态
python3 station_test_manager.py status
```

## 工位配置

| 工位ID | 名称 | PLC IP | 设备IP | 端口 |
|--------|------|--------|--------|------|
| 1700 | 磁航向 | 192.168.100.20 | 192.168.100.21 (地测口)<br>192.168.100.23 (扫码枪) | 10007, 1234 |
| 1500 | 总测1 | 192.168.100.40 | 192.168.100.45 (治具) | 10006, 10001-10005 |
| 1600 | 总测2 | 192.168.100.40 | 192.168.100.46 (治具) | 10006, 10001-10005 |
| 1200 | 拷机1 | 192.168.100.40 | 192.168.100.47 (治具) | 10006 |
| 1300 | 拷机2 | 192.168.100.40 | 192.168.100.48 (治具) | 10006 |

## 测试日志

所有测试日志保存在 `/tmp/station_tests/` 目录：

```
/tmp/station_tests/
├── mes_1700.log              # MES服务器日志
├── plc_1700.log              # PLC服务器日志
├── device_1700.log           # 设备模拟器日志
├── mes_1700_data.jsonl       # MES接收的数据
└── station_1700_*.log        # 测试主日志
```

### 查看实时日志

```bash
# 实时查看设备日志
tail -f /tmp/station_tests/device_1700.log

# 实时查看MES日志
tail -f /tmp/station_tests/mes_1700.log

# 查看MES接收的数据
cat /tmp/station_tests/mes_1700_data.jsonl | jq .
```

## 验证命令

### 查看DG-IoT内部状态

```bash
# 查看IP-工位映射
_build/emqx/rel/emqx/bin/emqx eval 'ets:tab2list(uav_ip_station_mapping).'

# 查询特定IP的工位
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_business_service:get_station_by_ip(<<"192.168.100.21">>).'

# 查看EB90解析日志
tail -f _build/emqx/rel/emqx/log/console.log | grep "EB90"

# 查看设备注册日志
tail -f _build/emqx/rel/emqx/log/console.log | grep "注册"
```

### 检查端口监听

```bash
# 检查DG-IoT端口
netstat -tlnp | grep 20000

# 检查MES端口
netstat -tlnp | grep 80

# 检查PLC端口
netstat -tlnp | grep 502
```

## MES服务器配置

MES服务器监听在 **端口80**，通过nginx反向代理映射到801。

### Nginx配置示例

```nginx
# /etc/nginx/conf.d/mes.conf
server {
    listen 801;
    
    location / {
        proxy_pass http://127.0.0.1:80;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

### MES数据格式

MES接收POST请求，数据格式：

```json
{
  "timestamp": 1234567890.123,
  "data": "{\"deviceId\":\"UAV-001\",\"testResult\":\"passed\",...}"
}
```

## 故障排查

### 1. IP绑定失败

```bash
# 手动绑定IP
sudo ip addr add 192.168.100.21/24 dev eth0

# 验证绑定
ip addr show eth0 | grep "192.168.100.21"
```

### 2. MES服务器启动失败

```bash
# 检查端口占用
netstat -tlnp | grep :80

# 停止占用进程
sudo fuser -k 80/tcp
```

### 3. 设备连接失败

```bash
# 检查DG-IoT服务器状态
systemctl status emqx

# 检查端口监听
netstat -tlnp | grep 20000

# 查看连接日志
tail -f _build/emqx/rel/emqx/log/console.log | grep "TCP"
```

### 4. 清理环境失败

```bash
# 手动清理ETS表
_build/emqx/rel/emqx/bin/emqx eval 'ets:delete_all_objects(uav_ip_station_mapping).'

# 重启DG-IoT
_build/emqx/rel/emqx/bin/emqx restart
```

## 最佳实践

1. **测试前清理** - 每次测试前运行 `clean` 命令
2. **测试后清理** - 测试完成后运行 `stop` 命令会自动清理
3. **查看日志** - 实时查看日志文件了解测试进展
4. **验证结果** - 使用验证命令确认数据正确上报
5. **单个工位** - 一次只测试一个工位，避免冲突

## 完整示例

### 磁航向工位完整测试流程

```bash
# 1. 进入脚本目录
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators

# 2. 清理环境
python3 station_test_manager.py clean 1700

# 3. 启动测试（600秒）
python3 station_test_manager.py start 1700 600

# 4. 查看状态
python3 station_test_manager.py status 1700

# 5. 实时查看日志
tail -f /tmp/station_tests/device_1700.log

# 6. 验证数据上报
_build/emqx/rel/emqx/bin/emqx eval 'ets:tab2list(uav_ip_station_mapping).'

# 7. 查看MES接收数据
cat /tmp/station_tests/mes_1700_data.jsonl | jq .

# 8. 停止测试（会自动清理环境）
python3 station_test_manager.py stop 1700
```

## 总结

- [OK] **环境清理自动化** - 测试前后自动清理
- [OK] **单个工位测试** - 避免多工位冲突
- [OK] **MES端口80** - 通过nginx映射
- [OK] **完整闭环测试** - 设备注册→数据上报→MES接收
- [OK] **日志完整记录** - 方便问题排查
