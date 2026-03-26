# MES代理配置说明

## 配置完成情况

### 1. Nginx代理配置 ✅

已添加到 `/data/dgiot/nginx/conf/nginx.conf`:

```nginx
# 80端口server块中添加
location /lezao/jymes/api/equip/proExec {
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_buffer_size  128k;
    proxy_buffers   32 32k;
    proxy_busy_buffers_size 128k;
    proxy_pass http://127.0.0.1:801/lezao/jymes/api/equip/proExec;
}

location /mes/health {
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_buffer_size  128k;
    proxy_buffers   32 32k;
    proxy_busy_buffers_size 128k;
    proxy_pass http://127.0.0.1:801/health;
}
```

### 2. MES模拟器配置 ✅

- 监听地址: `127.0.0.1:801`（只监听本地）
- 通过nginx代理访问
- 支持Erlang代码的API路径: `/lezao/jymes/api/equip/proExec`

### 3. 访问方式

#### 方式1：通过Nginx代理（推荐）

```bash
# 健康检查
curl http://<nginx-ip>/mes/health

# 上报测试数据
curl -X POST http://<nginx-ip>/lezao/jymes/api/equip/proExec \
  -H "Content-Type: application/json" \
  -d '{"device_id":"TEST001","result":"passed"}'
```

#### 方式2：本地直接访问

```bash
# 健康检查
curl http://127.0.0.1:801/health
```

## Erlang后端配置

### 当前配置

Erlang代码中使用的MES URL（`dgiot_uav_mes_api.erl:53`）:
```erlang
api_url => application:get_env(dgiot_uav, mes_api_url,
    "http://172.1.2.222/lezao/jymes/api/equip/proExec"),
```

### 配置修改建议

#### 方案1：修改为本地nginx地址

如果Erlang后端与nginx在同一台机器：

```erlang
% 修改默认值为
"http://127.0.0.1:80/lezao/jymes/api/equip/proExec"
```

#### 方案2：通过环境变量配置

```bash
# 设置环境变量
export MES_API_URL="http://127.0.0.1:80/lezao/jymes/api/equip/proExec"

# 重启emqx
_build/emqx/rel/emqx/bin/emqx restart
```

#### 方案3：通过应用配置文件

在 `emqx.conf` 或插件配置中添加：

```erlang
{dgiot_uav, [
    {mes_api_url, "http://127.0.0.1:80/lezao/jymes/api/equip/proExec"}
]}.
```

## 测试验证

### 1. 启动MES模拟器

```bash
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators
bash start_mes.sh
```

### 2. 测试Nginx代理

```bash
# 测试健康检查
curl http://127.0.0.1:80/mes/health

# 应该返回：
{
    "success": true,
    "service": "MES模拟服务器",
    "status": "running",
    "timestamp": "..."
}
```

### 3. 测试MES API

```bash
# 上报测试数据
curl -X POST http://127.0.0.1:80/lezao/jymes/api/equip/proExec \
  -H "Content-Type: application/json" \
  -d '{
    "device_id": "TEST001",
    "station_id": "1100",
    "result": "passed"
  }'
```

### 4. 查看MES日志

```bash
# MES模拟器日志会显示：
# [MES_API] 接收到设备执行上报（Erlang MES API）
# [MES_DB] 添加测试结果: 设备=TEST001, 结果=passed
```

## 网络架构

```
┌─────────────┐
│ Erlang后端  │
│ (emqx)      │
└──────┬──────┘
       │ http://172.1.2.222:80
       │ (或修改为 127.0.0.1:80)
       ↓
┌─────────────┐
│ Nginx       │
│ (80端口)    │
└──────┬──────┘
       │ proxy_pass
       │ 127.0.0.1:801
       ↓
┌─────────────┐
│ MES模拟器   │
│ (801端口)   │
└─────────────┘
```

## 优势

1. ✅ **统一入口** - 通过nginx统一管理API访问
2. ✅ **安全隔离** - MES只监听本地，不直接暴露
3. ✅ **灵活配置** - 可通过nginx配置路由、负载均衡
4. ✅ **易于维护** - 无需修改Erlang代码（通过配置即可）

## 注意事项

1. 确保nginx正在运行
2. 确保MES模拟器正在运行
3. Erlang后端的MES URL需要指向nginx地址
4. 如果Erlang后端在不同机器，需要将172.1.2.222替换为nginx的实际IP

---

**配置完成时间**: 2026-03-25 08:28
**版本**: 1.0
