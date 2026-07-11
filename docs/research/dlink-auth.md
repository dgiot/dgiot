# dgiot_dlink MQTT 授权架构

> 源码: `dgiot_mqtt_acl.erl` (228 行), `dgiot_mock_mqtt.erl`

## 三层授权模型

```
Layer 1: 设备级 (Device Secret)
  ClientID = {ProductID}_{DeviceAddr}  (例: 2de1b3e1b8_DEV-001)
  Username = {ProductID}               (例: 2de1b3e1b8)
  Topic    = $dg/thing/{ProductID}/...
  验证:     device address 在 topic 路径中
  用途:     设备直连 EMQX, 上报遥测数据

Layer 2: 用户级 (User Token)
  ClientID = {SessionToken}{Type}      (例: r:abc123..._web)
  Topic    = $dg/user/{DeviceID}/... 或 $dg/device/{ProductID}/...
  验证:     check_device_acl(Token, DeviceId, UserId)
           → 查询用户角色是否有该设备的权限
  用途:     Web/App 用户订阅设备数据

Layer 3: 超级用户 (dgiot)
  Username = "dgiot" (127.0.0.1 免检)
  或 ClientID 匹配 SuperPwd 正则
  用途:     平台内部服务间通信
```

## ACL 决策流

```erlang
check_acl(ClientInfo, PubSub, Topic, _NoMatchAction, _Params) ->
    %% Topic 匹配 "$dg/" 前缀 → 进入 ACL 检查
    case Topic of
        <<"$dg/", _/binary>> -> do_check(...)
        _ -> ok  %% 其他 topic 直接放行
    end.
```

## 设备授权流程

```erlang
%% dgiot_mock_mqtt.erl — 设备连接模板

%% 方式1: ProductSecret (产品级密钥)
start(ChannelId, DeviceId, #{<<"auth">> := <<"ProductSecret">>}) ->
    ClientId = <<ProductID:10/binary, "_", DeviceAddr/binary>>,
    Username = ProductID,
    Password = dgiot_product:get_productSecret(ProductId);

%% 方式2: DeviceSecret (设备级密钥)
start(ChannelId, DeviceId, #{<<"auth">> := <<"DeviceSecret">>}) ->
    ClientId = <<ProductID:10/binary, "_", DeviceAddr/binary>>,
    Username = ProductID,
    Password = DeviceSecret;
```

## 用户授权流程

```erlang
%% dgiot_mqtt_acl.erl:209-227
check_device_acl(Token, DeviceID, UserId) ->
    %% 1. 从 Token 获取 Session → UserId + Roles
    Session = dgiot_auth:get_session(Token),
    #{<<"objectId">> := UserId, <<"roles">> := Roles} = Session,
    
    %% 2. 展开角色层级 (继承)
    RoleIds = maps:keys(Roles),
    ChildRoleIds = dgiot_role:childrole(RoleIds, []),
    AllRoleIds = RoleIds ++ ChildRoleIds,
    
    %% 3. 获取设备的角色白名单
    DeviceRoleIds = dgiot_device:get_roleids(DeviceID),
    
    %% 4. 交集判断
    case DeviceRoleIds -- AllRoleIds of
        DeviceRoleIds -> deny;  %% 完全无交集
        _ -> ok                 %% 有交集 → 允许
    end.
```

## 关键 Topic 模式

| Topic 模式 | 方向 | 授权方式 |
|-----------|------|---------|
| `$dg/thing/{PID}/{PID}_{DevAddr}/...` | publish | DeviceAddr 匹配 |
| `$dg/user/{DevID}/...` | subscribe | Token→Role ACL |
| `$dg/device/{PID}/{DevID}/...` | subscribe | Token→Role ACL |
| `$dg/user/channel/{ChID}/...` | subscribe | Token→Channel Parse check |
| `$dg/user/dashboard/{DashID}/...` | subscribe | Token→View Parse check |
| `$dg/user/alarm/{PID}/{DevID}/...` | publish | Token→Role ACL |
| `$dg/user/topo/{Token}/...` | subscribe | SessionToken 精确匹配 |
| `dgiot/...` (自定义) | pub/sub | **直接放行 (ok)** |

## 当前问题

我们的边缘代理 topic 是 `dgiot/oil_field_01/gw_131/{device}/{point}/data` —— 不走 `$dg/` 前缀，ACL 直接放行。这意味着：

1. ✅ 开发阶段：简单，不需要配置
2. ❌ 生产阶段：无任何授权验证，任何人都可以 pub/sub

## 建议

迁移到设备授权模式：

```python
# 边缘代理 (iotStudio) 连接 EMQX
client = mqtt.Client(
    client_id=f"{PRODUCT_ID}_{DEVADDR}",  # 2de1b3e1b8_DEV-001
    protocol=mqtt.MQTTv311
)
client.username_pw_set(
    username=PRODUCT_ID,       # 2de1b3e1b8
    password=DEVICE_SECRET     # 从 dgiot_product:get_productSecret 获取
)
# Topic 改为 $dg/thing/ 前缀
topic = f"$dg/thing/{PRODUCT_ID}/{PRODUCT_ID}_{DEVADDR}/{point}/data"
```
