# Dlink协议
   Dlink协议是针对物联网开发领域设计的一种数据交换规范，数据格式是JSON，用于设备端和物联网平台的双向通信，更便捷地实现和规范了设备端和物联网平台之间的业务数据交互。

## 协议框架
![dlink proctol.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/backend/dgiot/dlink%20proctol.png)
+ 设备层和用户层隔离，物理层和逻辑层隔离
+ 用户层mqtt权限与openApi权限保持一致性
+ 设备侧topic交互采用 {productId}/{deviceAddr}的组合来唯一标识设备, deviceAddr为设备物理地址
+ 用户侧topic交互采用{deviceId}来唯一标识设备，用{userId}来唯一标识用户,deviceId为设备虚拟地址


### 鉴权设计
+ deviceId=md5("Device" + {productId} + {devAddr}).subString(10)
+ %u 表示用Username做ACL规则
+ %c 表示用clientId做ACL规则
 - 用户侧clientId用Token做ACL规则, Token是dgiot用户登录权限系统的token,与API权限一致
 - 设备侧clientId可用deviceAddr或者deviceId,如果用deviceAddr需要用户自己保证唯一性

| 客户端   | Username  |  Password |  ClientId  | 登录鉴权|  订阅ACL  | 发布ACL|
| --------  | -------- | ------- | -------- |-------- | ------- | -------- |
| Device |{productId}|{productSecret}|{clientId}| 一型一密 | $dg/device/%u/# | $dg/thing/%u/# |
| Device |{productId}|{deviceSecret}|{clientId}| 一机一密 | $dg/device/%u/%d/# | $dg/thing/%u/%c/# |
| Device |{productId}|{productSecret}|{clientId}| 证书加密 | $dg/device/%u/# | $dg/thing/%u/# |
| User |{userId}|{Token}|{Token}| Token认证 | $dg/user/%c/# | $dg/thing/%c/# |

## topic设计
| 分类   | Topic  |  发布者 |  订阅者  |
| --------  | -------- | ------- | -------- |
| 双向消息 |$dg/device/{productId}/{deviceAddr}/messages| 平台 | 设备 |
| 双向消息 |$dg/thing/{productId}/{deviceAddr}/messages| 设备 | 平台 |
| 双向消息 |$dg/thing/{deviceId}/messages|用户|平台|
| 双向消息 |$dg/user/{deviceId}/messages|平台|用户|
| 双向命令 |$dg/device/{productId}/{deviceAddr}/commands/request_id={request_id}| 平台 | 设备 |
| 双向命令 |$dg/thing/{productId}/{deviceAddr}/commands/request_id={request_id}| 设备 | 平台 |
| 双向命令 |$dg/thing/{deviceId}/commands/request_id={request_id}|用户|平台|
| 双向命令 |$dg/user/{deviceId}/commands/request_id={request_id}| 平台 | 用户 |
| 属性上报 |$dg/thing/{productId}/{deviceAddr}/properties/report|设备|平台|
| 属性上报 |$dg/user/{deviceId}/properties/report|平台|用户|
| 子属性上报 |$dg/thing/{productId}/{deviceAddr}/gateway/sub_devices/properties/report|设备|平台|
| 子属性上报 |$dg/user/{deviceId}/gateway/sub_devices/properties/report|平台|用户|
| 属性设置 |$dg/thing/{deviceId}/properties/set/request_id={request_id}|用户|平台|
| 属性设置 |$dg/device/{productId}/{deviceAddr}/properties/set/request_id={request_id}|平台|设备|
| 属性设置 |$dg/thing/{productId}/{deviceAddr}/properties/set/request_id={request_id}|设备|平台|
| 属性设置 |$dg/user/{deviceId}/properties/set/request_id={request_id}|平台|用户|
| 属性获取 |$dg/thing/{deviceId}/properties/get/request_id={request_id}|用户|平台|
| 属性获取 |$dg/device/{productId}/{deviceAddr}/properties/get/response/request_id={request_id}|平台|设备|
| 属性获取 |$dg/thing/{productId}/{deviceAddr}/properties/get/response/request_id={request_id}|设备|平台|
| 属性获取|$dg/{deviceId}/user/properties/get/request_id={request_id}|平台|用户|
| 属性获取 |$dg/thing/{productId}/{deviceAddr}/shadow/get/request_id={request_id}|设备|平台|
| 属性获取 |$dg/device/{productId}/{deviceAddr}/shadow/get/request_id={request_id}|平台|设备|
| 事件上报 |$dg/thing/{productId}/{deviceAddr}/events|设备|平台|
| 事件上报 |$dg/user/{deviceId}/events|平台|用户|

## payload设计

+ 支持grpc多语言编解码

```
Dlink RunTime                             Third-party Decode/Encode
+========================+                 +========+==========+
|    Dlink              |                 |        |          |
|   +----------------+   |      gRPC       | gRPC   |  User's  |
|   |   gPRC Client  | ------------------> | Server |  Codes   |
|   +----------------+   |    (HTTP/2)     |        |          |
|                        |                 |        |          |
+========================+                 +========+==========+
```
