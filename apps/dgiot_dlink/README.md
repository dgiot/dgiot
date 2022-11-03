# Dlink协议
Dlink协议是针对物联网开发领域设计的一种数据交换规范，数据格式是JSON，用于设备端和物联网平台的双向通信，更便捷地实现和规范了设备端和物联网平台之间的业务数据交互。

## 协议框架
![dlink proctol.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/backend/dgiot/dlink%20proctol.png)
+ 设备层和用户层隔离，物理层和逻辑层隔离
+ 用户层mqtt权限与openApi权限保持一致性
+ 设备侧topic交互采用 {productId}/{deviceAddr}的组合来唯一标识设备, deviceAddr为设备物理地址
+ 用户侧topic交互采用{deviceId}来唯一标识设备，用{userId}来唯一标识用户,deviceId为设备虚拟地址


### 鉴权设计
+ %u 表示用Username做ACL规则
+ %c 表示用clientId做ACL规则
- 用户侧clientId用Token做ACL规则, Token是dgiot用户登录权限系统的token,与API权限一致
- 设备侧clientId为{productId}_{deviceAddr},或者 deviceAddr, 必须保证clientId的唯一性

| 客户端   | Username  |  Password |  ClientId  | 登录鉴权|  订阅ACL  | 发布ACL|
| --------  | -------- | ------- | -------- |-------- | ------- | -------- |
| Device |{productId}|{productSecret}|clientId| 一型一密 | $dg/device/%u/# | $dg/thing/%u/# |
| Device |{productId}|{deviceSecret}|clientId| 一机一密 | $dg/device/%u/%d/# | $dg/thing/%u/%c/# |
| Device |{productId}|{productSecret}|clientId| 证书加密 | $dg/device/%u/# | $dg/thing/%u/# |
| User |{userId}|{Token}|{Token}| Token认证 | $dg/user/%c/# | $dg/thing/%c/# |

## topic设计
| 分类   | Topic  |  发布者 |  订阅者  |
| --------  | -------- | ------- | -------- |
| 属性上报 |$dg/thing/{productId}/{deviceAddr}/properties/report|设备|平台|
| 告警上报 |$dg/user/alarm/{productId}/{deviceId}/properties/report|平台|用户|
| 组态消息 |$dg/user/konva/{DeviceId}/report|平台|用户|
| 卡片消息 | $dg/user/realtimecard/{DeviceId}/report|平台|用户|
| 设备状态消息 | $dg/user/devicestate/{DeviceId}/report |平台|用户|
| 子属性上报 |$dg/thing/{productId}/{deviceAddr}/gateway/sub_devices/properties/report|设备|平台|
| 子属性上报 |$dg/user/gateway/{deviceId}/sub_devices/properties/report|平台|用户|
| 属性设置 | $dg/thing/{deviceId}/properties/set/request_id={request_id}|用户|平台|
| 属性设置 | $dg/device/{productId}/{deviceAddr}/properties/set/request_id={request_id}|平台|设备|
| 属性设置 |$dg/thing/{productId}/{deviceAddr}/properties/set/request_id={request_id}|设备|平台|
| 属性设置 |$dg/user/{deviceId}/properties/set/request_id={request_id}|平台|用户|
| 属性获取 |$dg/thing/{deviceId}/properties/get/request_id={request_id}|用户|平台|
| 属性获取 |$dg/device/{productId}/{deviceAddr}/properties/get/response/request_id={request_id}|平台|设备|
| 属性获取 |$dg/thing/{productId}/{deviceAddr}/properties/get/response/request_id={request_id}|设备|平台|
| 属性获取 |$dg/user/{deviceId}/properties/get/request_id={request_id}|平台|用户|
| 属性获取 |$dg/thing/{productId}/{deviceAddr}/shadow/get/request_id={request_id}|设备|平台|
| 属性获取 |$dg/device/{productId}/{deviceAddr}/shadow/get/request_id={request_id}|平台|设备|
| 属性采集 |$dg/device/{productId}/{deviceAddr}/properties|平台|设备|
| 配置下发 |$dg/device/{productId}/{deviceAddr}/profile|平台|设备|
| 事件上报 |$dg/thing/{productId}/{deviceAddr}/events|设备|平台|
| 事件上报 |$dg/user/{deviceId}/events|平台|用户|
| 通道消息 |$dg/user/channel/{channelId}/{productId}/{deviceId}|平台|用户|
| 通道设置 |$dg/user/channel/{channelId}/commands/request_id={request_id}|用户|平台|
| 通道设置 |$dg/user/channel/{channelId}/commands/response/request_id={request_id}|平台|用户|
| 大屏消息 |$dg/user/dashboard/{dashboardId}/{productId}/{deviceId}|平台|用户|
| 组态大屏 |$dg/user/topo/{SessionToken}/{NodeType}/{NodeId}/report|平台|app|
| 云云对接 |$dg/bridge/{bridgetopic}|平台|平台|
| app对接 |$dg/user/uniapp/{SessionToken}/report|平台|app|
| app对接 |$dg/thing/uniapp/{SessionToken}/report|app|平台|
| 设备初始化请求 |$dg/thing/{productId}/{deviceAddr}/init/request|设备|平台|
| 设备初始化响应 |$dg/device/{productId}/{deviceAddr}/init/response|平台|设备|

## payload设计
### 属性上报 ($dg/thing/{productId}/{deviceAddr}/properties/report)
+ dlink json格式
```json
{
    "Power":  "on",
    "WF":  23.6
}
```
+ 透传/自定义
```
  00002233441232013fa00000
```
### 事件上报 $dg/thing/{productId}/{deviceAddr}/events
```json
{
    "Power":  "on",
    "WF":  23.6
}
```

## 通道接入
### http server
### http client
### mqtt client
### mqtt bridge
### tpc server
### tcp client
### udp server
### udp client

+ 支持grpc多语言编解码

```
Dlink RunTime                             Third-party Decode/Encode
+========================+                 +========+==========+
|    Dlink               |                 |        |          |
|   +----------------+   |      gRPC       | gRPC   |  User's  |
|   |   gPRC Client  | ------------------> | Server |  Codes   |
|   +----------------+   |    (HTTP/2)     |        |          |
|                        |                 |        |          |
+========================+                 +========+==========+
```
### gRPC

For instructions on how to use the language-specific gRPC runtime for a project,
please refer to these documents

- [C++](https://github.com/fastdgiot/grpc/tree/master/src/cpp): follow the instructions under the `src/cpp` directory
- [C#](https://github.com/fastdgiot/grpc/tree/master/src/csharp): NuGet package `Grpc`
- [Dart](https://github.com/grpc/grpc-dart): pub package `grpc`
- [Go](https://github.com/grpc/grpc-go): `go get google.golang.org/grpc`
- [Java](https://github.com/grpc/grpc-java): Use JARs from Maven Central
  Repository
- [Kotlin](https://github.com/grpc/grpc-kotlin): Use JARs from Maven Central
  Repository
- [Node](https://github.com/grpc/grpc-node): `npm install grpc`
- [Objective-C](https://github.com/fastdgiot/grpc/tree/master/src/objective-c): Add `gRPC-ProtoRPC` dependency to podspec
- [PHP](https://github.com/fastdgiot/grpc/tree/master/src/php): `pecl install grpc`
- [Python](https://github.com/fastdgiot/grpc/tree/master/src/python/grpcio): `pip install grpcio`
- [Ruby](https://github.com/fastdgiot/grpc/tree/master/src/ruby): `gem install grpc`
- [WebJS](https://github.com/grpc/grpc-web): follow the grpc-web instructions
