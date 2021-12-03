# DGIOT轻量级工业物联网开源平台

[![GitHub Release](https://img.shields.io/github/release/dgiot/dgiot?color=brightgreen)](https://github.com/dgiot/dgiot/releases)
[![Docker Pulls](https://img.shields.io/docker/pulls/dgiot/dgiot)](https://hub.docker.com/r/dgiot/dgiot)
[![Community](https://img.shields.io/badge/Community-DGIOT-yellow)](https://tech.iotn2n.com)
[![QQ群346566935](https://img.shields.io/badge/QQ群-346566935-brightgreen)](https://jq.qq.com/?_wv=1027&k=LipWZvDe)

[English](./README.md) | [简体中文](./README-CN.md) | [日本語](./README-JP.md) | [русский](./README-RU.md)

# DG-IoT简介
DG-IoT是是国内首款轻量级开源工业物联网持续集成平台，我们致力于：

   + 让丰富工程人员可以通过视窗交互可以完成需求较简单的工业互联网项目

   + 让广大的初级前端工程师通过serverless的方式可以承接需求较复杂的工业互联网项目

   + 让Python、Java、Go、C初级后台工程师通过web编程开发通道来承接复杂的工业互联网项目
   + 前后端完全开源，极大节约用户成本，缩减物联网平台开发周期
   + 支持私有化一键式部署，快速拥有自己的物联网平台

## 核心特色
+ 前后端完全开源
+ 轻量级快速部署
+ 千万级长连接承载，电信级稳定性
+ 物模型-规则引擎-数据通道-组态页面全流程低代码开发
+ 多行业工业协议兼容



## 快速体验
|小程序|[电脑端](https://prod.iotn2n.com/)|账号|密码|
|---|---|---|---|
| ![dgiot小程序](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_release/dgiot_wechat.jpg) |![dgiot_dashboard.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_release/dgiot_dashboard.png) |dgiot_admin|dgiot_admin|

<!--  -->

# 构建

 构建 *dgiot* 需要 Erlang/OTP R21+, Windows下用 [msys64](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/msys64/msys64.zip)开发。下载后解压到D盘根目录，严格按照下图操作：
![效果图-1.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/msys64/%E6%95%88%E6%9E%9C%E5%9B%BE-1.png)

 +  国外下载源码
  ```bash
     git clone https://github.com/dgiot/dgiot_dashboard.git
     git clone https://github.com/dgiot/dgiot.git
   ```

 +  国内下载源码
   ```bash
     git clone https://gitee.com/dgiiot/dgiot_dashboard.git
     git clone https://gitee.com/dgiiot/dgiot.git
   ```

 +  国内Linux/Unix/Mac/windows 构建
  ```bash
    cd dgiot_dashboard
    git pull
    yarn dgiot:install
    yarn build
    cd ../dgiot
    git pull
    rm ./apps/dgiot_api/priv/www -rf
    cp ../dgiot_dashboard/dist/ ./apps/dgiot_api/priv/www -rf
    cp ../dgiot_dashboard/swagger ./apps/dgiot_api/priv/www/ -rf
    make run
 ```
+ Make Debug
 ```
  make DIAGNOSTIC=1
 ```
*DGIOT* 启动，可以使用浏览器访问 http://localhost:5080 来查看 Dashboard。

- 新功能的完整列表，请参阅 [DGIOT Release Notes](https://github.com/dgiot/dgiot/releases)。
- 获取更多信息，请访问 [DGIOT 官网](https://tech.iotn2n.com/)。

## 一键部署

 + **centos 7.6**

```
wget -q https://gitee.com/dgiiot/dgiot/raw/master/dgiot_install.sh  && sh dgiot_install.sh
```
# 社区
## 关于我们
| 联系方式       | 地址                                                         |
| -------------- | ------------------------------------------------------------ |
| github         | [https://github.com/dgiot](https://github.com/dgiot?from=git) |
| gitee          | [https://gitee.com/dgiot](https://gitee.com/dgiiot?from=git) |
| 官网           | [https://www.iotn2n.com](https://www.iotn2n.com?from=git)    |
| 博客           | [https://tech.iotn2n.com](https://tech.iotn2n.com?from=git)  |
| 物联网接入平台 | [https://dgiot.iotn2n.com](https://dgiot.iotn2n.com?from=git) |
| QQ群         | [346566935](https://jq.qq.com/?_wv=1027&k=LipWZvDe/) |
| 公众号         |  ![qrcode.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/wechat/qrcode.png)|

## 参与设计

如果对 DGIOT 有改进建议，可以向[EIP](https://github.com/dgiot/eip) 提交 PR 和 ISSUE

## 插件开发

如果想集成或开发你自己的插件，参考 [lib-extra/README.md](./lib-extra/README.md)

欢迎你将任何 bug、问题和功能请求提交到 [dgiot/dgiot](https://github.com/dgiot/dgiot/issues)。

## 开源许可
Apache License 2.0, 详见 [LICENSE](./LICENSE)。
