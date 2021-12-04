# DGIOT轻量级工业物联网开源平台

[![GitHub Release](https://img.shields.io/github/release/dgiot/dgiot?color=brightgreen)](https://github.com/dgiot/dgiot/releases)
[![Docker Pulls](https://img.shields.io/docker/pulls/dgiot/dgiot)](https://hub.docker.com/r/dgiot/dgiot)
[![Community](https://img.shields.io/badge/Community-DGIOT-yellow)](https://tech.iotn2n.com)
[![QQ群346566935](https://img.shields.io/badge/QQ群-346566935-brightgreen)](https://jq.qq.com/?_wv=1027&k=LipWZvDe)
[English](./README.md) | [中文](./README-CN.md) | [日本語](./README-JP.md) | [русский](./README-RU.md)|
[github](https://github.com/dgiot?from=git)|[gitee](https://www.iotn2n.com?from=git)|[官网](https://www.iotn2n.com?from=git)|[博客](https://tech.iotn2n.com?from=git)|[体验](https://prod.iotn2n.com?from=git)|
## DG-IoT简介
DG-IoT是国内首款轻量级开源工业物联网平台，我们致力于提供四类物联网解决方案：
+ 国企研究院：平台代码开源，无版权产权困扰，国产无“卡脖”之忧。
+ 系统集成商：通用设备海量接入、定制设备二次开发、30分钟一键式私有化快速部署，低成本（降90%成本）
+ 工业设备制造商：海量设备上线运维，不受公有云限制，低成本，短周期自建平台，私有化部署，数据安全
+ 开源平台开发者：一键式开发环境，集成和兼容各种最优开源工具，快速承接物联网项目
## 业务架构
![](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/msys64/dgiot_tech_arch.jpg)
## 核心特色
+ 前后端[完全开源](https://github.com/dgiot/dgiot/blob/master/Platform-service.md) ，无收费商业版
+ 支持私有化一键式部署，快速拥有自己的轻量级物联网平台
+ 千万级长连接承载，电信级稳定性
+ 物模型-规则引擎-数据通道-组态页面全流程可视化低代码开发
+ 多行业工业协议兼容
## 快速体验
|微信群|公众号|[电脑端](https://prod.iotn2n.com/)|小程序|账号|密码|
|---|---|---|---|---|---|
|![](https://user-images.githubusercontent.com/51999461/144572983-16bf3223-a00b-4cd6-9446-cb652f81c8af.png)|![](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_release/dgiot_wechat.jpg)|![](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/wechat/qrcode.png)|![](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_release/dgiot_dashboard.png) |dgiot_admin|dgiot_admin|
## 一键部署
+ **centos 7.6/7.9** 推荐使用腾讯云和阿里云
```
wget -q https://gitee.com/dgiiot/dgiot/raw/master/dgiot_install.sh  && sh dgiot_install.sh
```
## 构建
 构建 *dgiot* 需要 Erlang/OTP R23+, Windows下用 [msys64](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/msys64/msys64.zip)开发。下载后解压到D盘根目录，严格按照下图操作：
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

## 参与设计

如果对 DGIOT 有改进建议，可以向[EIP](https://github.com/dgiot/eip) 提交 PR 和 ISSUE

## 插件开发

如果想集成或开发你自己的插件，参考 [lib-extra/README.md](./lib-extra/README.md)

欢迎你将任何 bug、问题和功能请求提交到 [dgiot/dgiot](https://github.com/dgiot/dgiot/issues)。

## 开源许可
Apache License 2.0, 详见 [LICENSE](./LICENSE)。
