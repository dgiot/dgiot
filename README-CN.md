# DGIOT

[![GitHub Release](https://img.shields.io/github/release/dgiot/dgiot?color=brightgreen)](https://github.com/dgiot/dgiot/releases)
[![Docker Pulls](https://img.shields.io/docker/pulls/dgiot/dgiot)](https://hub.docker.com/r/dgiot/dgiot)
[![Community](https://img.shields.io/badge/Community-DGIOT-yellow)](https://tech.iotn2n.com)

[English](./README.md) | [简体中文](./README-CN.md) | [日本語](./README-JP.md) | [русский](./README-RU.md)

## 快速体验
|              |                      |    |    |
| -------------- | ------------------ |-----|---|
| 账号     | dgiot_admin                            |密码 |dgiot_admin|
| 小程序  | ![dgiot小程序](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_release/dgiot_wechat.jpg)|管理后台 | ![dgiot_dashboard.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_release/dgiot_dashboard.png)|



*DGIOT*  是国内首款轻量级开源工业物联网持续集成平台

DG-IoT团队2016年之前，在互联网和移动互联网爬坑多年，2016年开始进入物联网爬坑，希望通过这个开源平台把多年爬坑经验共享出来，让多学科交叉的工业互联网项目变得更简单。
   + 让丰富工程人员可以通过视窗交互可以完成需求较简单的工业互联网项目
   + 让广大的初级前端工程师通过serverless的方式可以承接需求较复杂的工业互联网项目
   + 让Python、Java、Go、C初级后台工程师通过web编程开发通道来承接复杂的工业互联网项目

# 愿景
  DG-IoT团队希望通过DG-IoT工业互联网持续集成平台达成下面一些愿景：
  + 通过工程人员、前端工程师、初级后台工程师在不超过1个月的实际完成中小型的工业互联网项目
  + 通过代码开源、软件免费、文档共享、技术认证、产品认证、运维托管等多种方式保证高质量的交付
  + 技术领域专家不断持续集成业界优秀技术框架、业务领域专家不断持续优化业务模型和流程、构建多学科交叉的开放平台
  + 物联网平台最终能够实现简洁易用，回归到工具化的本质

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

## 社区

### FAQ

访问 [DGIOT FAQ](https://tech.iotn2n.com/zh/backend/) 以获取常见问题的帮助。

### 问答

[GitHub Discussions](https://github.com/dgiot/dgiot_server/discussions)
[DGIOT 中文问答社区](https://tech.iotn2n.com/)

### 参与设计

如果对 DGIOT 有改进建议，可以向[EIP](https://github.com/dgiot/eip) 提交 PR 和 ISSUE

### 插件开发

如果想集成或开发你自己的插件，参考 [lib-extra/README.md](./lib-extra/README.md)

欢迎你将任何 bug、问题和功能请求提交到 [dgiot/dgiot](https://github.com/dgiot/dgiot/issues)。

### 关于我们
| 联系方式       | 地址                                                                                      |
| -------------- | ----------------------------------------------------------------------------------------- |
| github         | [https://github.com/dgiot](https://github.com/dgiot?from=git)                             |
| gitee          | [https://gitee.com/dgiot](https://gitee.com/dgiiot?from=git)                              |
| 官网           | [https://www.iotn2n.com](https://www.iotn2n.com?from=git)                                 |
| 博客           | [https://tech.iotn2n.com](https://tech.iotn2n.com?from=git)                               |
| 物联网接入平台 | [https://dgiot.iotn2n.com](https://dgiot.iotn2n.com?from=git)                             |
| 公众号         | ![qrcode.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/wechat/qrcode.png) |

### 联系我们
你可通过以下途径与 DGIOT 社区及开发者联系:
- [official](https://www.iotn2n.com)
- [ask](https://ask.iotn2n.com/)
- [Blog](https://tech.iotn2n.com)
- [Twitter](https://twitter.com/)
- [Facebook](https://www.facebook.com/)
- [Reddit](https://www.reddit.com/)
- [Weibo](https://weibo.com)

## 开源许可
Apache License 2.0, 详见 [LICENSE](./LICENSE)。
