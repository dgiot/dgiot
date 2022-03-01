#  DGIOT Lightweight industrial IoT open source platform

[![GitHub Release](https://img.shields.io/github/release/dgiot/dgiot?color=brightgreen)](https://github.com/dgiot/dgiot/releases)
[![Build Status](https://travis-ci.org/dgiot/dgiot.svg)](https://travis-ci.org/dgiot/dgiot)
[![Docker Pulls](https://img.shields.io/docker/pulls/dgiot/dgiot)](https://hub.docker.com/r/dgiot/dgiot)
[![Community](https://img.shields.io/badge/Community-DGIOT-yellow)](https://tech.iotn2n.com)
[![QQ群346566935](https://img.shields.io/badge/QQ群-346566935-brightgreen)](https://jq.qq.com/?_wv=1027&k=LipWZvDe)
[![star](https://gitee.com/dgiiot/dgiot/badge/star.svg?theme=gvp)](https://gitee.com/dgiiot/dgiot/stargazers)


[English](./README.md) | [简体中文](./README-CN.md) | [日本語](./README-JP.md) | [русский](./README-RU.md)


[github](https://github.com/dgiot?from=git)|[gitee](https://www.iotn2n.com?from=git)|[Official website](https://www.iotn2n.com?from=git)|[Blog](https://tech.iotn2n.com?from=git)|[Experience](https://prod.iotn2n.com?from=git)|

*DGIOT*  is the first lightweight open source industrial IoT continuous integration platform in China

Before 2016, the Shuwa team had been crawling on the Internet and the mobile Internet for many years. In 2016, it began to enter the Internet of Things crawling. It hopes to share many years of crawling experience through this open source platform and make multidisciplinary industrial Internet projects easier.
   + Let rich engineers to complete industrial Internet projects with simpler requirements through window interaction
   + Let the majority of junior front-end engineers to undertake industrial Internet projects with more complex requirements through the serverless method
   + Let Python, Java, Go, C junior background engineers undertake complex industrial Internet projects through web programming development channels

# Business architecture
  ![](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/msys64/dgiot_arch_msg.jpg)
# Core features
+ The front and back ends are completely open source, no-charge commercial version
+ Support privatization one-click deployment, quickly own your own lightweight IoT platform
+ Carrying over tens of millions of long connections, carrier-class stability
+ Object model-rule engine-data channel-configuration page full-process visualization low-code development
+ Multi-industry industrial protocol compatibility
# Building

 Bulid *DGIOT* Need Erlang/OTP R21+, Windows download  [msys64.zip](https://dgiotdev-1308220533.cos.ap-nanjing.myqcloud.com/msys64.zip)    ,After downloading, unzip it to the root directory of Disk D, and operate strictly in accordance with the following figure：
![dgiotdevtools.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/dgiotdevtools.png)

 +  Download source code abroad
  ```bash
     git clone -b master https://github.com/dgiot/dgiot-dashboard.git
     git clone https://github.com/dgiot/dgiot.git
   ```

 +  China download source code
   ```bash
     git clone -b master https://gitee.com/dgiiot/dgiot-dashboard.git
     git clone https://gitee.com/dgiiot/dgiot.git
   ```

 +  China Linux/Unix/Mac/windows Build
  ```bash
    cd dgiot-dashboard
    git pull
    pnpm install
    pnpm build
    cd ../dgiot
    git pull
    rm ./apps/dgiot_api/priv/www -rf
    cp ../dgiot-dashboard/dist/ ./apps/dgiot_api/priv/www/ -rf
    make run
    cd ./apps/dgiot_api/priv/www
    wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_dashboard.tar.gz &> /dev/null
    tar xf dgiot_dashboard.tar.gz &> /dev/null
    wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_file.tar.gz &> /dev/null
    tar xf dgiot_file.tar.gz &> /dev/null
 ```
+ Make Debug
 ```
  make DIAGNOSTIC=1
 ```
 *DGIOT* start, you can use a browser to visit  http://localhost to view Dashboard.。

- For a complete list of new features, see [DGIOT Release Notes](https://github.com/dgiot/dgiot/releases)。
- For more information, please visit [DGIOT Website](https://tech.iotn2n.com/)。

## Installation and deployment

 + **centos 7.6**


```
wget -q https://gitee.com/dgiiot/dgiot/raw/master/dgiot_install.sh  && sh dgiot_install.sh
```

## Community

### FAQ

Visit [DGIOT FAQ](https://tech.iotn2n.com/en/backend/) to get help on frequently asked questions

### FAQ

[GitHub Discussions](https://github.com/dgiot/dgiot_server/discussions)
[DGIOT Chinese Q&A Community](https://tech.iotn2n.com/)

### Involved in the design

If you have suggestions for improvements to DGIOT, you can submit PR and ISSUE to [EIP](https://github.com/dgiot/eip)

### Plug-in development

If you want to integrate or develop your own plug-in, refer to [lib-extra/README.md](./lib-extra/README.md)

You are welcome to submit any bugs, issues and feature requests to [dgiot/dgiot](https://github.com/dgiot/dgiot/issues)。

### About Us
| contact details       | address                                                                                      |
| -------------- | ----------------------------------------------------------------------------------------- |
| github         | [https://github.com/dgiot](https://github.com/dgiot?from=git)                             |
| gitee          | [https://gitee.com/dgiot](https://gitee.com/dgiiot?from=git)                              |
| Official website           | [https://www.iotn2n.com](https://www.iotn2n.com?from=git)                                 |
| Blog           | [https://tech.iotn2n.com](https://tech.iotn2n.com?from=git)                               |
| IoT access platform | [https://dgiot.iotn2n.com](https://dgiot.iotn2n.com?from=git)                             |
| The public         | ![qrcode.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/wechat/qrcode.png) |
|QQ group             | 346566935   |



## Preview address
[Tencent Cloud preview address](https://dgiotdashboard-8gb17b3673ff6cdd-1253666439.ap-shanghai.app.tcloudbase.com?ftom=git)


## Open source license
Apache License 2.0, 详见 [LICENSE](./LICENSE)。
