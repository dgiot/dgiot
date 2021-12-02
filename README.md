#  DGIOT

[![GitHub Release](https://img.shields.io/github/release/dgiot/dgiot?color=brightgreen)](https://github.com/dgiot/dgiot/releases)
[![Build Status](https://travis-ci.org/dgiot/dgiot.svg)](https://travis-ci.org/dgiot/dgiot)
[![Docker Pulls](https://img.shields.io/docker/pulls/dgiot/dgiot)](https://hub.docker.com/r/dgiot/dgiot)
[![Community](https://img.shields.io/badge/Community-DGIOT-yellow)](https://tech.iotn2n.com)
[![QQ群346566935](https://img.shields.io/badge/QQ群-346566935-brightgreen)](https://jq.qq.com/?_wv=1027&k=LipWZvDe_Owr-g6rfiTBC5DKEY59&jump_from=webapi)


[English](./README.md) | [简体中文](./README-CN.md) | [日本語](./README-JP.md) | [русский](./README-RU.md)

*DGIOT*  is the first lightweight open source industrial IoT continuous integration platform in China

Before 2016, the Shuwa team had been crawling on the Internet and the mobile Internet for many years. In 2016, it began to enter the Internet of Things crawling. It hopes to share many years of crawling experience through this open source platform and make multidisciplinary industrial Internet projects easier.
   + Let rich engineers to complete industrial Internet projects with simpler requirements through window interaction
   + Let the majority of junior front-end engineers to undertake industrial Internet projects with more complex requirements through the serverless method
   + Let Python, Java, Go, C junior background engineers undertake complex industrial Internet projects through web programming development channels

# Vision
  The DGIOT team hopes to achieve the following visions through the Shuwa Industrial Internet continuous integration platform
  + Through engineers, front-end engineers, and junior back-office engineers to actually complete small and medium-sized industrial Internet projects in no more than 1 month
  + Ensure high-quality delivery through multiple methods such as open source code, free software, document sharing, technical certification, product certification, operation and maintenance hosting, etc.
  + Experts in the technical field continue to integrate the industry's excellent technical framework, and experts in the business field continue to optimize business models and processes, and build a multidisciplinary open platform
  + The IoT platform can finally be simple and easy to use, returning to the essence of tooling

# Building

 Bulid *DGIOT* Need Erlang/OTP R21+, Windows download  [msys64.zip](https://dgiot-dev-1306147891.cos.ap-nanjing.myqcloud.com/msys64/msys64.zip)    ,After downloading, unzip it to the root directory of Disk D, and operate strictly in accordance with the following figure：
![效果图-1.png](https://dgiot-dev-1306147891.cos.ap-nanjing.myqcloud.com/msys64/%E6%95%88%E6%9E%9C%E5%9B%BE-1.png))

 +  Download source code abroad
  ```bash
     git clone https://github.com/dgiot/dgiot_dashboard.git
     git clone https://github.com/dgiot/dgiot.git
   ```

 +  China download source code
   ```bash
     git clone https://gitee.com/dgiiot/dgiot_dashboard.git
     git clone https://gitee.com/dgiiot/dgiot.git
   ```

 +  China Linux/Unix/Mac/windows Build
  ```bash
    cd dgiot_dashboard
    git pull
    yarn dgiot:install
    yarn build
    cd ../dgiot
    git pull
    rm ./apps/dgiot_api/priv/www -rf
    cp ../dgiot_dashboard/dist/ ./apps/dgiot_api/priv/www -rf
    make run
 ```
+ Make Debug
 ```
  make DIAGNOSTIC=1
 ```
 *DGIOT* start, you can use a browser to visit http://localhost:5080 to view Dashboard.。

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

### Contact us
You can contact the DGIOT community and developers through the following channels:
- [official](https://www.iotn2n.com)
- [ask](https://ask.iotn2n.com/)
- [Blog](https://tech.iotn2n.com)
- [Twitter](https://twitter.com/)
- [Facebook](https://www.facebook.com/)
- [Reddit](https://www.reddit.com/)
- [Weibo](https://weibo.com)

## Preview address
[Tencent Cloud preview address](https://dgiotdashboard-8gb17b3673ff6cdd-1253666439.ap-shanghai.app.tcloudbase.com?ftom=git)

## Scan code preview
![dgiot_dashboard.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/wechat/dgiot_dashboard.png)


## Open source license
Apache License 2.0, 详见 [LICENSE](./LICENSE)。
