#  DGIOT Lightweight industrial iot open source platform

[![GitHub Release](https://img.shields.io/github/release/dgiot/dgiot?color=brightgreen)](https://github.com/dgiot/dgiot/releases)
[![Docker Pulls](https://img.shields.io/docker/pulls/dgiot/dgiot)](https://hub.docker.com/r/dgiot/dgiot)
[![Community](https://img.shields.io/badge/Community-DGIOT-yellow)](https://www.dgiotcloud.cn/)
[![QQ群346566935](https://img.shields.io/badge/QQ群-346566935-brightgreen)](https://jq.qq.com/?_wv=1027&k=LipWZvDe)
[![star](https://gitee.com/dgiiot/dgiot/badge/star.svg?theme=gvp)](https://gitee.com/dgiiot/dgiot/stargazers)


[English](./README.md) | [简体中文](./README-CN.md) | [日本語](./README-JP.md) | [русский](./README-RU.md)


[github](https://github.com/dgiot?from=git)|[gitee](https://gitee.com/dgiiot)|[Official website](https://www.dgiotcloud.cn/)|[Blog](https://www.dgiotcloud.cn/?cat=19)|[Experience](https://prod.dgiotcloud.cn)|

*DGIOT*  is the first lightweight open source industrial iot continuous integration platform in China

Before 2016, the dgiot team had been crawling on the Internet and the mobile Internet for many years. In 2016, it began to enter the Internet of Things crawling. It hopes to share many years of crawling experience through this open source platform and make multidisciplinary industrial Internet projects easier.
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

![dgiot_family.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/dgiot_family.png)

# Building

 Bulid *DGIOT* Need Erlang/OTP R24+, [linux](https://github.com/erlang/otp/releases/download/OTP-24.3.4.2/otp_src_24.3.4.2.tar.gz) Windows download  [msys64.zip](https://prod.dgiotcloud.cn/msys64.zip)    ,After downloading, unzip it to the root directory of Disk D, and operate strictly in accordance with the following figure：
![dgiotdevtools.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/dgiotdevtools.png)

 +  Download source code abroad
     ```bash
     git clone -b master https://github.com/dgiot/iotStudio.git
     git clone -b master https://github.com/dgiot/iotView.git
     git clone -b master https://github.com/dgiot/iotApp.git
     git clone -b master https://github.com/dgiot/iotWechat.git
     git clone https://github.com/dgiot/dgiot.git
    ```

 +  China download source code
     ```bash
      git clone -b master https://gitee.com/dgiot/iotStudio.git
      git clone -b master https://gitee.com/dgiot/iotView.git
      git clone -b master https://gitee.com/dgiot/iotApp.git
      git clone -b master https://gitee.com/dgiot/iotWechat.git
      git clone https://gitee.com/dgiiot/dgiot.git
    ```

 +  One touch start command Linux/Unix/Mac/windows
 ```bash
 make run
 ```
+ Make Debug
 ```
  make DIAGNOSTIC=1
 ```
 *DGIOT* start, you can use a browser to visit  http://localhost to view Dashboard.。

- For a complete list of new features, see [DGIOT Release Notes](https://github.com/dgiot/dgiot/releases)。
- For more information, please visit [DGIOT Website](https://www.dgiotcloud.cn/)。

## Installation and deployment

 + **centos 7.6**


```
wget -q https://gitee.com/dgiiot/dgiot/raw/master/dgiot_install.sh  && sh dgiot_install.sh
```

## Community

### FAQ

Visit [DGIOT FAQ](https://www.dgiotcloud.cn/?cat=19) to get help on frequently asked questions

### FAQ

[GitHub Discussions](https://github.com/dgiot/dgiot_server/discussions)
[DGIOT Chinese Q&A Community](https://www.dgiotcloud.cn/?page_id=12)

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
| Official website           | [https://www.dgiotcloud.cn](https://www.dgiotcloud.cn)                                 |
| Blog           | [https://www.dgiotcloud.cn/?cat=19](https://www.dgiotcloud.cn/?cat=19)                               |
| IoT access platform | [https://prod.dgiotcloud.cn](https://prod.dgiotcloud.cn)                             |
| The public         | ![qrcode.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/wechat/qrcode.png) |
|We chat group|<img src="https://dgiot-web-1306147891.cos.ap-nanjing.myqcloud.com/wechat230703.png" width = "25%" />|
| QQ group             | 346566935   |

## 平台介绍
<table >
    <tr style={{border : 'none'}}>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1yP411V7k7?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E5%B9%B3%E5%8F%B0%E4%BB%8B%E7%BB%8D/1.jpg" /></a></td>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1GP4y1L7Sv?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E5%B9%B3%E5%8F%B0%E4%BB%8B%E7%BB%8D/2.jpg" /></a></td>
    </tr>
    <tr class="td1" valign="top" style={{border : 'none',background : '#fff'}}>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1yP411V7k7?spm_id_from=333.999.0.0" title="DG-IoT平台简介及业务架构">DG-IoT平台简介及业务架构</a></td>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1GP4y1L7Sv?spm_id_from=333.999.0.0" title="DG-IoT平台业务架构详细讲解">DG-IoT平台业务架构详细讲解</a></td>
     </tr>
</table>

## 服务器部署教程
<table style={{border : 'none'}}>
    <tr style={{border : 'none'}}>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1fS4y1z7Gp?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E6%9C%8D%E5%8A%A1%E5%99%A8%E9%83%A8%E7%BD%B2%E6%95%99%E7%A8%8B/1.jpg" /></a></td>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV18b4y1x77E?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E6%9C%8D%E5%8A%A1%E5%99%A8%E9%83%A8%E7%BD%B2%E6%95%99%E7%A8%8B/2.jpg" /></a></td>
    </tr>
    <tr class="td1" valign="top" style={{border : 'none',background : '#fff'}}>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1fS4y1z7Gp?spm_id_from=333.999.0.0" title="DGIoT开源物联网平台——腾讯云服务器购买">DGIoT开源物联网平台——腾讯云服务器购买</a></td>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV18b4y1x77E?spm_id_from=333.999.0.0" title="DGIoT开源物联网平台——服务器部署">DGIoT开源物联网平台——服务器部署</a></td>
     </tr>
</table>

## 实战教程

<table style={{border : 'none'}}>
    <tr style={{border : 'none'}}>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1GP4y1M7ot?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E5%AE%9E%E6%88%98%E6%95%99%E7%A8%8B/1.jpg" /></a></td>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1MV4y1p7Yn?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E5%AE%9E%E6%88%98%E6%95%99%E7%A8%8B/2.jpg" /></a></td>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1XF411473H?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E5%AE%9E%E6%88%98%E6%95%99%E7%A8%8B/3.jpg" /></a></td>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1fG41157CS?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E5%AE%9E%E6%88%98%E6%95%99%E7%A8%8B/4.jpg" /></a></td>
    </tr>
    <tr class="td1" valign="top" style={{border : 'none',background : '#fff'}}>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV18d4y1R74q?spm_id_from=333.999.0.0" title="MODBUS温湿度平台接入">MODBUS温湿度平台接入</a></td>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV18d4y1R74q?spm_id_from=333.999.0.0" title="modbus虚拟设备平台接入">modbus虚拟设备平台接入</a></td>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV18d4y1R74q?spm_id_from=333.999.0.0" title="DGIoT实物电表连接">DGIoT实物电表连接</a></td>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV18d4y1R74q?spm_id_from=333.999.0.0" title="虚拟电表连接">虚拟电表连接</a></td>
     </tr>
    <tr style={{border : 'none'}}>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1EU4y1z7wF?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E5%AE%9E%E6%88%98%E6%95%99%E7%A8%8B/5.jpg" /></a></td>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1334y1b7qc?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E5%AE%9E%E6%88%98%E6%95%99%E7%A8%8B/6.jpg" /></a></td>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1pF411t7bq?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E5%AE%9E%E6%88%98%E6%95%99%E7%A8%8B/3.jpg" /></a></td>
    </tr>
    <tr class="td1" valign="top" style={{border : 'none',background : '#fff'}}>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV18d4y1R74q?spm_id_from=333.999.0.0" title="mqtt虚拟设备连接">mqtt虚拟设备连接</a></td>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV18d4y1R74q?spm_id_from=333.999.0.0" title="opc虚拟设备连接">opc虚拟设备连接</a></td>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV18d4y1R74q?spm_id_from=333.999.0.0" title="dgiot低代码远程控制电表-拉合闸视频演练">dgiot低代码远程控制电表-拉合闸视频演练</a></td>
    </tr>
</table>

## 开发教程
<table style={{border : 'none'}}>
    <tr style={{border : 'none'}}>
       <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1Yx4y1f791/?spm_id_from=333.337"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/developer_guid/front_end/dgiot_build.png" /></a></td>
      <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1A44y1U7pk/?spm_id_from=333.337"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/developer_guid/front_end/dgiot_wechat_dev.png" /></a></td>
      <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV15u411z73i?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E5%BC%80%E5%8F%91%E6%95%99%E7%A8%8B/1.jpg" /></a></td>
    </tr>
    <tr class="td1" valign="top" style={{border : 'none',background : '#fff'}}>
       <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1Yx4y1f791?spm_id_from=333.337" title="dgiot编译环境搭建踩坑记之深夜鏖战">dgiot编译环境搭建踩坑记之深夜鏖战</a></td>
       <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV15u411z73i?spm_id_from=333.337" title="微信小程序零基础接入教程">微信小程序零基础接入教程</a></td>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV15u411z73i?spm_id_from=333.999.0.0" title="dgiot api编写教程">dgiot api编写教程</a></td>
     </tr>
</table>


## 智慧校园传感接入教程
<table style={{border : 'none'}}>
    <tr style={{border : 'none'}}>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1Zd4y1Q7Kf?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E6%99%BA%E6%85%A7%E6%A0%A1%E5%9B%AD%E4%BC%A0%E6%84%9F%E6%8E%A5%E5%85%A5%E6%95%99%E7%A8%8B/1.jpg" /></a></td>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1xe4y1h7Dk?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E6%99%BA%E6%85%A7%E6%A0%A1%E5%9B%AD%E4%BC%A0%E6%84%9F%E6%8E%A5%E5%85%A5%E6%95%99%E7%A8%8B/2.jpg" /></a></td>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1C14y1s75L?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E6%99%BA%E6%85%A7%E6%A0%A1%E5%9B%AD%E4%BC%A0%E6%84%9F%E6%8E%A5%E5%85%A5%E6%95%99%E7%A8%8B/3.jpg" /></a></td>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1nW4y1z7Nw?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E6%99%BA%E6%85%A7%E6%A0%A1%E5%9B%AD%E4%BC%A0%E6%84%9F%E6%8E%A5%E5%85%A5%E6%95%99%E7%A8%8B/4.jpg" /></a></td>
    </tr>
    <tr class="td1" valign="top" style={{border : 'none',background : '#fff'}}>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1Zd4y1Q7Kf?spm_id_from=333.999.0.0" title="DGIOT平台接入红外传感器教程">DGIOT平台接入红外传感器教程</a></td>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1xe4y1h7Dk?spm_id_from=333.999.0.0" title="DGIOT平台接入甲烷传感器教程">DGIOT平台接入甲烷传感器教程</a></td>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1C14y1s75L?spm_id_from=333.999.0.0" title="DGIOT平台接入水位传感器教程">DGIOT平台接入水位传感器教程</a></td>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1nW4y1z7Nw?spm_id_from=333.999.0.0" title="DGIOT平台接入烟感传感器教程">DGIOT平台接入烟感传感器教程</a></td>
     </tr>
    <tr style={{border : 'none'}}>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1C34y1H7rR?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E6%99%BA%E6%85%A7%E6%A0%A1%E5%9B%AD%E4%BC%A0%E6%84%9F%E6%8E%A5%E5%85%A5%E6%95%99%E7%A8%8B/5.jpg" /></a></td>
        <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV12f4y1o7d3?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E6%99%BA%E6%85%A7%E6%A0%A1%E5%9B%AD%E4%BC%A0%E6%84%9F%E6%8E%A5%E5%85%A5%E6%95%99%E7%A8%8B/6.jpg" /></a></td>
    </tr>
    <tr class="td1" valign="top" style={{border : 'none',background : '#fff'}}>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV1C34y1H7rR?spm_id_from=333.999.0.0" title="DGIOT平台接入噪声传感器教程">DGIOT平台接入噪声传感器教程</a></td>
        <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV12f4y1o7d3?spm_id_from=333.999.0.0" title="DGIOT平台接入振动传感器教程">DGIOT平台接入振动传感器教程</a></td>
    </tr>
</table>

## 案例视频
<table style={{border : 'none'}}>
    <tr style={{border : 'none'}}>
            <td style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV18d4y1R74q?spm_id_from=333.999.0.0"><img width="216px" src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_doc/doc%E8%A7%86%E9%A2%91%E5%9B%BE%E7%89%87/%E6%A1%88%E4%BE%8B%E8%A7%86%E9%A2%91/1.jpg" /></a></td>
    </tr>
    <tr class="td1" valign="top" style={{border : 'none',background : '#fff'}}>
            <td class="a1" style={{border : 'none'}}> <a href="https://www.bilibili.com/video/BV18d4y1R74q?spm_id_from=333.999.0.0" title="海量用电信息采集项目性能测试">海量用电信息采集项目性能测试</a></td>
    </tr>
</table>

## Preview address
[Tencent Cloud preview address](https://dgiotdashboard-8gb17b3673ff6cdd-1253666439.ap-shanghai.app.tcloudbase.com?ftom=git)


## Open source license
Apache License 2.0, 详见 [LICENSE](./LICENSE)。

