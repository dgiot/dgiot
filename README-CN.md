# DGIOT轻量级工业物联网开源平台

[![GitHub Release](https://img.shields.io/github/release/dgiot/dgiot?color=brightgreen)](https://github.com/dgiot/dgiot/releases)
[![Docker Pulls](https://img.shields.io/docker/pulls/dgiot/dgiot)](https://hub.docker.com/r/dgiot/dgiot)
[![Community](https://img.shields.io/badge/Community-DGIOT-yellow)](https://www.dgiotcloud.cn/)
[![QQ群346566935](https://img.shields.io/badge/QQ群-346566935-brightgreen)](https://jq.qq.com/?_wv=1027&k=LipWZvDe)


[English](./README.md) | [中文](./README-CN.md) | [日本語](./README-JP.md) | [русский](./README-RU.md)|
[github](https://github.com/dgiot)|[gitee](https://gitee.com/dgiiot)|[官网](https://www.dgiotcloud.cn/)|[博客](https://www.dgiotcloud.cn/?cat=19)|[体验](https://prod.dgiotcloud.cn)|[微信群](#jump)

## DGIOT简介
DGIOT是国内首款轻量级开源工业物联网平台，我们致力于提供五类物联网解决方案：
+ **国企/研究院**：平台代码开源，无版权产权困扰，国产无“卡脖”之忧
+ **系统集成商**：通用设备海量接入、定制设备二次开发、6分钟一键式私有化快速部署，低成本（降90%成本）
+ **工业设备制造商**：海量设备上线运维，不受公有云限制，低成本，短周期自建平台，私有化部署，数据安全
+ **开源平台开发者**：一键式开发环境，集成和兼容各种最优开源工具，快速承接物联网项目
+ **垂直领域物联网平台**：快速部署私有化平台，千万级承载，运营级底座，全开放扩展

## 快速体验与微信群
| 微信技术支持群 | [QQ技术支持群](https://jq.qq.com/?_wv=1027&k=LipWZvDe)   | 小程序体验 | 官网案例 |
|:---:|:---:|:---:|:---:|
|<img src="https://dgiot-web-1306147891.cos.ap-nanjing.myqcloud.com/wechat.png" width = "100%" />|<img src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/QQ%E6%8A%80%E6%9C%AF%E7%BE%A4%E4%BA%8C%E7%BB%B4%E7%A0%81.png" width = "100%" /> |<img src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_release/dgiot_wechat.jpg" width = "100%" /> | [官网体验地址](https://prod.dgiotcloud.cn)  </br> </br> [智慧园区](https://www.dgiotcloud.cn/smartpark/) </br></br> [平安校园](https://www.dgiotcloud.cn/smartcampus/)|

## 核心特色
+ **完全开源**：前后端完全开源，无收费商业版
+ **快速部署**：6分钟私有化[一键式部署](https://doc.dgiotcloud.cn/docs/product_doc/docs/deployment_details/)，快速拥有自己的物联网平台
+ **专业可靠**：千万级设备接入与管理，电信级稳定性
+ **兼容并包**：兼容工业领域多行业的常见协议类型
+ **全流程低代码**：物模型-规则引擎-数据通道-组态页面全流程可视化低代码开发

## 案例教程
|   |   | |
| ------------ | ------------ | ------------ |
| 行业分类 |实战教程| 案例简述  |
| [智慧能源](https://doc.dgiotcloud.cn/docs/user_manual/docs/wisdom_energy/) | [电能表接入](https://doc.dgiotcloud.cn/docs/practical_tutorial/docs/Meter/dlt645)|[千万级国/南网电表采集](https://doc.dgiotcloud.cn/docs/user_manual/docs/pressure_test/meter_pressure_test)，预付费表采集，太阳能设备管控  |
| [智能制造](https://doc.dgiotcloud.cn/docs/user_manual/docs/digital_factory/)  | [Mqtt设备接入](https://gitee.com/dgiiot/dgiot/wikis/%E5%AE%9E%E6%88%98%E6%8E%A5%E5%85%A5/MQTT%E8%AE%BE%E5%A4%87%E6%8E%A5%E5%85%A5)| 工地电梯远程管控 |
| [智慧交通](https://gitee.com/dgiiot/dgiot/wikis/%E8%A1%8C%E4%B8%9A%E6%A1%88%E4%BE%8B/%E4%BA%A4%E9%80%9A%E8%A1%8C%E4%B8%9A)  |[Modbus设备接入](https://gitee.com/dgiiot/dgiot/wikis/%E5%AE%9E%E6%88%98%E6%8E%A5%E5%85%A5/Modbus%E8%AE%BE%E5%A4%87%E6%8E%A5%E5%85%A5?sort_id=5023597)|高速公路隧道设备检测   |
| [智慧检测](https://gitee.com/dgiiot/dgiot/wikis/%E8%A1%8C%E4%B8%9A%E6%A1%88%E4%BE%8B/%E6%99%BA%E6%85%A7%E6%A3%80%E6%B5%8B)|[OPC设备接入](https://gitee.com/dgiiot/dgiot/wikis/%E5%AE%9E%E6%88%98%E6%8E%A5%E5%85%A5/OPC%E8%AE%BE%E5%A4%87%E6%8E%A5%E5%85%A5/%E6%A6%82%E8%BF%B0)|浙里办水泵远程检测|
| [智慧物流](https://gitee.com/dgiiot/dgiot/wikis/%E8%A1%8C%E4%B8%9A%E6%A1%88%E4%BE%8B/%E6%99%BA%E6%85%A7%E7%89%A9%E6%B5%81)|[Zeta设备接入](https://gitee.com/dgiiot/dgiot/wikis/%E5%AE%9E%E6%88%98%E6%8E%A5%E5%85%A5/Zeta%E8%AE%BE%E5%A4%87%E6%8E%A5%E5%85%A5/%E6%A6%82%E8%BF%B0)|[千万级物流Zetag标签压测](https://gitee.com/dgiiot/dgiot/wikis/%E5%8E%8B%E6%B5%8B%E6%8A%A5%E5%91%8A/1500%E4%B8%87Zetag%E6%A0%87%E7%AD%BE%E7%89%A9%E6%B5%81%E5%85%A8%E4%B8%9A%E5%8A%A1%E5%8E%8B%E6%B5%8B)|
| [智慧园区](https://doc.dgiotcloud.cn/docs/user_manual/docs/wisdom_park/)|[云云对接](https://gitee.com/dgiiot/dgiot/wikis/%E5%AE%9E%E6%88%98%E6%8E%A5%E5%85%A5/%E4%BA%91%E4%BA%91%E5%AF%B9%E6%8E%A5/%E6%A6%82%E8%BF%B0)|智慧场馆|

## 业务架构
![业务架构图.jpg](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/%E4%B8%9A%E5%8A%A1%E6%9E%B6%E6%9E%84%E5%9B%BE.jpg)

## 系统部署
+ **centos 7.6/7.9** 推荐使用腾讯云和阿里云部署，具体操作[点击这里](https://gitee.com/dgiiot/dgiot/wikis/DG-IoT%E7%89%A9%E8%81%94%E7%BD%91%E6%89%8B%E5%86%8C/%E7%AE%80%E4%BB%8B/%E5%AE%89%E8%A3%85%E9%83%A8%E7%BD%B2)
```
wget -qO dgiot_install.sh https://gitee.com/dgiiot/dgiot/raw/master/dgiot_install.sh  && sh dgiot_install.sh
```
|   |   | |
| -----| ------ |------ |
|内容  |  脚本 | 说明 |
| 版本更新  | sh dgiot_install.sh -s {版本} | [版本列表](https://gitee.com/dgiiot/dgiot/wikis/%E4%BA%A7%E5%93%81%E6%89%8B%E5%86%8C/%E4%BA%A7%E5%93%81%E7%AE%80%E4%BB%8B/%E7%B3%BB%E7%BB%9F%E9%83%A8%E7%BD%B2/%E7%89%88%E6%9C%AC%E5%88%97%E8%A1%A8)|
| 证书安装  | sh dgiot_install.sh -d {域名} | [证书制作](https://gitee.com/dgiiot/dgiot/wikis/%E4%BA%A7%E5%93%81%E6%89%8B%E5%86%8C/%E4%BA%A7%E5%93%81%E7%AE%80%E4%BB%8B/%E5%AE%89%E8%A3%85%E9%83%A8%E7%BD%B2)|
| 本地构建  | sh dgiot_install.sh -v devops -s {版本} -p {插件名称} | [插件开发](https://gitee.com/dgiiot/dgiot/wikis/%E5%BC%80%E5%8F%91%E6%8C%87%E5%8D%97/%E6%8F%92%E4%BB%B6%E5%BC%80%E5%8F%91/%E6%A6%82%E8%BF%B0) |
|Docker部署|https://hub.docker.com/r/dgiot/dgiot|

## 服务支持
数字化需求日益增长与企业缺钱缺人缺经验的矛盾日益突出，为此dgiot坚持代码开源，产品免费，持续改善物联网服务性价比，同时倡议：
+ 欢迎dgiot用户多尝试自助服务，多参与社区建设，多提issue，多在qq或者微信上反馈真实的物联网需求，助力提升社区服务品质
+ 欢迎dgiot先来者可以为后来者提供系统部署，插件开发和设备接入等指导服务，一起提供更多优质VIP服务，构建可持续的社区生态
+ dgiot团队在保守用户商业机密的前提下，将持续提炼行业业务需求和物联网平台运维经验，把行业真需求和一线经验沉淀到社区生态中

|   |   | | |
| -----| ------ |------- |------- |
| 内容  |  服务等级 |服务收费| 服务方式 |
| 一键系统部署  | 自助  |  免费 | 点击查看部署脚本，社区服务 |
| [一键插件开发](https://gitee.com/dgiiot/dgiot/wikis/%E4%B8%80%E9%94%AE%E9%83%A8%E7%BD%B2/%E7%B3%BB%E7%BB%9F%E9%83%A8%E7%BD%B2)  | 自助 |  免费 | 点击查看DevOps脚本，社区服务 |
| 设备接入教程  | 自助 |  免费 | 点击查看教程，社区服务 |
| 脚本使用指导  | 半小时   |  百元级 | 远程VIP服务|
| 设备接入指导  | 半天 | 千元级 | 远程VIP服务 |
| 项目经理培训  | 三天 | 数千元级 | 线下培训服务 |
| 应用开发培训  | 三周 | 万元级 | 线下培训服务 |
| [业务模拟压测](https://gitee.com/dgiiot/dgiot/wikis/%E4%BA%A7%E5%93%81%E6%89%8B%E5%86%8C/%E4%BA%91%E5%8E%8B%E6%B5%8B/%E4%BA%A7%E5%93%81%E5%8A%9F%E8%83%BD)  | 一月 | 万元级 | 7 X 24 小时VIP服务 |
| 企业设备管控  | 一年 | 十万元级 | 5 X 8 小时VIP服务 |
| 行业运营平台  | 一年 | 面谈 | 7 X 24 小时VIP服务 |

## 工具矩阵

![dgiot_family.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/dgiot_family.png)

## 构建
构建 *dgiot* 需要 Erlang/OTP R24+, [linux](https://github.com/erlang/otp/releases/download/OTP-24.3.4.2/otp_src_24.3.4.2.tar.gz) Windows下用 [开发环境包](https://dgiotdev-1308220533.cos.ap-nanjing.myqcloud.com/msys64.zip)开发。下载后解压到D盘根目录，严格按照下图操作：
![dgiotdevtools.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/dgiotdevtools.png)
+  国外下载源码
```bash
 git clone -b master https://github.com/dgiot/iotStudio.git
 git clone -b master https://github.com/dgiot/iotView.git
 git clone -b master https://github.com/dgiot/iotApp.git
 git clone -b master https://github.com/dgiot/iotWechat.git
 git clone https://github.com/dgiot/dgiot.git
```
+  国内下载源码
```bash
  git clone -b master https://gitee.com/dgiot/iotStudio.git
  git clone -b master https://gitee.com/dgiot/iotView.git
  git clone -b master https://gitee.com/dgiot/iotApp.git
  git clone -b master https://gitee.com/dgiot/iotWechat.git
  git clone https://gitee.com/dgiiot/dgiot.git
```
+  一键式启动命令Linux/Unix/Mac/windows
 ```bash
 make run
 ```
+ Make Debug
 ```
  make DIAGNOSTIC=1
 ```
*DGIOT* 启动，可以使用浏览器访问 http://localhost:5080 来查看 Dashboard。

- 新功能的完整列表，请参阅 [DGIOT Release Notes](https://github.com/dgiot/dgiot/releases)。
- 获取更多信息，请访问 [DGIOT 官网](https://www.dgiotcloud.cn/)。

## 参与设计

如果对 DGIOT 有改进建议，可以向[EIP](https://github.com/dgiot/eip) 提交 PR 和 ISSUE

## 插件开发

如果想集成或开发你自己的插件，参考 [lib-extra/README.md](./lib-extra/README.md)

欢迎你将任何 bug、问题和功能请求提交到 [dgiot/dgiot](https://github.com/dgiot/dgiot/issues)。

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

## 开源许可
Apache License 2.0, 详见 [LICENSE](./LICENSE)。
