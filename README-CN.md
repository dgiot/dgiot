# DGIOT轻量级工业物联网开源平台

[![GitHub Release](https://img.shields.io/github/release/dgiot/dgiot?color=brightgreen)](https://github.com/dgiot/dgiot/releases)
[![Docker Pulls](https://img.shields.io/docker/pulls/dgiot/dgiot)](https://hub.docker.com/r/dgiot/dgiot)
[![Community](https://img.shields.io/badge/Community-DGIOT-yellow)](https://tech.iotn2n.com)
[![QQ群346566935](https://img.shields.io/badge/QQ群-346566935-brightgreen)](https://jq.qq.com/?_wv=1027&k=LipWZvDe)


[English](./README.md) | [中文](./README-CN.md) | [日本語](./README-JP.md) | [русский](./README-RU.md)|
[github](https://github.com/dgiot?from=git)|[gitee](https://www.iotn2n.com?from=git)|[官网](https://www.iotn2n.com?from=git)|[博客](https://tech.iotn2n.com?from=git)|[体验](https://prod.iotn2n.com?from=git)|[微信群](#jump)
## DG-IoT简介
DG-IoT是国内首款轻量级开源工业物联网平台，我们致力于提供五类物联网解决方案：
+ **国企/研究院**：平台代码开源，无版权产权困扰，国产无“卡脖”之忧
+ **系统集成商**：通用设备海量接入、定制设备二次开发、30分钟一键式私有化快速部署，低成本（降90%成本）
+ **工业设备制造商**：海量设备上线运维，不受公有云限制，低成本，短周期自建平台，私有化部署，数据安全
+ **开源平台开发者**：一键式开发环境，集成和兼容各种最优开源工具，快速承接物联网项目
+ **垂直领域物联网平台**：快速部署私有化平台，千万级承载，运营级底座，全开放扩展

## 快速体验与微信群
| 微信技术支持群 | [QQ技术支持群](https://jq.qq.com/?_wv=1027&k=LipWZvDe)   | 小程序体验 |电脑端https://prod.iotn2n.com|
|:---:|:---:|:---:|:---:|
|![image](https://user-images.githubusercontent.com/51999461/163802813-577e4345-9f3e-404f-963f-95862077ba13.png)|<img src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/QQ%E6%8A%80%E6%9C%AF%E7%BE%A4%E4%BA%8C%E7%BB%B4%E7%A0%81.png" width = "60%" /> |<img src="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_release/dgiot_wechat.jpg" width = "60%" />|运维账号：dgiot_admin  </br>  密码: dgiot_admin  </br> </br>开发账号: dgiot_dev  </br>  密码: dgiot_dev |


## 核心特色
+ **完全开源**：前后端完全开源，无收费商业版
+ **快速部署**：30分钟私有化[一键式部署](https://gitee.com/dgiiot/dgiot/wikis/%E4%BA%A7%E5%93%81%E6%89%8B%E5%86%8C/%E4%BA%A7%E5%93%81%E7%AE%80%E4%BB%8B/%E5%AE%89%E8%A3%85%E9%83%A8%E7%BD%B2)，快速拥有自己的物联网平台
+ **专业可靠**：千万级设备接入与管理，电信级稳定性
+ **兼容并包**：兼容工业领域多行业的常见协议类型
+ **全流程低代码**：物模型-规则引擎-数据通道-组态页面全流程可视化低代码开发
## 案例教程
|   |   | |
| ------------ | ------------ | ------------ |
| 行业分类 |实战教程| 案例简述  |
| [智慧能源](https://gitee.com/dgiiot/dgiot/wikis/%E8%A1%8C%E4%B8%9A%E6%A1%88%E4%BE%8B/%E8%83%BD%E6%BA%90%E8%A1%8C%E4%B8%9A?sort_id=4971731) | [电能表接入](https://gitee.com/dgiiot/dgiot/wikis/%E5%AE%9E%E6%88%98%E6%8E%A5%E5%85%A5/%E7%94%B5%E8%A1%A8%E6%8E%A5%E5%85%A5/%E6%A6%82%E8%BF%B0)|[千万级国/南网电表采集](https://gitee.com/dgiiot/dgiot/wikis/%E5%8E%8B%E6%B5%8B%E6%8A%A5%E5%91%8A/3000%E4%B8%87%E7%9C%81%E7%BA%A7%E7%94%B5%E8%A1%A8%E9%9B%86%E6%8A%84%E5%8E%8B%E6%B5%8B)，预付费表采集，太阳能设备管控  |
| [智能工业](https://gitee.com/dgiiot/dgiot/wikis/%E8%A1%8C%E4%B8%9A%E6%A1%88%E4%BE%8B/%E5%B7%A5%E4%B8%9A%E8%AE%BE%E5%A4%87%E8%A1%8C%E4%B8%9A)  | [Mqtt设备接入](https://gitee.com/dgiiot/dgiot/wikis/%E5%AE%9E%E6%88%98%E6%8E%A5%E5%85%A5/MQTT%E8%AE%BE%E5%A4%87%E6%8E%A5%E5%85%A5)| 工地电梯远程管控 |
| [智慧交通](https://gitee.com/dgiiot/dgiot/wikis/%E8%A1%8C%E4%B8%9A%E6%A1%88%E4%BE%8B/%E4%BA%A4%E9%80%9A%E8%A1%8C%E4%B8%9A)  |[Modbus设备接入](https://gitee.com/dgiiot/dgiot/wikis/%E5%AE%9E%E6%88%98%E6%8E%A5%E5%85%A5/Modbus%E8%AE%BE%E5%A4%87%E6%8E%A5%E5%85%A5?sort_id=5023597)|高速公路隧道设备检测   |
|[智慧检测](https://gitee.com/dgiiot/dgiot/wikis/%E8%A1%8C%E4%B8%9A%E6%A1%88%E4%BE%8B/%E6%99%BA%E6%85%A7%E6%A3%80%E6%B5%8B)|[OPC设备接入](https://gitee.com/dgiiot/dgiot/wikis/%E5%AE%9E%E6%88%98%E6%8E%A5%E5%85%A5/OPC%E8%AE%BE%E5%A4%87%E6%8E%A5%E5%85%A5/%E6%A6%82%E8%BF%B0)|浙里办水泵远程检测|
|[智慧物流](https://gitee.com/dgiiot/dgiot/wikis/%E8%A1%8C%E4%B8%9A%E6%A1%88%E4%BE%8B/%E6%99%BA%E6%85%A7%E7%89%A9%E6%B5%81)|[Zeta设备接入](https://gitee.com/dgiiot/dgiot/wikis/%E5%AE%9E%E6%88%98%E6%8E%A5%E5%85%A5/Zeta%E8%AE%BE%E5%A4%87%E6%8E%A5%E5%85%A5/%E6%A6%82%E8%BF%B0)|[千万级物流Zetag标签压测](https://gitee.com/dgiiot/dgiot/wikis/%E5%8E%8B%E6%B5%8B%E6%8A%A5%E5%91%8A/1500%E4%B8%87Zetag%E6%A0%87%E7%AD%BE%E7%89%A9%E6%B5%81%E5%85%A8%E4%B8%9A%E5%8A%A1%E5%8E%8B%E6%B5%8B)|
|[智慧园区](https://gitee.com/dgiiot/dgiot/wikis/%E8%A1%8C%E4%B8%9A%E6%A1%88%E4%BE%8B/%E6%99%BA%E6%85%A7%E5%9B%AD%E5%8C%BA)|[云云对接](https://gitee.com/dgiiot/dgiot/wikis/%E5%AE%9E%E6%88%98%E6%8E%A5%E5%85%A5/%E4%BA%91%E4%BA%91%E5%AF%B9%E6%8E%A5/%E6%A6%82%E8%BF%B0)|智慧场馆|

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

## 构建
 构建 *dgiot* 需要 Erlang/OTP R23+, Windows下用 [开发环境包](https://dgiotdev-1308220533.cos.ap-nanjing.myqcloud.com/msys64.zip)开发。下载后解压到D盘根目录，严格按照下图操作：
![dgiotdevtools.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/dgiotdevtools.png)
 +  国外下载源码
  ```bash
     git clone -b master https://github.com/dgiot/dgiot-dashboard.git
     git clone https://github.com/dgiot/dgiot.git
   ```
 +  国内下载源码
   ```bash
     git clone -b master https://gitee.com/dgiiot/dgiot-dashboard.git
     git clone https://gitee.com/dgiiot/dgiot.git
   ```
 +  国内Linux/Unix/Mac/windows 构建
  ```bash
    cd dgiot-dashboard
    git pull
    pnpm install
    pnpm build
    cd ./dist/
    wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_dashboard.tar.gz &> /dev/null
    tar xf dgiot_dashboard.tar.gz &> /dev/null
    wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_file.tar.gz &> /dev/null
    tar xf dgiot_file.tar.gz &> /dev/null
    wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_swagger.tar.gz &> /dev/null
    tar xf dgiot_swagger.tar.gz &> /dev/null
    rm -rf dgiot_swagger.tar.gz dgiot_file.tar.gz dgiot_dashboard.tar.gz
    cd ../../dgiot
    git pull
    rm ./apps/dgiot_api/priv/www -rf
    cp ../dgiot-dashboard/dist/ ./apps/dgiot_api/priv/www -rf
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
