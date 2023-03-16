# dgiot_bacnet

# BACnet 协议栈

# 关于这个项目

BACnet 协议源码库 提供 BACnet 应用层、网络层和媒体访问层通信服务。 
它是嵌入式系统、Windows、Linux 或其他操作系统的开源、免版税库。 
示例 BACnet 客户机和服务器应用程序 包括在内。

BACnet 
- 用于构建自动化和控制网络的数据通信协议 
- 请参阅 bacnet.org. BACnet 是一种用于楼宇自动化和控制网络的标准数据通信协议。 
  BACnet 是一个开放的协议，这意味着任何人都可以对标准做出贡献，任何人都可以使用它。 
  唯一注意是，BACnet 标准文档本身受 ASHRAE 的版权保护，他们出售该文档以帮助支付开发和维护标准的成本 （就像 IEEE 或 ANSI 或 ISO）。
    对于软件开发人员来说，BACnet 协议是在连接上发送和接收消息的标准方式，这些消息包含其他 BACnet 兼容设备能够理解的数据。 
  BACnet 标准定义了一种通过多种连线进行通信的标准方式，称为数据链路/物理层：以太网、EIA-485、EIA-232、ARCNET 和 LonTalk。
  BACnet 标准还定义了使用 UDP、IP 和 HTTP（Web 服务）进行通信的标准方法。

# 还有其他的 BACnet 项目：

- VTS - Win32 的 visual test shell， 用于可视化测试 BACnet 实现。它还包括 BACnet 消息的详细网络嗅探器，以及发送任何 BACnet 服务的能力。 源代码在公共域中。
- Wireshark - 支持 BACnet 的开源跨平台协议分析器。 详细的 BACnet 支持始于2005年5月4日发布的版本0.10.11，当时 Wireshark 被称为 Ethereal。
- BACnet4Linux - 需要 Linux 作为操作系统的 LGPL BACnet 应用程序。
- BACnet Firewall Router - 将 BACnet 路由功能与流量管理功能相结合的应用程序，可以更好控制对楼宇自动化和控制网络的访问。
- BACpypes - 使用 Python 编写的 BACnet 协议栈。
- BACsharp - 使用 C# 编写的 BACnet/IP 协议栈。
- BACnet4J - 使用 Java 编写的 BACnet/IP 协议栈，充当 Mango的 BACnet 层， Mango 是一种开源的机器对机器的软件（又名工业控制、SCADA、HMI 或 domotics）。
还有用于嵌入式应用的 BACnet 的商业 BACnet 协议源代码库：

- CimetricsTM - 有一个 BACstac/32 源码库， 作为其 BACnet 协议栈 SDK 的一部分。
- Polarsoft - 有一个用于嵌入式的协议源码库， 叫 FreeRangeTM 和 PolarSoft® FreeRange VSB (非常小的 BACnet 协议栈).
- SCADA Engine - BACnet Linux 服务器是在 Linux 平台上运行的一个完整的 BACnet 设备。 完整的源代码可用于定制应用程序，并用 ANSI C 编写，已成功地移植到 Unix、VxWorks 等。
- BACnet Stack - Chipkin 自动化系统 BACnet 协议栈是嵌入式系统和应用程序开发的应用层 BACnet 库。


# 一、开发准备

    a、模拟器  VTS和BACnetDeviceSimulator

b、主站  BACnetScan

c、参考文档 http://wenku.baidu.com/view/3052760f5acfa1c7aa00cc89.html?from=search

d、参考项目 https://github.com/kib357/BACsharp

# 二、开发概述

    1、采集软件（主站）UDP监听47808 端口 ，同时向从站的网络47808端口广播召唤仪表（whois）

2、从站收到召唤指令向主站回复（IAM）

3、主站收到从站IAM之后，接着发送readproperty（propertylist指令），问询从站所有的数据点属性列表

4、从站回复读属性列表，主站接着（批量）读取属性的presentvalue

# 三、重难点

1、字节解析

2、报文分段
