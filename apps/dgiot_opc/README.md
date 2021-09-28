## dgiot_opc

 DGIOT_OPC通道面向工业控制与工业检测场景,通过零代码或者低代码的方式快速完成项目

 OPC软件作为工业自动化领域应用最广泛的软件，深受工业控制人员的喜爱。但也有许多情况下，OPC软件并不能满足实际的使用需求：

*使用场景*

- 1.OPC只在内网运行，希望可以将数据传递至外网，随时随地查看
- 2.OPC数据难以存库
- 3.希望可以更好展示数据，进行数据分析
- ...

鉴此,DG-IoT专门提供了基于OPC通讯的OPC接口，以实现OPC数据的简单传输，解决行业痛点。

# 自动扫描点位
DG-IoT OPC通道与[DG-IoT桥接服务工具](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/blog/study/opc/dgiot_opc.zip)配合，实现opc转mqtt功能，支持opc指标扫描和读写。


# 指令集编排
DG-IoT OPC通道将自动处理扫描的点位信息，生成读取指定点位有效信息的指令集，通过该指令集读取OPC数据。

# 组态交互关联物模型
当OPC采集通道成功采集到OPC数据时，若该产品没有物模型，通道会自动生成与OPC信息对应的物模型。
![物模型1.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/blog/study/opc/%E7%89%A9%E6%A8%A1%E5%9E%8B1.png)

当OPC采集通道成功采集到OPC数据时会自动生成组态元素，实时展示当前OPC数据。

在设备管理 -> 点击组态
![点击组态.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/blog/study/opc/%E7%82%B9%E5%87%BB%E7%BB%84%E6%80%81.png)

![组态展示1.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/blog/study/opc/%E7%BB%84%E6%80%81%E5%B1%95%E7%A4%BA1.png)


# 数据自动存库
当使用OPC采集通道成功采集数据后，可在产品详情中->物存储中添加TD通道，OPC通道将自动把数据传递给存储数据的TD通道，将数据存入TD时序数据库

![TD通道日志.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/blog/study/opc/TD%E9%80%9A%E9%81%93%E6%97%A5%E5%BF%97.png)

具体使用操作以及更多信息可见[DG-IoT操作手册](https://tech.iotn2n.com/w/docs/details?id=9)

# 插件集成
 请见 https://github.com/dgiot/dgiot/tree/master/apps/dgiot_opc



