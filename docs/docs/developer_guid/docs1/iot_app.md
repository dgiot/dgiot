  ---
  title: 用户场景3
  sidebar_position: 1
  ---
     
     
 #  用户场景3
 从用户到设备，主要需要解决如下几个问题：
 + 人与设备的关系，User基于流动性，权限系统里一般不会直接绑定User与Device的权限关系，中间会通过Department(Role)来间接绑定ACL
 + 设备与设备的关系，设备与设备之间有可能存在真实的关系，例如DTU与Meter，也可能只有一种虚拟关系，例如Group与DTU，属于因工程需要，临时拉群
 + 对具体设备来说，1、需要一个UID来表征身份；2、需要一个route来表征关系；3、需要多个tag来表征特征
 + 重点讨论Meter、DTU和TCP Server的交互过程
 
   | No.|名称|   Meter         |   DTU                  | TCP Server                 |  说明      |
   | --| ----   | -------      | ------                 | -----------               |-----------|
   |1 |连接     |               | send ->  [IP]           | ack <-- [IP]             | 必选      |
   |2 |登陆     |               | send ->  [DtuAddr]      | ack <-- [DtuAddr]        | 可选，可用IP代替|
   |3 |扫串口   | ack-> [485]   | send <-- [search 485]   | send <--[search 485]    | 可选，有档案可免 |
   |4 |扫modbus | ack-> [modbus]| send <-- [search modbus]   | send <--[search modbus] |可选，有档案可免 |
   |5 |扫表 | ack-> [Meter Addr]| send <-- [search meter]   | send <--[search meter] |可选，有档案可免 |
   |6 |抄表 | ack-> [Meter Data]| send <-- [read meter]   | send <--[read meter] |必选 |
   |7 |写表 | ack-> [Meter Control]| send <-- [write meter]   | send <--[write meter] |可选 |
   |8 |登出 |       |  send -> [DtuAddr] |  ack <-- [DtuAddr]        |可选 |
   |9 |断开 |     |  send -> [IP]      |  do_something        |必选 |
   
   扫串口、扫描modbus、扫表是一个费时费流量的操作，例如扫表一般至少需要扫256次，一般只会在物联网工程施工阶段进行，并完成相应的自动注册功能，形成设备档案，正常运行中不进行这些操作。
   
   这也是为什么电力抄表非常强调电表档案建设，如果没有档案，每一次DTU掉线都需要重新进行非常耗时耗流量的扫描任务   
   
  整体交互流程如下
 
 ```
 ---------------------------------------------------------------------------------------------------------------------
 |             物理层                                      |   连接层                 |      虚拟层            | 应用层|
 ---------------------------------------------------------------------------------------------------------------------
          User1(Session)                User2(Session)                           
              |                              |           
             API                            API             <--http--> shuwa_rest  --+
              |                              |                                       |
              +                              +                                       |
         Department1(Role)             Department2(Role)                             |
              |                              |                                       |
             ACL                            ACL            <--parse--> shuaw_parse --+
              +                              +                                       |              +-- 时序数据--+
          Group(Devcie)                 Group(Devcie)                                |              |            |
              |                              |                                       | === 流计算==> 物理层镜像    +--> 批量计算      
     +--------+-------+                      +                                       |              |            |
     |                |                      |                                       |              +-- 关系数据--+              
DTU1(Device1)    DTU2(Device)           DTU3(Device)        <--tcp-->  tcp_server ---+
     |                |                      |                                       | 
 modbus             modbus                modbus            <--modbus-->  proctol ---+
     |                |                      |                                       |
     +                +                      +                                       |
    485              485                     485             <--485-->    proctol  --+ 
     |                |                      |                                       |
     +                +             +--------+--------+                              |
     |                |             |                 |                              |
 Meter1(Device) Meter2(Device) Meter4(Device）Meter5(Device）<--DLT/645--> proctol --+                
```
 在正式动手之前，请先体会一下心得和理解用户场景
 
 # 培训纲要
 
 - 学会创建一个部门
 - 学会创建一个岗位
 - 学会创建一个用户
 - 学会创建一个应用
 - 学习创建一个API
 - 学会创建一个分组
 - 学会创建一个产品
 - 学会创建一个设备
 - 学会创建一个指令
 - 学会创建一个子设备
 - 学会创建一个物模型
 - 学会创建一个组态
 - 学会创建一个通道
 - 学会创建一个规则
 - 学会开发一个插件
 - 学会接入一个设备
 - 学会设计一个统计量
 - 学会状态迁移编程
 - 学会网络通信编程
 - 学会消息路由编程
 - 学会设备动态注册

 # 详细步骤
