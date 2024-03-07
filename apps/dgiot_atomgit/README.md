
<a href="https://competition.atomgit.com/competitionInfo?id=2b580e8c23bd4f1ef2cef3346b36600f"><img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/bm.png" /></a>

## **一：竞赛流程**

请说明本赛项的赛程设置、阶段划分及时间安排等。
 <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/baoming.png" width = "100%" />

## **二：大赛联系人邀请**
### 1: 邀请参赛选手加入私仓
  <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/admin_1.png" width = "100%" />
  <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/admin_2.png" width = "100%" />

##  **三：大赛参赛选手加入**
 ### 1：点击右上角邮箱查看邀请信息
  <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/player_1.png" width = "100%" />

###  2：查看邀请信息详情
  <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/player_2.png" width = "100%" />

###  3：接受邀请
  <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/player_3.png" width = "100%" />

 ###  4:查看私仓点击个人令牌
  <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/player_4.png" width = "100%" />

 ###  5:创建个人令牌
  <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/ctoken1.png" width = "100%" />

 ###  6:保存个人令牌
  <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/ctoken2.png" width = "100%" />

## **四：开发环境**
 ### 1：开发常见工具列表

|                                         linux/erlang                                          |                                windows/erlang                                 |                                             Putty                                             |                            winscp                            |                                  BCompare                                  |                                                                                                                                                                                                                                  idea |
|:---------------------------------------------------------------------------------------------:|:-----------------------------------------------------------------------------:|:---------------------------------------------------------------------------------------------:|:------------------------------------------------------------:|:----------------:|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
| [linux](https://github.com/erlang/otp/releases/download/OTP-24.3.4.2/otp_src_24.3.4.2.tar.gz) | [windows](https://dgiotdev-1308220533.cos.ap-nanjing.myqcloud.com/msys64.zip) | [putty](https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot4.0/windows/PuTTY.zip) | [winscp](https://winscp.net/download/WinSCP-6.3.1-Setup.exe) | [BCompare](https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot4.0/windows/BComparePortable.rar) |   [IDEA](https://download.jetbrains.com/idea/ideaIC-2023.3.4.exe?_gl=1*1r3j49k*_ga*NzkyOTYyNDQ2LjE2OTE3MjI5OTQ.*_ga_9J976DJZ68*MTcwODY3NDc0Mi4zLjEuMTcwODY3NTQxNy42MC4wLjA.&_ga=2.8291826.1742129966.1708674742-792962446.1691722994) |
|                                           linux开发环境                                           |                                  winows开发环境                                   |linux远程登录工具|linux文件传输工具|代码比较工具|代码编辑工具|

 ### 2：构建 *dgiot* 需要 Erlang/OTP R24+, [linux](https://github.com/erlang/otp/releases/download/OTP-24.3.4.2/otp_src_24.3.4.2.tar.gz) Windows下用 [开发环境包](https://dgiotdev-1308220533.cos.ap-nanjing.myqcloud.com/msys64.zip)开发。 下载后解压到D盘根目录，严格按照下图操作：
   <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/mk8.png" width = "100%" />
   管理员身份运行后弹出下面命令框，可以在该命令框下，拉取上传编译代码
   <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/mk9.png" width = "100%" />

   会在 **D:\msys64\home** 下生成一个已电脑名命名的文件夹作为工作空间（*注：名字必须为中文*）

   <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/mk10.png" width = "100%" />

## **五：插件开发**
   在windows开发环境下，选手**小明** 准备实现 **test** 协议插件

### 1：一键式生成插件框架

   **注：test替换成自己插件名字**
   ```
    git clone https://atomgit.com/dgiot/dgiot.git
    cd dgiot
    ./dgiot_install.sh -v atomgit -m test
   ```
   生成成功
   <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/mk1.jpg" width = "100%" />
 ###  2:查看生成的插件代码
   <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/mk3.png" width = "100%" />

   ##### 插件目录结构解释
   <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/mod1.png" width = "100%" />

###   3:编译启动工程
   ```
    make run
   ```
   启动成功后 **ctrl+c** 退出控制台
   <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/mk4.png" width = "100%" />
   弹出下面erlang控制台框编译成功
   <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/mk2.jpg" width = "100%" />
   在自己的插件代码中添加函数
   ```
    hello_world() ->
        io:format("~s ~p ~p.~n", [?FILE, ?LINE, <<"Hello World!!!">>]).
   ```
   <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/mk7.png" width = "100%" />

   + 修改自己的插件代码后可以在erlang控制台执行 **dgiot_plugin:compile(dgiot_test).** 进行热编译
   + 执行 **dgiot_test:hello_world().** 执行函数
   <img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/mk6.png" width = "100%" />

###   4:确认插件代码无误且能正确编译成功后再上传私仓

   **注：git remote 中的`username`和`password`替换成自己的用户名和个人令牌**
   ```
    git config --global user.name "xiaoming"
    git config --global user.email "xiaoming@qq.com"
    cd apps/dgiot_test
    git init
    git remote add origin https://username:password@atomgit.com/dgiot/dgiot_test.git
    git add .
    git commit -m "feat: first commit"
    git push -u origin master
   ```

###  5:查看私仓上传成功
<img src="https://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/atom/viewps.jpg" width = "100%" />


## **六：gRPC**
### 1:grpc介绍
  gRPC是一个高性能，开源和通用的RPC框架，基于Protobuf序列化协议开发，且支持众多开发语言。gRPC基于以下理念：定义一个服务，指定其能够被远程调用的方法（包含参数和返回类型）。在服务端实现这个接口。并运行一个gRPC服务器来处理客户端调用。在客户端拥有一个存根能够向服务端一样的方法。
###  2:dgiot_grpc_python连接
  ![1.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_web/doc_gjb/grpc/1.png)
### 3:Dgiot客户端
dgiot通过grpc与其他语言相互通信以及函数调用。在dgiot中，以一个grpc通道处理所有交互。而grpc相关函数定义在dgiot_dlink下，分为login,logout,send三个函数。
![1.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_web/doc_gjb/grpc/2.png)
  + login
    login函数用于登录grpc服务端，其参数为ClinetId，为客户端编号。建立grpc通道后，将会在通道启动时以通道id为ClinetId登录python服务器。
  + send
    send函数用于向python客户端发送消息。send函数位于通道的handle_message函数中。在通道接收到消息，并且匹配上Message中内容后即可根据消息内容执行send函数调用python服务。
    send函数以map的格式发送消息，消息在发送前将经过base64编码。
  + logout
   logout函数用于断开与python服务的连接，一般在通道的关闭函数stop中调用。

### 4:Python 程序文件
  python程序示例位于插件\priv\example\python3目录下，其目录结构如下：
  ![1.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_web/doc_gjb/grpc/3.png)

  运行dlink_server文件即可启动python服务，接收信息函数位于Dlink类下SayHello方法。其接收request和context两个参数，参数含义如下：
  + 1.request：接收参数，request下name属性为接收到的信息。
  + 2.context：接收参数，当前的环境

  通过对request.name使用base64解码并转为字典后即可获取原本的信息。
  ![1.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_web/doc_gjb/grpc/4.png)
  接收参数后通过HelloReply的message字段返回一条信息：
  ![1.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_web/doc_gjb/grpc/5.png)
  完整交互结果如下图：
  ![1.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_web/doc_gjb/grpc/6.png)

### 5:gRPC官方文档
  https://grpc.io/docs/
