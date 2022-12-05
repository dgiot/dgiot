-define(DGIOT_IEC104_TCP_DTU, dgiot_iec104_tcp_dtu).
-record(state, {
    id,
    devaddr = <<>>,
    len = 0,
    app = <<>>,
    product = <<>>,
    deviceId = <<>>,
    env = <<>>,
    devtype = <<>>
}).


%%设置超时时间及重发次数
-define(SetTimeOutReSendTimes, 1000).
%%提取现场机时间
-define(GetSceneDeviceTime, 1011).
%%上传现场机时间
-define(UploadSceneDeviceTime, 1011).
%%设置现场机时间
-define(SetSceneDeviceTime, 1012).
%%现场机时间校准请求
-define(SceneDeviceTimeCalibration, 1013).
%%提取实时数据间隔
-define(GetRtdDataInterval, 1061).
%%上传实时数据间隔
-define(UploadRtdDataInterval, 1061).
%%设置实时数据间隔
-define(SetRtdDataInterval, 1062).
%%提取分钟数据间隔
-define(GetMinuteDataInterval, 1063).
%%上传分钟数据间隔
-define(UploadMinuteDataInterval, 1063).
%%设置分钟数据间隔
-define(SetMinuteDataInterval, 1064).
%%用于设置现场机的密码
-define(SetSceneDevicePassword, 1072).
%%取污染物实时数据
-define(GetRtdData, 2011).
%%上传污染物实时数据
-define(UploadRtdData, 2011).
%%停止察看污染物实时数据
-define(StopRtdData, 2012).
%%取设备运行状态数据
-define(GetDeviceRunState, 2021).
%%上传设备运行状态数据
-define(UploadDeviceRunState, 2021).
%%停止察看设备运行状态
-define(StopDeviceRunState, 2022).
%%取污染物日历史数据
-define(GetDayData, 2031).
%%上传污染物日历史数据
-define(UploadDayData, 2031).
%%取设备运行时间日历史数据
-define(GetDeviceRunTimeDayData, 2041).
%%上传设备运行时间日历史数据
-define(UploadDeviceRunTimeDayData, 2041).
%%取污染物分钟数据
-define(GetMinuteData, 2051).
%%上传污染物分钟数据
-define(UploadMinuteData, 2051).
%%取污染物小时数据
-define(GetHourData, 2061).
%%上传污染物小时数据
-define(UploadHourData, 2061).
%%上传数采仪开机时间
-define(UploadComputerPowerOnTime, 2081).
%%零点校准量程校准
-define(RangeCalibration, 3011).
%%即时采样
-define(TakeSampleImmediately, 3012).
%%启动清洗/反吹
-define(StartClear, 3013).
%%比对采样
-define(CompareSample, 3014).
%%超标留样
-define(LeaveSuperstandardSample, 3015).
%%上传超标留样信息
-define(UploadSuperstandardSample, 3015).
%%设置采样时间周期
-define(SetSampleTimeInterval, 3016).
%%提取采样时间周期
-define(GetSampleTimeInterval, 3017).
%%上传采样时间周期
-define(UploadSampleTimeInterval, 3017).
%%提取出样时间
-define(GetSampleTime, 3018).
%%上传出样时间
-define(UploadSampleTime, 3018).
%%提取设备唯一标识
-define(GetSceneDeviceUUID, 3019).
%%上传设备唯一标识
-define(UploadSceneDeviceUUID, 3019).
%%提取现场机信息
-define(GetSceneDeviceInfo, 3020).
%%上传现场机信息
-define(UploadSceneDeviceInfo, 3020).
%%设置现场机参数
-define(SetSceneDeviceParam, 3021).
%%取污染物周期数据
-define(GetCycleData, 8051).
%%上传污染物周期数据
-define(UploadCycleData, 8051).
%%请求应答
-define(RequestResponse, 9011).
%%执行结果
-define(ExecuteResponse, 9012).
%%通知应答
-define(NoticeResponse, 9013).
%%数据应答
-define(DataResponse, 9014).
%%心跳
-define(HeartBeat, 9021).


%%commandType
-define(Request, 1).  %%请求命令
-define(Upload, 2).  %% 上传命令
-define(Notice, 3).  %% 通知命令
-define(Other, 4). %% 其他
-define(None, 5).  %%不支持

-define(IEC104, <<"iec104">>).
-define(IEC104_ETS, iec104_ets).
