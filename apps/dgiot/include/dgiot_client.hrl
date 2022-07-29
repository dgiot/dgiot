%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
-author("johnliu").
-define(DCLINET_QUE(Name), dgiot_utils:to_atom(lists:concat([dgiot_utils:to_atom(Name), "_que"]))).
-define(DCLINET_PNQUE(Name), dgiot_utils:to_atom(lists:concat([dgiot_utils:to_atom(Name), "_pnque"]))).
-record(dclock, {
    nexttime :: non_neg_integer(),    %% 下一次闹铃执行时间
    freq :: non_neg_integer(),        %% 周期闹铃提醒频率单位为秒
    count :: non_neg_integer(),       %% 闹铃总计执行多少次
    round :: non_neg_integer()       %% 闹铃总计已执行轮次
}).

-record(dclient, {
    channel :: atom(),              %% 客户端的用户管理通道
    client :: binary(),             %% 客户端地址
    status :: integer(),            %% client的状态值
    clock :: #dclock{},             %% client的闹铃
    userdata :: any(),              %% 用户自定义参数
    child :: any()                  %% 子模块参数
}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dclient的状态字
-define(DCLIENT_SUCCESS, 0).             %  CLIENT运行正常
-define(DCLIENT_INTIALIZED, 1).          %  CLIENT初始化状态
-define(DCLIENT_START_LOGININ, 2).       %  CLIENT开始登录
-define(DCLIENT_LOGIN_SUCCESS, 3).       %  CLIENT登录成功
-define(DCLIENT_START_REGISTER, 4).      %  CLIENT开始注册
-define(DCLIENT_REGISTER_SUCCESS, 5).    %  CLIENT注册成功
