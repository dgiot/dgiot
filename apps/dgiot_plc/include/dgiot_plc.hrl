%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-define(S7, <<"S7">>).

-define(S7_ERROR_CODE_OK, 0).                           %% 成功
-define(S7_ERROR_CODE_FAILED, 1).                        %% 错误
-define(S7_ERROR_CODE_FW_ERROR, 2).                            %% 发生了异常，具体信息查找Fetch/Write协议文档
-define(S7_ERROR_CODE_ERROR_0006, 3).                        %% 当前操作的数据类型不支持
-define(S7_ERROR_CODE_ERROR_000A, 4).                        %% 尝试读取不存在的DB块数据
-define(S7_ERROR_CODE_WRITE_ERROR, 5).                        %% 写入数据异常
-define(S7_ERROR_CODE_DB_SIZE_TOO_LARGE, 6).                %% DB块数据无法大于255
-define(S7_ERROR_CODE_READ_LENGTH_MAST_BE_EVEN, 7).            %% 读取的数据长度必须为偶数
-define(S7_ERROR_CODE_DATA_LENGTH_CHECK_FAILED, 8).            %% 数据块长度校验失败，请检查是否开启put/get以及关闭db块优化
-define(S7_ERROR_CODE_READ_LENGTH_OVER_PLC_ASSIGN, 9).        %% 读取的数据范围超出了PLC的设定
-define(S7_ERROR_CODE_READ_LENGTH_CANNT_LARAGE_THAN_19, 10).    %% 读取的数组数量不允许大于19
-define(S7_ERROR_CODE_UNKOWN, 99).                            %% 未知错误
