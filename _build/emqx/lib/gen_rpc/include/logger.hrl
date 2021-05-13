%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

%% debug | info | notice | warning | error | critical | alert | emergency

-define(debug(Format), ?log(debug, Format, [])).
-define(debug(Format, Args), ?log(debug, Format, Args)).

-define(info(Format), ?log(info, Format, [])).
-define(info(Format, Args), ?log(info, Format, Args)).

-define(notice(Format), ?log(notice, Format, [])).
-define(notice(Format, Args), ?log(notice, Format, Args)).

-define(warn(Format), ?log(warning, Format, [])).
-define(warn(Format, Args), ?log(warning, Format, Args)).

-define(error(Format), ?log(error, Format, [])).
-define(error(Format, Args), ?log(error, Format, Args)).

-define(critical(Format), ?log(critical, Format, [])).
-define(critical(Format, Args), ?log(critical, Format, Args)).

-define(alert(Format), ?log(alert, Format, [])).
-define(alert(Format, Args), ?log(alert, Format, Args)).

-define(log(Level, Format), ?log(Level, Format, [])).

-define(log(Level, Format, Args),
        begin
          (logger:log(Level,#{},#{report_cb => fun(_) -> {(Format), (Args)} end}))
        end).
