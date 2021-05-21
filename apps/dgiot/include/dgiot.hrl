%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-ifndef(DGIOT_HRL).
-define(DGIOT_HRL, true).

-define(APP, dgiot).

%%--------------------------------------------------------------------
%% Common
%%--------------------------------------------------------------------

-define(Otherwise, true).

%%--------------------------------------------------------------------
%% Banner
%%--------------------------------------------------------------------

-define(ERTS_MINIMUM_REQUIRED, "10.0").

%%--------------------------------------------------------------------
%% Configs
%%--------------------------------------------------------------------

-define(NO_PRIORITY_TABLE, none).

%%--------------------------------------------------------------------
%% Alarm
%%--------------------------------------------------------------------

-record(alarm, {
          id        :: binary(),
          severity  :: notice | warning | error | critical,
          title     :: iolist(),
          summary   :: iolist(),
          %% Timestamp (Unit: millisecond)
          timestamp :: integer() | undefined
        }).

%%--------------------------------------------------------------------
%% Plugin
%%--------------------------------------------------------------------

-record(plugin, {
          name           :: atom(),
          dir            :: string() | undefined,
          descr          :: string(),
          vendor         :: string() | undefined,
          active = false :: boolean(),
          info   = #{}   :: map(),
          type           :: atom()
        }).

%%--------------------------------------------------------------------
%% Command
%%--------------------------------------------------------------------

-record(command, {
          name      :: atom(),
          action    :: atom(),
          args = [] :: list(),
          opts = [] :: list(),
          usage     :: string(),
          descr     :: string()
        }).

-endif.
