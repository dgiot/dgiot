%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(ekka_guid_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> ekka_ct:all(?MODULE).

t_gen(_) ->
    <<_:128>> = Guid1 = ekka_guid:gen(),
    <<_:128>> = Guid2 = ekka_guid:gen(),
    ?assert(Guid2 > Guid1).

t_new(_) ->
    {Ts1, _NPid, 0} = ekka_guid:new(),
    {Ts2, _NPid, 0} = ekka_guid:new(),
    ?assert(Ts2 > Ts1).

t_timestamp(_) ->
    Ts1 = ekka_guid:timestamp(ekka_guid:gen()),
    Ts2 = ekka_guid:timestamp(ekka_guid:gen()),
    ?assert(Ts2 > Ts1).

t_to_from_hexstr(_) ->
    ?assertEqual(Guid = ekka_guid:gen(), ekka_guid:from_hexstr(ekka_guid:to_hexstr(Guid))).

