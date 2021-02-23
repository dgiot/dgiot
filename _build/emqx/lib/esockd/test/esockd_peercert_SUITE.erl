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

-module(esockd_peercert_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> esockd_ct:all(?MODULE).

t_subject(Config) ->
    undefined = esockd_peercert:subject(nossl),
    undefined = esockd_peercert:subject(undefined),
    DerCert = pem_decode(esockd_ct:certfile(Config)),
    _Subject = esockd_peercert:subject(DerCert),
    <<"C=CH">> = esockd_peercert:subject([{pp2_ssl_cn, <<"C=CH">>}]).

t_common_name(Config) ->
    undefined = esockd_peercert:common_name(nossl),
    undefined = esockd_peercert:common_name(undefined),
    DerCert = pem_decode(esockd_ct:certfile(Config)),
    _CN = esockd_peercert:common_name(DerCert),
    <<"C=CH">> = esockd_peercert:common_name([{pp2_ssl_cn, <<"C=CH">>}]).

pem_decode(CertFile) ->
    {ok, CertBin} = file:read_file(CertFile),
    [{'Certificate', DerCert, _}] = public_key:pem_decode(CertBin),
    DerCert.

