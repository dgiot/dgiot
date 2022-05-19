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

-define(GLOBAL_TOPIC, <<"global/dgiot">>).
-define(DCACHE, dgiotdiskcache).
-define(DEFREGISTRY, dgiot_global).
-define(DGIOT_CLIENT(ChannelId), binary_to_atom(<<"dgiot_client_", ChannelId/binary>>)).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

-define(CHILD2(I, Mod, Type, Args), {I, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).
-define(CHILD3(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(SYS_APP, [
    kernel, stdlib, sasl, appmon, eldap, erts,
    syntax_tools, ssl, crypto, mnesia, os_mon,
    inets, lager, runtime_tools, snmp, otp_mibs, public_key,
    asn1, ssh, hipe, common_test, observer, webtool, xmerl, tools,
    test_server, compiler, debugger, eunit, et,
    wx, cowboy, ranch, large, poolboy, jesse, jsx,
    supervisor3, eredis, sqlparse, rfc3339, cuttlefish,
    cowlib, epgsql, erlydtl, websocket_client,esockd,gen_coap,
    gen_rpc,getopt, goldrush, gpb,gproc,gun,jiffy,luerl,lwm2m_coap,
    minirest,mysql,prometheus,recon,rulesql,ssl_verify_fun,
    ekka,ibrowse,grpc,hut,emqx,ehttpc,ejdbc
]).
