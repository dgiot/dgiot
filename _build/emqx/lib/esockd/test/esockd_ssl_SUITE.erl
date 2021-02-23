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

-module(esockd_ssl_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> esockd_ct:all(?MODULE).

init_per_testcase(_TestCase, Config) ->
    CertFile = esockd_ct:certfile(Config),
    [{cert, pem_decode(CertFile)}|Config].

end_per_testcase(_TestCase, Config) ->
    Config.

pem_decode(CertFile) ->
    {ok, CertBin} = file:read_file(CertFile),
    [{'Certificate', DerCert, _}] = public_key:pem_decode(CertBin),
    DerCert.

cert(Config) -> proplists:get_value(cert, Config).

t_peer_cert_issuer(Config) ->
    esockd_ssl:peer_cert_issuer(cert(Config)).

t_peer_cert_subject_items(Config) ->
    esockd_ssl:peer_cert_subject_items(cert(Config), {0,9,2342,19200300,100,1,1}).

t_peer_cert_validity(Config) ->
    esockd_ssl:peer_cert_validity(cert(Config)).

t_peer_cert_common_name(Config) ->
    esockd_ssl:peer_cert_common_name(cert(Config)).

t_peer_cert_subject(Config) ->
    esockd_ssl:peer_cert_subject(cert(Config)).

