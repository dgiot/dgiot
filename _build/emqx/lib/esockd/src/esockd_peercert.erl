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

-module(esockd_peercert).

-export([subject/1, common_name/1]).

-export_type([peercert/0]).

-opaque(peercert() :: binary() | proplists:proplist()).

-spec(subject(nossl | undefined | peercert()) -> undefined | binary()).
subject(nossl)     -> undefined;
subject(undefined) -> undefined;
subject(Cert) when is_binary(Cert) ->
    esockd_ssl:peer_cert_subject(Cert);
subject(PP2Info) when is_list(PP2Info) ->
    %%Notice: DN is not available in ppv2 additional info
    proplists:get_value(pp2_ssl_cn, PP2Info).

-spec(common_name(nossl | undefined | peercert()) -> undefined | binary()).
common_name(nossl)     -> undefined;
common_name(undefined) -> undefined;
common_name(Cert) when is_binary(Cert) ->
    esockd_ssl:peer_cert_common_name(Cert);
common_name(PP2Info) when is_list(PP2Info) ->
    proplists:get_value(pp2_ssl_cn, PP2Info).

