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

%% @doc dgiot_geoip
-module(dgiot_geoip).
-include("dgiot_location.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([
    start/1,
    lookup/2
]).

%% EditionIDs:  GeoLite2-ASN GeoLite2-City GeoLite2-Country
%%# GeoIP.conf file for `geoipupdate` program, for versions >= 3.1.1.
%%# Used to update GeoIP databases from https://www.maxmind.com.
%%# For more information about this config file, visit the docs at
%%# https://dev.maxmind.com/geoip/updating-databases?lang=en.
%%
%%# `AccountID` is from your MaxMind account.
%%AccountID 654717
%%
%%# `LicenseKey` is from your MaxMind account
%%LicenseKey rG64ZPJAwU4ZhiJB
%%
%%# `EditionIDs` is from your MaxMind account.
%%EditionIDs GeoLite2-ASN GeoLite2-City GeoLite2-Country
% You can also use:
% * an HTTP(S) URL,
% * or a local path, e.g. "/usr/share/GeoIP/GeoLite2-City.mmdb"
% * or a {custom_fetcher, Module, Args} tuple, with Module
%   implementing the locus_custom_fetcher behaviour.
%% Type : <<"ASN">> || <<"City">> || <<"Country">>
start(Type) ->
    case dgiot_utils:to_binary(application:get_env(dgiot_location, geoip_license, "dgiot_rG61ZPJAwU5ZhiJB")) of
        <<"dgiot_", LicenseKey/binary>> ->
            application:set_env(locus, license_key, dgiot_utils:to_list(LicenseKey)),
            Url = "https://download.maxmind.com/app/geoip_download?edition_id=GeoLite2-"
                ++ dgiot_utils:to_list(Type)
                ++ "&license_key="
                ++ dgiot_utils:to_list(LicenseKey)
                ++ "&suffix=tar.gz",
            AtomType = dgiot_utils:to_atom(string:to_lower(dgiot_utils:to_list(Type))),
            ok = locus:start_loader(AtomType,  Url),
            {ok, _DatabaseVersion} = locus:await_loader(AtomType); % or `{error, Reason}
        _ ->
            pass
    end.

%% dgiot_geoip:lookup(<<"Country">>, "93.184.216.34")
lookup(Type, Ip) ->
    AtomType = dgiot_utils:to_atom(string:to_lower(dgiot_utils:to_list(Type))),
    locus:lookup(AtomType, dgiot_utils:to_list(Ip)).

