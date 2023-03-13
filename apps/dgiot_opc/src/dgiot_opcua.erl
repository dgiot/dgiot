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
-module(dgiot_opcua).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").

-export([
    test/0,
    create_certificate/1,
    replace_host/2
]).

test() ->
    {ok, Client} = opcua_client:connect(<<"opc.tcp://127.0.0.1">>),
    opcua_client:read(Client, server, browse_name),
    opcua_client:read(Client, server, [node_id, node_class]),
    _Refs = opcua_client:browse(Client, root, #{
        direction => forward,
        type => 35,
        include_subtypes => true}),
    opcua_client:close(Client).

]).
create_certificate(Name) ->
    Priv = code:priv_dir(opcua),
    Key = Priv ++ "/eopcua.pem",
    Cert = Priv ++ "/eopcua.der",

    Cmd =
        "openssl req -new -x509  -config " ++
        Priv ++ "/cert/example.cert.config -newkey rsa:2048 -keyout " ++
        Key ++ " -nodes -outform der " ++
        "-subj '/CN=" ++ unicode:characters_to_list(Name) ++ "' " ++
        "-out " ++ Cert,

    Out = os:cmd(Cmd),

    Result =
        case {file:read_file(Key), file:read_file(Cert)} of
            {{ok, KeyData}, {ok, CertData}} ->
                {ok, #{key => KeyData, certificate => CertData}};
            _ ->
                {error, {Cmd, Out}}
        end,
    file:delete(Key),
    file:delete(Cert),

    Result.

replace_host(Endpoint, Host) ->
    % Open62541 sometimes returns bad strings in ad[i].discoveryUrls[j].data
    % probably without the null at the end
    E = <<<<C>> || <<C>> <= Endpoint, C >= 33, C =< 127>>,
    % We need to replace host in the original endpoint to be able to connect
    % to the server even if its host name is not resolved to the IP
    re:replace(E, "//.*:", <<"//", Host/binary, ":">>, [{return, binary}]).
