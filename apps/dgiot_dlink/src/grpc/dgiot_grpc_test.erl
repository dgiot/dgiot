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

-module(dgiot_grpc_test).

-compile(export_all).
-compile(nowarn_export_all).

start() ->
    Services = #{
        protos => [dgiot_dlink_pb],
        services => #{'dgiot.Dlink' => dgiot_grpc_server}
    },
    {ok, _} = grpc:start_server(server, 30051, Services, []).

stop() ->
    _ = grpc:stop_server(server).

%%https://grpc.io/docs/languages/php/basics/
login() ->
    SvrAddr = "tcp://127.0.0.1:30051",
    {ok, _} = grpc_client_sup:create_channel_pool(<<"channel">>, SvrAddr, #{}).

%%start_client_channel() ->
%%    ClientOps = #{},
%%    SvrAddr = "http://127.0.0.1:10000",
%%    {ok, _} = grpc_client_sup:create_channel_pool(
%%        ?CHANN_NAME,
%%        SvrAddr,
%%        ClientOps
%%    ),
%%    io:format("Start client channel ~s for ~s successfully!~n~n"
%%    "Call the 'routeguide_route_guide_client' module exported functions "
%%    "to use it. e.g:~n"
%%    "  routeguide_route_guide_client:get_feature(#{latitude => 1"
%%    ", longitude => 1}, #{channel => channel1}).~n",
%%        [?CHANN_NAME, SvrAddr]).

logout() ->
    _ = grpc_client_sup:stop_channel_pool(<<"channel">>).

send() ->
    Result = dgiot_dlink_client:say_hello(#{name => <<"Xiao Ming">>}, #{channel => <<"channel">>}),
    io:format("Result ~p ~n", [Result]).

test() ->
    FileName = <<"dlink">>,
    Proto = <<"Ly8gY29weSBmcm9tOiBodHRwczovL2dycGMuaW8vZG9jcy93aGF0LWlzLWdycGMvaW50cm9kdWN0aW9uLwoKCnN5bnRheCA9ICJwcm90bzMiOwoKb3B0aW9uIGphdmFfbXVsdGlwbGVfZmlsZXMgPSB0cnVlOwpvcHRpb24gamF2YV9wYWNrYWdlID0gImlvLmdycGMuZXhhbXBsZXMuZGxpbmsiOwpvcHRpb24gamF2YV9vdXRlcl9jbGFzc25hbWUgPSAiRGxpbmtQcm90byI7Cm9wdGlvbiBvYmpjX2NsYXNzX3ByZWZpeCA9ICJkbGluayI7CgpwYWNrYWdlIGRnaW90OwoKLy8gVGhlIGRsaW5rIHNlcnZpY2UgZGVmaW5pdGlvbi4Kc2VydmljZSBEbGluayB7CiAgLy8gU2VuZHMgYSBncmVldGluZwogIHJwYyBTYXlIZWxsbyAoSGVsbG9SZXF1ZXN0KSByZXR1cm5zIChIZWxsb1JlcGx5KSB7fQoKICAvLyBJZiB0aGUgcmVxdWVzdGVkIHNlcnZpY2UgaXMgdW5rbm93biwgdGhlIGNhbGwgd2lsbCBmYWlsIHdpdGggc3RhdHVzCiAgLy8gTk9UX0ZPVU5ELgogIHJwYyBDaGVjayhIZWFsdGhDaGVja1JlcXVlc3QpIHJldHVybnMgKEhlYWx0aENoZWNrUmVzcG9uc2UpOwoKICAvLyBQZXJmb3JtcyBhIHdhdGNoIGZvciB0aGUgc2VydmluZyBzdGF0dXMgb2YgdGhlIHJlcXVlc3RlZCBzZXJ2aWNlLgogIC8vIFRoZSBzZXJ2ZXIgd2lsbCBpbW1lZGlhdGVseSBzZW5kIGJhY2sgYSBtZXNzYWdlIGluZGljYXRpbmcgdGhlIGN1cnJlbnQKICAvLyBzZXJ2aW5nIHN0YXR1cy4gIEl0IHdpbGwgdGhlbiBzdWJzZXF1ZW50bHkgc2VuZCBhIG5ldyBtZXNzYWdlIHdoZW5ldmVyCiAgLy8gdGhlIHNlcnZpY2UncyBzZXJ2aW5nIHN0YXR1cyBjaGFuZ2VzLgogIC8vCiAgLy8gSWYgdGhlIHJlcXVlc3RlZCBzZXJ2aWNlIGlzIHVua25vd24gd2hlbiB0aGUgY2FsbCBpcyByZWNlaXZlZCwgdGhlCiAgLy8gc2VydmVyIHdpbGwgc2VuZCBhIG1lc3NhZ2Ugc2V0dGluZyB0aGUgc2VydmluZyBzdGF0dXMgdG8KICAvLyBTRVJWSUNFX1VOS05PV04gYnV0IHdpbGwgKm5vdCogdGVybWluYXRlIHRoZSBjYWxsLiAgSWYgYXQgc29tZQogIC8vIGZ1dHVyZSBwb2ludCwgdGhlIHNlcnZpbmcgc3RhdHVzIG9mIHRoZSBzZXJ2aWNlIGJlY29tZXMga25vd24sIHRoZQogIC8vIHNlcnZlciB3aWxsIHNlbmQgYSBuZXcgbWVzc2FnZSB3aXRoIHRoZSBzZXJ2aWNlJ3Mgc2VydmluZyBzdGF0dXMuCiAgLy8KICAvLyBJZiB0aGUgY2FsbCB0ZXJtaW5hdGVzIHdpdGggc3RhdHVzIFVOSU1QTEVNRU5URUQsIHRoZW4gY2xpZW50cwogIC8vIHNob3VsZCBhc3N1bWUgdGhpcyBtZXRob2QgaXMgbm90IHN1cHBvcnRlZCBhbmQgc2hvdWxkIG5vdCByZXRyeSB0aGUKICAvLyBjYWxsLiAgSWYgdGhlIGNhbGwgdGVybWluYXRlcyB3aXRoIGFueSBvdGhlciBzdGF0dXMgKGluY2x1ZGluZyBPSyksCiAgLy8gY2xpZW50cyBzaG91bGQgcmV0cnkgdGhlIGNhbGwgd2l0aCBhcHByb3ByaWF0ZSBleHBvbmVudGlhbCBiYWNrb2ZmLgogIHJwYyBXYXRjaChIZWFsdGhDaGVja1JlcXVlc3QpIHJldHVybnMgKHN0cmVhbSBIZWFsdGhDaGVja1Jlc3BvbnNlKTsKCn0KCi8vIFRoZSByZXF1ZXN0IG1lc3NhZ2UgY29udGFpbmluZyB0aGUgdXNlcidzIG5hbWUuCm1lc3NhZ2UgSGVsbG9SZXF1ZXN0IHsKICBzdHJpbmcgbmFtZSA9IDE7Cn0KCi8vIFRoZSByZXNwb25zZSBtZXNzYWdlIGNvbnRhaW5pbmcgdGhlIGdyZWV0aW5ncwptZXNzYWdlIEhlbGxvUmVwbHkgewogIHN0cmluZyBtZXNzYWdlID0gMTsKfQoKCm1lc3NhZ2UgSGVhbHRoQ2hlY2tSZXF1ZXN0IHsKICBzdHJpbmcgc2VydmljZSA9IDE7Cn0KCm1lc3NhZ2UgSGVhbHRoQ2hlY2tSZXNwb25zZSB7CiAgZW51bSBTZXJ2aW5nU3RhdHVzIHsKICAgIFVOS05PV04gPSAwOwogICAgU0VSVklORyA9IDE7CiAgICBOT1RfU0VSVklORyA9IDI7CiAgICBTRVJWSUNFX1VOS05PV04gPSAzOyAgLy8gVXNlZCBvbmx5IGJ5IHRoZSBXYXRjaCBtZXRob2QuCiAgfQogIFNlcnZpbmdTdGF0dXMgc3RhdHVzID0gMTsKfQ==">>,
    {file, Here} = code:is_loaded(?MODULE),
    ErlangDir = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/example/Erlang/"]),
    Dir = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/example/Erlang/priv/"]),
    Name = dgiot_utils:to_list(FileName),
    io:format(" ~s ~p file =  ~p ~n", [?FILE, ?LINE, Dir ++ Name ++ ".proto"]),
    io:format(" ~s ~p Proto =  ~p ~n", [?FILE, ?LINE, base64:decode(Proto)]),
    case file:open(Name, [raw, write, delayed_write, binary]) of
        {ok, Fd} ->
            Result = file:write_file(Dir ++ Name ++ ".proto", base64:decode(Proto)),
            file:close(Fd),
            grpcx_client:compile(Dir ++ Name ++ ".proto", base64:decode(Proto)),
            io:format(" ~s ~p Result =  ~p ~n", [?FILE, ?LINE, Result]);
        _ -> pass
    end,
    Cmd = "cd " ++ ErlangDir ++ " &&  ./rebar3 compile",
    io:format(" ~s ~p Cmd = ~p ~n", [?FILE, ?LINE, Cmd]),
    os:cmd(Cmd),
    ok.
