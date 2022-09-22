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
%% https://doc.oschina.net/grpc
%% https://www.grpc.io/docs/

-module(dgiot_factory_worker).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("dgiot/include/logger.hrl").



-define(TYPE, ?FACTORY_WOKER).

%% 注册协议参数
-params(#{
    <<"shift">> => #{
        order => 1,
        type => string,
        required => true,
        default => #{<<"value">> => <<"moring">>, <<"label">> => <<"白班"/utf8>>},
        enum => [
            #{<<"value">> => <<"moring">>, <<"label">> => <<"白班"/utf8>>},
            #{<<"value">> => <<"afernoon">>, <<"label">> => <<"晚班"/utf8>>}
        ],
        title => #{
            zh => <<"班次"/utf8>>
        },
        description => #{
            zh => <<"班次"/utf8>>
        }
    },
    <<"workteam">> => #{
        order => 2,
        type => string,
        required => true,
        default => <<"0000"/utf8>>,
        enum => [
            #{<<"value">> => <<"A">>, <<"label">> => <<"A班"/utf8>>},
            #{<<"value">> => <<"B">>, <<"label">> => <<"B班"/utf8>>},
            #{<<"value">> => <<"C">>, <<"label">> => <<"C班"/utf8>>}
        ],
        title => #{
            zh => <<"班组"/utf8>>
        },
        description => #{
            zh => <<"班组"/utf8>>
        }
    },
    <<"operatetype">> => #{
        order => 3,
        type => string,
        required => true,
        default => #{<<"value">> => <<"shift">>, <<"label">> => <<"记录排班"/utf8>>},
        enum => [#{<<"value">> => <<"shift">>, <<"label">> => <<"记录排班"/utf8>>},
            #{<<"value">> => <<"pay">>, <<"label">> => <<"计算工资"/utf8>>}
        ],
        title => #{
            zh => <<"操作类型"/utf8>>
        },
        description => #{
            zh => <<"操作类型"/utf8>>
        }
    },
    <<"address">> => #{
        order => 4,
        type => string,
        required => true,
        default => <<"0X00"/utf8>>,
        title => #{
            zh => <<"寄存器地址"/utf8>>
        },
        description => #{
            zh => <<"寄存器地址:原数据地址(16进制加0X,例如:0X10,否在是10进制);8位寄存器,一个字节;16位寄存器,两个字节;32位寄存器,四个字节"/utf8>>
        }
    },
    <<"registersnumber">> => #{
        order => 5,
        type => string,
        required => true,
        default => <<"1">>,
        title => #{
            zh => <<"寄存器个数"/utf8>>
        },
        description => #{
            zh => <<"寄存器个数(多个寄存器个数)"/utf8>>
        }
    }
}).

start(Server) ->
    Services = #{protos => [dgiot_dlink_pb],
        services => #{'dgiot.Dlink' => dgiot_dlink_server}
    },
    {ok, _} = grpc:start_server(Server, 30051, Services, []).

stop(Server) ->
    _ = grpc:stop_server(Server).

login(ClinetId) ->
    SvrAddr = "http://127.0.0.1:30051",
    {ok, _} = grpc_client_sup:create_channel_pool(ClinetId, SvrAddr, #{}).

logout(ClinetId) ->
    _ = grpc_client_sup:stop_channel_pool(ClinetId).

send(ClinetId) ->
    dgiot_dlink_client:say_hello(#{name => <<"Xiao Ming">>}, #{channel => ClinetId}).

test() ->
    FileName = <<"dlink">>,
    Proto = <<"Ly8gY29weSBmcm9tOiBodHRwczovL2dycGMuaW8vZG9jcy93aGF0LWlzLWdycGMvaW50cm9kdWN0aW9uLwoKCnN5bnRheCA9ICJwcm90bzMiOwoKb3B0aW9uIGphdmFfbXVsdGlwbGVfZmlsZXMgPSB0cnVlOwpvcHRpb24gamF2YV9wYWNrYWdlID0gImlvLmdycGMuZXhhbXBsZXMuZGxpbmsiOwpvcHRpb24gamF2YV9vdXRlcl9jbGFzc25hbWUgPSAiRGxpbmtQcm90byI7Cm9wdGlvbiBvYmpjX2NsYXNzX3ByZWZpeCA9ICJkbGluayI7CgpwYWNrYWdlIGRnaW90OwoKLy8gVGhlIGRsaW5rIHNlcnZpY2UgZGVmaW5pdGlvbi4Kc2VydmljZSBEbGluayB7CiAgLy8gU2VuZHMgYSBncmVldGluZwogIHJwYyBTYXlIZWxsbyAoSGVsbG9SZXF1ZXN0KSByZXR1cm5zIChIZWxsb1JlcGx5KSB7fQoKICAvLyBJZiB0aGUgcmVxdWVzdGVkIHNlcnZpY2UgaXMgdW5rbm93biwgdGhlIGNhbGwgd2lsbCBmYWlsIHdpdGggc3RhdHVzCiAgLy8gTk9UX0ZPVU5ELgogIHJwYyBDaGVjayhIZWFsdGhDaGVja1JlcXVlc3QpIHJldHVybnMgKEhlYWx0aENoZWNrUmVzcG9uc2UpOwoKICAvLyBQZXJmb3JtcyBhIHdhdGNoIGZvciB0aGUgc2VydmluZyBzdGF0dXMgb2YgdGhlIHJlcXVlc3RlZCBzZXJ2aWNlLgogIC8vIFRoZSBzZXJ2ZXIgd2lsbCBpbW1lZGlhdGVseSBzZW5kIGJhY2sgYSBtZXNzYWdlIGluZGljYXRpbmcgdGhlIGN1cnJlbnQKICAvLyBzZXJ2aW5nIHN0YXR1cy4gIEl0IHdpbGwgdGhlbiBzdWJzZXF1ZW50bHkgc2VuZCBhIG5ldyBtZXNzYWdlIHdoZW5ldmVyCiAgLy8gdGhlIHNlcnZpY2UncyBzZXJ2aW5nIHN0YXR1cyBjaGFuZ2VzLgogIC8vCiAgLy8gSWYgdGhlIHJlcXVlc3RlZCBzZXJ2aWNlIGlzIHVua25vd24gd2hlbiB0aGUgY2FsbCBpcyByZWNlaXZlZCwgdGhlCiAgLy8gc2VydmVyIHdpbGwgc2VuZCBhIG1lc3NhZ2Ugc2V0dGluZyB0aGUgc2VydmluZyBzdGF0dXMgdG8KICAvLyBTRVJWSUNFX1VOS05PV04gYnV0IHdpbGwgKm5vdCogdGVybWluYXRlIHRoZSBjYWxsLiAgSWYgYXQgc29tZQogIC8vIGZ1dHVyZSBwb2ludCwgdGhlIHNlcnZpbmcgc3RhdHVzIG9mIHRoZSBzZXJ2aWNlIGJlY29tZXMga25vd24sIHRoZQogIC8vIHNlcnZlciB3aWxsIHNlbmQgYSBuZXcgbWVzc2FnZSB3aXRoIHRoZSBzZXJ2aWNlJ3Mgc2VydmluZyBzdGF0dXMuCiAgLy8KICAvLyBJZiB0aGUgY2FsbCB0ZXJtaW5hdGVzIHdpdGggc3RhdHVzIFVOSU1QTEVNRU5URUQsIHRoZW4gY2xpZW50cwogIC8vIHNob3VsZCBhc3N1bWUgdGhpcyBtZXRob2QgaXMgbm90IHN1cHBvcnRlZCBhbmQgc2hvdWxkIG5vdCByZXRyeSB0aGUKICAvLyBjYWxsLiAgSWYgdGhlIGNhbGwgdGVybWluYXRlcyB3aXRoIGFueSBvdGhlciBzdGF0dXMgKGluY2x1ZGluZyBPSyksCiAgLy8gY2xpZW50cyBzaG91bGQgcmV0cnkgdGhlIGNhbGwgd2l0aCBhcHByb3ByaWF0ZSBleHBvbmVudGlhbCBiYWNrb2ZmLgogIHJwYyBXYXRjaChIZWFsdGhDaGVja1JlcXVlc3QpIHJldHVybnMgKHN0cmVhbSBIZWFsdGhDaGVja1Jlc3BvbnNlKTsKCn0KCi8vIFRoZSByZXF1ZXN0IG1lc3NhZ2UgY29udGFpbmluZyB0aGUgdXNlcidzIG5hbWUuCm1lc3NhZ2UgSGVsbG9SZXF1ZXN0IHsKICBzdHJpbmcgbmFtZSA9IDE7Cn0KCi8vIFRoZSByZXNwb25zZSBtZXNzYWdlIGNvbnRhaW5pbmcgdGhlIGdyZWV0aW5ncwptZXNzYWdlIEhlbGxvUmVwbHkgewogIHN0cmluZyBtZXNzYWdlID0gMTsKfQoKCm1lc3NhZ2UgSGVhbHRoQ2hlY2tSZXF1ZXN0IHsKICBzdHJpbmcgc2VydmljZSA9IDE7Cn0KCm1lc3NhZ2UgSGVhbHRoQ2hlY2tSZXNwb25zZSB7CiAgZW51bSBTZXJ2aW5nU3RhdHVzIHsKICAgIFVOS05PV04gPSAwOwogICAgU0VSVklORyA9IDE7CiAgICBOT1RfU0VSVklORyA9IDI7CiAgICBTRVJWSUNFX1VOS05PV04gPSAzOyAgLy8gVXNlZCBvbmx5IGJ5IHRoZSBXYXRjaCBtZXRob2QuCiAgfQogIFNlcnZpbmdTdGF0dXMgc3RhdHVzID0gMTsKfQ==">>,
    {file, Here} = code:is_loaded(?MODULE),
    ErlangDir = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/example/Erlang/"]),
    Dir = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/example/Erlang/priv/"]),
    Name = dgiot_utils:to_list(FileName),
    ?LOG(info, "~p", [Dir ++ Name ++ ".proto"]),
    ?LOG(info, "~p", [base64:decode(Proto)]),
    case file:open(Name, [raw, write, delayed_write, binary]) of
        {ok, Fd} ->
            Result = file:write_file(Dir ++ Name ++ ".proto", base64:decode(Proto)),
            file:close(Fd),
            ?LOG(info, "~p", [Result]);
        _ -> pass
    end,
    Cmd = "cd " ++ ErlangDir ++ " &&  ./rebar3 compile",
    ?LOG(info, "~p", [Cmd]),
    os:cmd(Cmd),
    ok.

get_all_protocol() ->
    lists:foldl(fun({_, Channel_type}, Acc) ->
        App = maps:get(app, Channel_type),
        Mod = maps:get(mod, Channel_type),
        CType = maps:get(cType, Channel_type),
        Attributes = Mod:module_info(attributes),
        [format_protocol(App, CType, Channel_type, Attributes) | Acc]
                end, [], list()).

format_protocol(App, CType, Channel_type, Attributes) ->
    [Params] = proplists:get_value(params, Attributes, [#{}]),
    Channel_type#{
        cType => CType,
        app => App,
        params => maps:merge(#{
            <<"_dlinkindex">> => #{
                order => 103,
                type => integer,
                required => true,
                default => 0,
                title => #{
                    zh => <<"报文序号"/utf8>>
                },
                description => #{
                    zh => <<"报文序号,初始值为0"/utf8>>
                }
            }
        }, Params)
    }.

list() ->
    Fun =
        fun({App, Vsn, Mod}, Acc) ->
            case code:is_loaded(Mod) == false of
                true ->
                    Acc;
                false ->
                    case [Channel || {protocol_type, [Channel]} <- Mod:module_info(attributes)] of
                        [] ->
                            Acc;
                        [Channel | _] ->
                            [{maps:get(priority, Channel, 255), Channel#{app => App, mod => Mod, vsn => Vsn}}] ++ Acc
                    end
            end
        end,
    lists:sort(dgiot_plugin:check_module(Fun, [])).

get_json(Type) ->
    dgiot_utils:get_JsonFile(?MODULE, <<Type/binary, ".json">>).
