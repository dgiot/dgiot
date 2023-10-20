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

-module(dgiot).
-author("johnliu").
-include("dgiot.hrl").
-export([get_attr/3, get_env/1, get_env/2, get_env/3, init_plugins/0, check_dgiot_app/0, child_spec/2, child_spec/3, domain/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
get_env(Key) ->
    case get_env(Key, not_find) of
        not_find -> throw({error, not_find});
        Value -> Value
    end.

get_env(Key, Default) ->
    get_env(?MODULE, Key, Default).

get_env(App, Key, Default) ->
    application:get_env(App, Key, Default).

get_attr(Module, Attr, Fun) ->
    Attributes = Module:module_info(attributes),
    case proplists:get_value(Attr, Attributes) of
        undefined ->
            true;
        [Module | _] ->
            case os:type() of
                {win32, nt} ->
                    true;
                _ ->
                    Attr:Fun(Module)
            end
    end.

init_plugins() ->
    SysApp = lists:foldl(fun(X, Acc) ->
        case X of
            {Plugin, _, _} ->
                BinPlugin = dgiot_utils:to_binary(Plugin),
                case BinPlugin of
                    <<"dgiot_", _>> -> Acc ++ [Plugin];
                    _ -> Acc
                end;
            _ -> Acc
        end
                         end, ?SYS_APP, ekka_boot:all_module_attributes(dgiot_plugin)),
    dgiot_data:insert({dgiot, sys_app}, SysApp).

check_dgiot_app() ->
    lists:foldl(fun({Module, _, _} = App, Acc) ->
        case dgiot_utils:to_binary(Module) of
            <<"dgiot_", _/binary>> ->
                Acc ++ [App];
            <<"dgiot">> ->
                Acc ++ [App];
            _ ->
                Acc
        end
                end, [], application:loaded_applications()).

child_spec(M, Type) ->
    child_spec(M, Type, []).

child_spec(M, worker, Args) ->
    #{
        id => M,
        start => {M, start_link, Args},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [M]
    };

child_spec(M, supervisor, Args) ->
    #{
        id => M,
        start => {M, start_link, Args},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [M]
    }.


domain(Args) ->
    DictId = dgiot_parse_id:get_dictid(<<"dgiotdomain">>, <<"domain">>, <<"domain">>, <<"dgiotdomainconfiguration">>),
    Data =
        case dgiot_parse:get_object(<<"Dict">>, DictId) of
            {ok, Dict} ->
                domain_(Args, Dict);
            _ ->
                Dict = #{
                    <<"objectId">> => DictId,
                    <<"key">> => <<"dgiotdomain">>,
                    <<"type">> => <<"domain">>,
                    <<"class">> => <<"domain">>,
                    <<"title">> => <<"dgiotdomainconfiguration">>,
                    <<"data">> => Args
                },
                dgiot_parse:create_object(<<"Dict">>, Dict),
                domain_(Args, Dict),
                Args
        end,
    {ok, Data#{<<"status">> => 0}}.

%% 获取域名
domain_(#{<<"action">> := <<"getDomainSSL">>}, #{<<"data">> := Data}) ->
    #{<<"data">> => Data};

%% 修改域名
%% sed -ri 's/(server_name )[^*]*/\1 dev.dgiotcloud.com;/' /data/dgiot/nginx/conf/nginx.conf
%% sed -ri 's/("download_domain": ")[^"]*/\1dev.dgiotcloud.com/' /data/dgiot/go_fastdfs/conf/cfg.json
domain_(#{<<"action">> := <<"setDomainSSL">>, <<"domain">> := Domain, <<"key">> := Key, <<"csr">> := Csr} = Args, #{<<"objectId">> := DictId, <<"data">> := Data}) ->
%%    nginx
    os:cmd("sed -ri 's/(server_name )[^*]*/\\1 " ++ dgiot_utils:to_list(Domain) ++ ";/' /data/dgiot/nginx/conf/nginx.conf"),
    os:cmd("sed -ri 's/(ssl_certificate )[^*]*/\\1 \\/etc\\/pki\\/tls\\/certs\\/fullchain.pem;/' /data/dgiot/nginx/conf/nginx.conf"),
    os:cmd("sed -ri 's/(ssl_certificate_key )[^*]*/\\1 \\/etc\\/pki\\/tls\\/certs\\/privkey.key;/' /data/dgiot/nginx/conf/nginx.conf"),
    os:cmd("echo '" ++ dgiot_utils:to_list(Key) ++ "' > /etc/pki/tls/certs/privkey.key"),
    os:cmd("echo '" ++ dgiot_utils:to_list(Csr) ++ "' > /etc/pki/tls/certs/fullchain.pem"),
%%    go_fastdfs
    os:cmd("sed -ri 's/(\"download_domain\": \")[^\"]*/\\1" ++ dgiot_utils:to_list(Domain) ++ "/' /data/dgiot/go_fastdfs/conf/cfg.json"),
    dgiot_parse:update_object(<<"Dict">>, DictId, #{<<"data">> => maps:merge(Data, maps:without([<<"action">>], Args))}),
%%    restart
    os:cmd("systemctl restart nginx"),
    os:cmd("systemctl restart gofastdfs"),
    #{<<"domain">> => Domain, <<"msg">> => <<"已保存"/utf8>>};

domain_(_Args, _) ->
    #{<<"msg">> => <<"success">>}.
