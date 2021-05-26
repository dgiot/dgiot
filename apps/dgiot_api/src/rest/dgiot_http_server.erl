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


-module(dgiot_http_server).
-include("dgiot_api.hrl").
-include_lib("dgiot/include/logger.hrl").
-author("johnliu").
-export([child_spec/2, start_http/3, bind/4, reload_paths/0, reload_paths/2, reload_paths/1]).

-export([get_env/1, get_env/2]).

%% 获取HTTP Server
child_spec(App, Name) ->
    Env = get_env(App),
    get_child_spec(Name, Env).

start_http(Name, Port, DocRoot) ->
    Env = get_env(?APP),
    get_child_spec(Name, Env#{port => Port, docroot => DocRoot}).

reload_paths(Name) ->
    reload_paths(?APP, Name).

%% 重新设置路由表
reload_paths(App, Name) ->
    Env = get_env(App),
    Dispatch = get_routes(Name, Env),
    cowboy:set_env(Name, dispatch, Dispatch).

reload_paths() ->
    reload_paths(?APP, ?WEBSERVER).

save_path(Path, Mod, Type) ->
    case dgiot_swagger:load_schema(Mod, Path, [{labels, binary}, return_maps]) of
        {ok, Schemas} ->
            case Schemas of
                #{<<"paths">> := Paths} ->
                    maps:fold(fun(K, V, Acc) ->
                        case binary:split(K, <<$/>>, [global, trim]) of
                            [<<>>, RestPath | _] ->
                                lists:map(fun(X) ->
                                    dgiot_data:insert({ext, <<X/binary, "_", RestPath/binary>>}, {Mod, Type}),
                                    ?LOG(info,"Id ~p", [<<X/binary, "_", RestPath/binary>>])
                                          end, maps:keys(V));
                            _ -> pass
                        end,
                        Acc
                              end, [], Paths);
                _ -> pass
            end,
            Schemas;
        {error, Reason} ->
            throw({error, Reason})
    end.

%% 动态加入路径
bind(Path, _Mod, _Options, ext) ->
    case dgiot_swagger:load_schema(Path, [{labels, binary}, return_maps], ext) of
        {ok, Schemas} ->
            Schemas;
        {error, Reason} ->
            throw({error, Reason})
    end;

bind(Path, Mod, python, priv) ->
    save_path(Path, Mod, python);

bind(Path, Mod, java, priv) ->
    save_path(Path, Mod, java);

bind(Path, Mod, ruby, priv) ->
    save_path(Path, Mod, ruby);

bind(Path, Mod, _Options, priv) ->
    case dgiot_swagger:load_schema(Mod, Path, [{labels, binary}, return_maps]) of
        {ok, Schemas} ->
            Schemas;
        {error, Reason} ->
            throw({error, Reason})
    end;
bind(Path, _Mod, Options, MetaData) ->
    #{
        <<"definitions">> => proplists:get_value(<<"definitions">>, Options, #{}),
        <<"paths">> => #{
            Path => MetaData
        }
    }.


-spec get_child_spec(Name :: atom(), #{port => inet:port_number(), acceptors => integer(), docroot => list()}) -> {ok, pid()} | {error, any()}.
get_child_spec(Name, #{
    port := Port,
    acceptors := Acceptors
} = Env) ->
    SSL = maps:with([cacertfile, certfile, keyfile], Env),
    IP = {0, 0, 0, 0},
    NetOpts = [{ssl, maps:to_list(SSL)}],
    {Transport, TransportOpts} = get_transport(IP, Port, NetOpts),
    ExtraOpts = maps:get(cowboy_extra_opts, Env, []),
    DefaultOpts = #{
        env => #{
            dispatch => get_routes(Name, Env)
        }
    },
    Opts = get_config(ExtraOpts, DefaultOpts),
    dgiot_data:insert({Port, httpd}, #{name => Name}),
    ?LOG(error, "~n Start ~p listener on ~p successfully. ~n",[Name, Port]),
    ranch:child_spec(Name, Acceptors, Transport, TransportOpts, cowboy_clear, Opts).


get_transport(IP, Port, Options) ->
    Opts = [
        {ip, IP},
        {port, Port}
    ],
    case proplists:get_value(ssl, Options, []) of
        [] ->
            {ranch_tcp, Opts};
        SslOpts = [_ | _] ->
            {ranch_ssl, Opts ++ SslOpts}
    end.


get_config([], Opts) ->
    Opts;
get_config([{env, Env} | Rest], Opts) ->
    OldEnv = maps:get(env, Opts, #{}),
    OldDispatch = maps:get(dispatch, OldEnv, []),
    NewDisPatch = maps:get(dispatch, Env, []),
    DisPatch = lists:concat([OldDispatch, NewDisPatch]),
    NewEnv = maps:merge(OldEnv, maps:without([dispatch], Env)),
    get_config(Rest, Opts#{env => NewEnv#{dispatch => DisPatch}});
get_config([{Key, Value} | Rest], Opts) ->
    get_config(Rest, Opts#{Key => Value}).


get_routes(Name, #{docroot := DocRoot}) ->
    Dispatch = dgiot_router:get_paths(Name, DocRoot),
    cowboy_router:compile([{'_', Dispatch}]).


get_env(App) ->
    lists:foldl(
        fun(Key, Acc) ->
            case get_env(App, Key) of
                undefined -> Acc;
                Value -> Acc#{Key => Value}
            end
        end, #{}, [port, docroot, acceptors, cacertfile, certfile, keyfile]).
get_env(App, Key) when Key == docroot; Key == cacertfile; Key == certfile; Key == keyfile ->
    case get_env_value(App, Key) of
            "priv/" ++ _ = Path ->
            {file, Here} = code:is_loaded(?MODULE),
            Dir = filename:dirname(filename:dirname(Here)),
            filename:join([Dir, Path]);
        Path ->
            Path
    end;
get_env(App, Key) ->
    get_env_value(App, Key).


get_env_value(App, Key) ->
    case application:get_env(App, Key) of
        undefined -> undefined;
        {ok, ""} -> undefined;
        {ok, <<>>} -> undefined;
        {ok, Value} -> Value
    end.
