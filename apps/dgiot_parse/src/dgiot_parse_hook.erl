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

-module(dgiot_parse_hook).
-author("kenneth").
-include("dgiot_parse.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(DEFField, re:split(application:get_env(?MODULE, delete_field, ""), ",")).


%% API
-export([
    do_request_hook/6,
    subscribe/3,
    publish/4,
    publish/5,
    add_trigger/3,
    del_trigger/2,
    del_trigger/1,
    del_trigger/0,
    update_trigger/3,
    get_trigger/0,
    get_trigger/2,
    add_all_trigger/1,
    do_hook/2,
    get_count/2
]).

get_count(ClassName, Acls) ->
    case catch dgiot_hook:run_hook({'parse_get_count', ClassName}, [Acls]) of
        {'EXIT', Reason} ->
            {error, Reason};
        {error, not_find} ->
            dgiot_parse_cache:get_count(ClassName, Acls, roleid);
        {ok, []} ->
            ignore;
        {ok, [{error, Reason} | _]} ->
            {error, Reason};
        {ok, [Rtn | _]} ->
            Rtn
    end.

subscribe(Table, Method, Channel) ->
    case dgiot_data:get({sub, Table, Method}) of
        not_find ->
            dgiot_data:insert({sub, Table, Method}, [Channel]);
        Acc ->
            dgiot_data:insert({sub, Table, Method}, dgiot_utils:unique_2(Acc ++ [Channel]))
    end,
    Fun = fun(Args) ->
        case Args of
            ['after', BeforData, AfterData, _Body] ->
                publish(Table, Method, BeforData, AfterData),
                {ok, AfterData};
            ['after', BeforData, AfterData, ObjectId, _Body] ->
                publish(Table, Method, BeforData, AfterData, ObjectId),
                {ok, AfterData};
            _ ->
                {ok, []}
        end
          end,
    dgiot_hook:add(one_for_one, {Table, Method}, Fun).

publish(Table, Method, BeforData, AfterData) ->
    lists:map(fun(ChannelId) ->
        dgiot_channelx:do_message(ChannelId, {sync_parse, Method, Table, dgiot_utils:to_map(BeforData), dgiot_utils:to_map(AfterData)})
              end, dgiot_data:get({sub, Table, Method})).

publish(Table, Method, BeforData, AfterData, ObjectId) ->
    lists:map(fun(ChannelId) ->
        dgiot_channelx:do_message(ChannelId, {sync_parse, Method, Table, dgiot_utils:to_map(BeforData), dgiot_utils:to_map(AfterData), ObjectId})
              end, dgiot_data:get({sub, Table, Method})).

do_request_hook(Type, [<<"classes">>, Class, ObjectId], Method, BeforData, AfterData, Body) ->
    do_hook({<<Class/binary, "/*">>, Method}, [Type, BeforData, AfterData, ObjectId, Body]);
do_request_hook(Type, [<<"classes">>, Class], Method, BeforData, AfterData, Body) ->
    do_hook({Class, Method}, [Type, BeforData, AfterData, Body]);
do_request_hook(_Type, _Paths, _Method, _BeforData, _AfterData, _Body) ->
    ignore.
do_hook(Key, Args) ->
    case catch dgiot_hook:run_hook(Key, Args) of
        {'EXIT', Reason} ->
            {error, Reason};
        {error, not_find} ->
            ignore;
        {ok, []} ->
            ignore;
        {ok, [{error, Reason} | _]} ->
            {error, Reason};
        {ok, [Rtn | _]} ->
            Rtn
    end.

%% 创建触发器
add_trigger(Class, TriggerName, Url) ->
    add_trigger(?DEFAULT, Class, TriggerName, Url).
add_trigger(Name, Class, TriggerName, Url) ->
    true = lists:member(TriggerName, [<<"beforeSave">>, <<"beforeDelete">>, <<"afterSave">>, <<"afterDelete">>]),
    Path = <<"/hooks/triggers">>,
    Body = #{
        <<"className">> => Class,
        <<"triggerName">> => TriggerName,
        <<"url">> => Url
    },
    dgiot_parse:request_rest(Name, 'POST', [], Path, Body, [{from, master}]).

%% 获取触发器
get_trigger() ->
    get_trigger(?DEFAULT).
get_trigger(Name) ->
    Path = <<"/hooks/triggers">>,
    dgiot_parse:request_rest(Name, 'GET', [], Path, #{}, [{from, master}]).
get_trigger(Class, TriggerName) ->
    get_trigger(?DEFAULT, Class, TriggerName).
get_trigger(Name, Class, TriggerName) ->
    Path = <<"/hooks/triggers/", Class/binary, "/", TriggerName/binary>>,
    dgiot_parse:request_rest(Name, 'GET', [], Path, #{}, [{from, master}]).


%% 更新触发器
update_trigger(Class, TriggerName, Url) ->
    update_trigger(?DEFAULT, Class, TriggerName, Url).
update_trigger(Name, Class, TriggerName, Url) ->
    Path = <<"/hooks/triggers/", Class/binary, "/", TriggerName/binary>>,
    Body = #{<<"url">> => Url},
    dgiot_parse:request_rest(Name, 'PUT', [], Path, Body, [{from, master}]).


%% 删除触发器
del_trigger() ->
    case get_trigger() of
        {ok, Results} ->
            Fun =
                fun(#{<<"className">> := Class}) ->
                    del_trigger(Class)
                end,
            lists:foreach(Fun, Results);
        {error, Reason} ->
            {error, Reason}
    end.

del_trigger(Class) ->
    del_trigger(?DEFAULT, Class).
del_trigger(Name, Class) ->
    lists:foreach(
        fun(TriggerName) ->
            del_trigger(Name, Class, TriggerName)
        end, [<<"beforeSave">>, <<"beforeDelete">>, <<"afterSave">>, <<"afterDelete">>]).
del_trigger(Name, Class, TriggerName) ->
    Path = <<"/hooks/triggers/", Class/binary, "/", TriggerName/binary>>,
    Body = #{<<"__op">> => <<"Delete">>},
    dgiot_parse:request_rest(Name, 'PUT', [], Path, Body, [{from, master}]).


add_all_trigger(Host) ->
    add_all_trigger(?DEFAULT, Host).

add_all_trigger(Name, Host) ->
    case get_trigger(Name) of
        {ok, Triggers} ->
            NTrig = lists:foldl(
                fun(Trigger, Acc) ->
                    ClassName = maps:get(<<"className">>, Trigger),
                    TriggerName = maps:get(<<"triggerName">>, Trigger),
                    Url = maps:get(<<"url">>, Trigger),
                    Acc#{<<ClassName/binary, "/", TriggerName/binary>> => Url}
                end, #{}, Triggers),
            case dgiot_parse:get_schemas(Name, <<>>) of
                {ok, #{<<"results">> := Results}} ->
                    Fun =
                        fun
                            (#{<<"className">> := Class}, Acc) when Class == <<"_Session">> ->
                                Acc;
                            (#{<<"className">> := Class}, Acc) ->
                                lists:foldl(
                                    fun(TriggerName, Acc1) ->
                                        Path = <<Host/binary, "/hooks/parse_trigger/do?class=", Class/binary, "&name=", TriggerName/binary>>,
                                        Key = <<Class/binary, "/", TriggerName/binary>>,
                                        case maps:get(Key, NTrig, undefined) of
                                            Path ->
                                                Acc1;
                                            _ ->
                                                case add_trigger(Name, Class, TriggerName, Path) of
                                                    {ok, _} ->
                                                        [{Key, success} | Acc1];
                                                    {error, Reason} ->
                                                        ?LOG(error, "~p,~p~n", [Key, Reason]),
                                                        [Reason | Acc1]
                                                end
                                        end
                                    end, Acc, [<<"beforeSave">>, <<"beforeDelete">>, <<"afterSave">>, <<"afterDelete">>])
                        end,
                    lists:foldl(Fun, [], Results)
            end;
        {error, Reason} ->
            {error, Reason}
    end.
