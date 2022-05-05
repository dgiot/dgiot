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
    subscribe/4,
    publish/2,
    add_trigger/3,
    del_trigger/2,
    del_trigger/1,
    del_trigger/0,
    update_trigger/3,
    get_trigger/0,
    get_trigger/2,
    add_all_trigger/1,
    do_hook/2,
    notify/5,
    api_hook/1
]).

subscribe(Table, Method, Channel) ->
    subscribe(Table, Method, Channel, [<<"*">>]).

subscribe(Table, Method, Channel, Keys) ->
    NewKeys =
        case Method of
            put -> Keys;
            _ -> [<<"*">>]
        end,
    case dgiot_data:get({sub, Table, Method}) of
        not_find ->
            dgiot_data:insert({sub, Table, Method}, [{Channel, NewKeys}]);
        Acc ->
            dgiot_data:insert({sub, Table, Method}, dgiot_utils:unique_2(Acc ++ [{Channel, NewKeys}]))
    end,
    add_hook({Table, Method}).

add_hook(Key) ->
    Fun =
        fun
            ({'after', get, Token, Class, _QueryData, ResBody}) ->
                notify('after', get, Token, Class, dgiot_utils:to_map(ResBody)),
                receive_ack(ResBody);
            ({'after', get, Token, Class, _ObjectId, _QueryData, ResBody}) ->
                notify('after', get, Token, Class, dgiot_utils:to_map(ResBody)),
                receive_ack(ResBody);
            ({'after', post, Token, Class, QueryData, ResBody}) ->
                notify('after', post, Token, Class, dgiot_utils:to_map(QueryData)),
                {ok, ResBody};
            ({'after', put, Token, Class, ObjectId, QueryData, ResBody}) ->
                Map = dgiot_utils:to_map(QueryData),
                notify('after', put, Token, Class, Map#{<<"objectId">> => ObjectId}),
                {ok, ResBody};
            ({'after', delete, Token, Class, ObjectId, _QueryData, ResBody}) ->
                notify('after', delete, Token, Class, ObjectId),
                {ok, ResBody};
            (_) ->
                {ok, []}
        end,
    dgiot_hook:add(one_for_one, Key, Fun).

publish(Pid, Payload) ->
    Pid ! {sync_parse, Payload}.

%% 同步等待消息处理
receive_ack(ResBody) ->
    receive
        {sync_parse, NewResBody} when is_map(NewResBody) ->
%%            io:format("~s ~p ~p  ~n", [?FILE, ?LINE, length(maps:to_list(NewResBody))]),
            {ok, jsx:encode(NewResBody)};
        {sync_parse, NewResBody} ->
%%            io:format("~s ~p ~p  ~n", [?FILE, ?LINE, NewResBody]),
            {ok, NewResBody};
        {error} ->
            {ok, ResBody}
    after 5000 ->  %% 5秒消息没有响应则用原响应报文返回
        {ok, ResBody}
    end.

notify(Type, Method, Token, Class, Data) ->
    case dgiot_data:get({sub, Class, Method}) of
        not_find ->
            pass;
        List ->
            lists:map(
                fun
                    ({ChannelId, [<<"*">>]}) ->
                        dgiot_channelx:do_message(ChannelId, {sync_parse, self(), Type, Method, Token, Class, Data});
                    ({ChannelId, Keys}) when Method == put ->
                        List = maps:keys(Data),
                        case List -- Keys of
                            List ->
                                pass;
                            _ ->
                                dgiot_channelx:do_message(ChannelId, {sync_parse, self(), Type, Method, Token, Class, Data})
                        end
                end, List)
    end.

do_request_hook(Type, [<<"classes">>, Class, ObjectId], Method, Token, QueryData, ResBody) ->
    do_hook({<<Class/binary, "/*">>, Method}, {Type, Method, Token, Class, ObjectId, QueryData, ResBody});
do_request_hook(Type, [<<"classes">>, Class], Method, Token, QueryData, ResBody) ->
    do_hook({Class, Method}, {Type, Method, Token, Class, QueryData, ResBody});
%% 批处理只做异步通知，不做同步hook
do_request_hook(Type, [<<"batch">>], _Method, Token, QueryData, #{<<"requests">> := Requests} = ResBody) ->
    lists:map(fun(Request) ->
        SubMethod = maps:get(<<"method">>, Request, <<"post">>),
        Path = maps:get(<<"path">>, Request, <<"">>),
        Body = maps:get(<<"body">>, Request, #{}),
        {match, PathList} = re:run(Path, <<"([^/]+)">>, [global, {capture, all_but_first, binary}]),
        do_request_hook(Type, lists:concat(PathList), dgiot_parse_rest:method(SubMethod), Token, QueryData, Body)
              end, Requests),
    {ok, ResBody};
do_request_hook(_Type, _Paths, _Method, _Token, _QueryData, _ResBody) ->
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

api_hook({'before', OperationID, Token, QS, Path, Args}) ->
    {Method, Type, Id} = get_id(OperationID),
    NewArgs = do_put(Method, Token, Path, Args),
    NewQs = case dgiot_hook:run_hook({Method, Type}, {'before', Id, Args}) of
                {ok, [Rtn | _]} ->
                    dgiot_parse:get_qs(maps:without([<<"id">>], Rtn));
                _ ->
                    QS
            end,
    {NewQs, Type, NewArgs};

api_hook({'after', OperationID, Map, ResBody}) ->
    [Method, Type | _] = re:split(OperationID, <<"_">>),
    case dgiot_hook:run_hook({Method, Type}, {'after', Map}) of
        {ok, [Rtn | _]} when is_map(Rtn) ->
            jsx:encode(Rtn);
        _ ->
            ResBody
    end.

get_id(OperationID) ->
    %%    <<"get_classes_product">>,
    case re:split(OperationID, <<"_">>, [{return, binary}]) of
        [Method, Type, _Table, Id | _] ->
            {Method, Type, Id};
        [Method, Type | _] ->
            {Method, Type, '*'}
    end.

%% todo 可以做多级json的 merge修改
do_put(<<"put">>, Token, <<"/iotapi/classes/", Tail/binary>>, #{<<"id">> := Id} = Args) ->
    [ClassName | _] = re:split(Tail, <<"/">>),
    notify('before', put, Token, ClassName, Args),
    case dgiot_parse:get_object(ClassName, Id) of
        {ok, Class} ->
            maps:fold(fun(K, V, Acc) ->
                case maps:find(K, Class) of
                    error -> Acc;
                    {ok, Value} when is_map(Value) ->
                        Acc#{K => maps:merge(Value, V)};
                    _ ->
                        Acc#{K => V}
                end
                      end,
                #{}, maps:without([<<"id">>], Args));
        _ ->
            Args
    end;

do_put(_, _Token, _ClassName, Args) ->
    Args.