%%--------------------------------------------------------------------
%% Copyright (c) 2018-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_tracer).

-export([
    add_trace/1,
    del_trace/1,
    check_trace/3,
    check_trace/5
]).

%% Mnesia bootstrap
-export([mnesia/1]).
-define(DGIOT_CLIENT_TRACE, dgiot_client_trace).
-define(DGIOT_TOPIC_TRACE, dgiot_topic_trace).
-record(dgiot_client_trace, {key :: binary(), value}).
-record(dgiot_topic_trace, {key :: binary(), value}).

-boot_mnesia({mnesia, [boot]}).
-copy_mnesia({mnesia, [copy]}).

%% APIs
%% @doc Create or replicate topics table.
-spec(mnesia(boot | copy) -> ok).
mnesia(boot) ->
    %% Optimize storage
    StoreProps = [{ets, [{read_concurrency, true},
        {write_concurrency, true}
    ]}],
    ok = ekka_mnesia:create_table(?DGIOT_TOPIC_TRACE, [
        {ram_copies, [node()]},
        {record_name, ?DGIOT_TOPIC_TRACE},
        {attributes, record_info(fields, ?DGIOT_TOPIC_TRACE)},
        {type, ordered_set},
        {storage_properties, StoreProps}]),
    ok = ekka_mnesia:create_table(?DGIOT_CLIENT_TRACE, [
        {ram_copies, [node()]},
        {record_name, ?DGIOT_CLIENT_TRACE},
        {attributes, record_info(fields, ?DGIOT_CLIENT_TRACE)},
        {type, ordered_set},
        {storage_properties, StoreProps}]);
mnesia(copy) ->
    %% Copy topics table
    ok = ekka_mnesia:copy_table(?DGIOT_TOPIC_TRACE, ram_copies),
    ok = ekka_mnesia:copy_table(?DGIOT_CLIENT_TRACE, ram_copies).

add_trace({Type, Id}) when is_list(Id) ->
    add_trace({Type, list_to_binary(Id)});
add_trace({Type, Id}) when is_atom(Id) ->
    add_trace({Type, atom_to_binary(Id)});
add_trace({clientid, ClientId}) ->
    insert(?DGIOT_CLIENT_TRACE, ClientId, clientid);
add_trace({topic, TopicFilter}) ->
    insert(?DGIOT_TOPIC_TRACE, TopicFilter, topic);
add_trace(_) ->
    ignore.

del_trace({Type, Id}) when is_list(Id) ->
    del_trace({Type, list_to_binary(Id)});
del_trace({Type, Id}) when is_atom(Id) ->
    del_trace({Type, atom_to_binary(Id)});
del_trace({clientid, ClientId}) ->
    delete(?DGIOT_CLIENT_TRACE, ClientId);
del_trace({topic, TopicFilter}) ->
    delete(?DGIOT_TOPIC_TRACE, TopicFilter);
del_trace(_) ->
    ignore.
get_trace({Type, Id}) when is_list(Id) ->
    get_trace({Type, list_to_binary(Id)});
get_trace({Type, Id}) when is_atom(Id) ->
    get_trace({Type, atom_to_binary(Id)});
get_trace({clientid, ClientId}) ->
    ets:member(?DGIOT_CLIENT_TRACE, ClientId);
get_trace({topic, Topic}) ->
    lists:any(fun({emqx_topic_trace, TopicFilter, _}) ->
        emqx_topic:match(Topic, TopicFilter)
              end, ets:tab2list(?DGIOT_TOPIC_TRACE));
get_trace(_) ->
    false.

check_trace(From, Topic, Payload, Module, Line) ->
    Msg = #{<<"msg">> => Payload, <<"topic">> => Topic, <<"time">> => dgiot_datetime:now_secs(),
        <<"pid">> => dgiot_utils:to_binary(self()), <<"peername">> => From, <<"mfa">> => Module, <<"line">> => Line},
    check_trace(From, Topic, jsx:encode(Msg)).

check_trace(From, Topic, Payload) ->
    case get_trace({clientid, From}) of
        true ->
            BinClientId = dgiot_utils:to_binary(From),
            dgiot_mqtt:publish(From, <<"logger_trace/trace/", BinClientId/binary, "/", Topic/binary>>, Payload);
        false ->
            case get_trace({topic, Topic}) of
                true ->
                    dgiot_mqtt:publish(self(), <<"logger_trace/trace/", Topic/binary>>, Payload);
                false ->
                    false
            end
    end.

%%--------------------------------------------------------------------
%% Mnesia APIs
%%--------------------------------------------------------------------
lookup(Tab, Key) ->
    Result = mnesia:transaction(fun mnesia:read/1, [{Tab, Key}]),
    result(Result).

insert(?DGIOT_CLIENT_TRACE, Key, Value) ->
    case lookup(?DGIOT_CLIENT_TRACE, Key) of
        {ok, _} ->
            delete(?DGIOT_CLIENT_TRACE, Key);
        _ ->
            pass
    end,
    case get(?DGIOT_CLIENT_TRACE, Key) of
        not_find ->
            insert_(?DGIOT_CLIENT_TRACE, #?DGIOT_CLIENT_TRACE{key = Key, value = Value});
        _ -> pass
    end,
    insert_(?DGIOT_CLIENT_TRACE, #?DGIOT_CLIENT_TRACE{key = Key, value = Value});

insert(?DGIOT_TOPIC_TRACE, Key, Value) ->
    case lookup(?DGIOT_TOPIC_TRACE, Key) of
        {ok, _} ->
            delete(?DGIOT_TOPIC_TRACE, Key);
        _ ->
            pass
    end,
    case get(?DGIOT_TOPIC_TRACE, Key) of
        not_find ->
            insert_(?DGIOT_TOPIC_TRACE, #?DGIOT_TOPIC_TRACE{key = Key, value = Value});
        _ -> pass
    end,
    insert_(?DGIOT_TOPIC_TRACE, #?DGIOT_TOPIC_TRACE{key = Key, value = Value}).

insert_(?DGIOT_CLIENT_TRACE, Record) ->
    F = fun() ->
        mnesia:write(?DGIOT_CLIENT_TRACE, Record, write)
        end,
    Result = mnesia:transaction(F),
    result(Result);

insert_(?DGIOT_TOPIC_TRACE, Record) ->
    F = fun() ->
        mnesia:write(?DGIOT_TOPIC_TRACE, Record, write)
        end,
    Result = mnesia:transaction(F),
    result(Result).

get(Name, Key) ->
    case ets:lookup(Name, Key) of
        [] -> not_find;
        [{Key, Value} | _] -> Value;
        [Value | _] -> Value
    end.

delete(TAB, Key) ->
    F =
        fun() ->
            mnesia:delete({TAB, Key})
        end,
    Result = mnesia:transaction(F),
    result(Result).


result({atomic, ok}) -> true;
result({atomic, []}) -> {error, empty};
result({aborted, Reason}) -> {error, Reason};
result({atomic, Result}) -> {ok, Result};
result(Result) -> Result.
