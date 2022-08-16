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

-module(dgiot_parse_timescale).
-author("zwx").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_parse.hrl").
-dgiot_swagger(<<"classes">>).

-define(RE_OPTIONS, [global, {return, binary}]).
%%  pg_dump -h localhost -U postgres timescale> ~/timescale_4.0.sql
%% API.
-export([test/0, get_schemas/1, create_schemas/1, create_object/2, get_object/2, query_object/2, update_object/3]).
-export([do_save/3, get_realtime_data/1, get_history_data/2]).

test() ->
    dgiot_parse_timescale:do_save(<<"c507e76f57">>, <<"006573871">>, #{<<"a">> => 2}),
%%    dgiot_parse_timescale:get_realtime_data(DeviceId).
    ok.

get_realtime_data(DeviceId) ->
    case query_object(DeviceId, #{<<"limit">> => 1, <<"order">> => <<"-createdAt">>}) of
        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
            lists:foldl(fun
                            (#{<<"createdAt">> := CreatedAt, <<"timescale">> := Timescale}, Acc) ->
                                Acc ++ [Timescale#{<<"createdAt">> => CreatedAt}];
                            (_, Acc) ->
                                Acc
                        end, [], Results);
        _ ->
            [#{}]
    end.

get_history_data(DeviceId, Query) ->
    {Names, Where} = get_where(Query),
    case query_object(DeviceId, Where) of
        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
            NewResults =
                lists:foldl(fun
                                (#{<<"createdAt">> := CreatedAt, <<"timescale">> := Timescale}, Acc) ->
                                    Acc ++ [Timescale#{<<"createdat">> => CreatedAt}];
                                (_, Acc) ->
                                    Acc
                            end, [], Results),
            {Names, NewResults};
        _ ->
            {Names, []}
    end.

get_where(Query) ->
    Keys = maps:get(<<"keys">>, Query, <<"*">>),
    Database = maps:get(<<"db">>, Query),
    Limit = maps:get(<<"limit">>, Query, 5000),
    Function = maps:get(<<"function">>, Query, <<"last">>),
    Starttime = dgiot_utils:to_int(maps:get(<<"starttime">>, Query, dgiot_datetime:now_ms() - 604800000)) / 1000,
    NewStarttime = dgiot_tdengine_field:get_time(dgiot_utils:to_binary(Starttime), <<"111">>),
    Endtime = dgiot_utils:to_int(maps:get(<<"endtime">>, Query, dgiot_datetime:now_ms())) / 1000,
    NewEndtime = dgiot_tdengine_field:get_time(dgiot_utils:to_binary(Endtime), <<"111">>),
    {Names, _} = dgiot_product_tdengine:get_keys(Database, Function, Keys),
    CreatedAt = #{<<"$lt">> => #{<<"__type">> => <<"Date">>, <<"iso">> => NewEndtime}, <<"$gt">> => #{<<"__type">> => <<"Date">>, <<"iso">> => NewStarttime}},
    {Names, #{<<"limit">> => Limit, <<"order">> => <<"createdAt">>, <<"where">> => #{<<"createdAt">> => CreatedAt}}}.

do_save(ProductId, DevAddr, Data) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    case dgiot_parse_timescale:get_schemas(DeviceId) of
        {error, #{<<"code">> := 103}} ->
            %%  表不存在,建表
            create_schemas(DeviceId);
        _ ->
            pass
    end,
    timer:sleep(500),
    create_object(DeviceId, #{<<"timescale">> => Data}).

get_schemas(Class) ->
    dgiot_parse:get_schemas(?TIMESCALE, Class).

%% 建表
create_schemas(DeviceId) ->
    dgiot_parse:create_schemas(?TIMESCALE, #{
        <<"classLevelPermissions">> => #{
            <<"addField">> => #{<<"role:root">> => true},
            <<"count">> => #{<<"*">> => true},
            <<"create">> => #{<<"*">> => true},
            <<"delete">> => #{<<"*">> => true},
            <<"find">> => #{<<"*">> => true},
            <<"get">> => #{<<"*">> => true},
            <<"protectedFields">> => #{},
            <<"update">> => #{<<"*">> => true}},
        <<"className">> => <<"timescale_", DeviceId/binary>>,
        <<"fields">> => #{
            <<"timescale">> => #{<<"type">> => <<"Object">>}
        }}).

create_object(DeviceId, Data) ->
    dgiot_parse:create_object(?TIMESCALE, <<"timescale_", DeviceId/binary>>, Data).

get_object(DeviceId, ObjectId) ->
    dgiot_parse:get_object(?TIMESCALE, <<"timescale_", DeviceId/binary>>, ObjectId).

query_object(DeviceId, Args) ->
    dgiot_parse:query_object(?TIMESCALE, <<"timescale_", DeviceId/binary>>, Args).

update_object(DeviceId, ObjectId, Data) ->
    dgiot_parse:update_object(?TIMESCALE, <<"timescale_", DeviceId/binary>>, ObjectId, Data).


