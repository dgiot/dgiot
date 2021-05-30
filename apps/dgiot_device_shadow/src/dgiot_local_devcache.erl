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

-module(dgiot_local_devcache).
-include("dgiot_device_shadow.hrl").
-include_lib("dgiot/include/dgiot_mnesia.hrl").
-include_lib("dgiot/include/logger.hrl").
-author("kenneth").
%% API
-export([init/0, match/4, delete_from_cache/3, save_to_cache/4, lookup_from_cache/3, page/6]).


init() ->
    {DBType, Name} = get_cache_table(),
    case DBType of
        mnesia ->
            ok;
        dets ->
            dgiot_dcache:start(Name, #{auto_save => 60 * 1000});
        ets ->
            dgiot_data:init(Name)
    end.


match(Type, DevType, Pattern, RowFun) ->
    {DBType, Name} = get_cache_table(),
    case DBType of
        mnesia ->
            dgiot_mcache:match_object(Name, {{'_', DevType, Type}, Pattern}, RowFun);
        _ ->
            dgiot_data:match_object(Name, {{'_', DevType, Type}, Pattern}, RowFun)
    end.



delete_from_cache(Type, DevType, DevAddr) ->
    {DBType, Name} = get_cache_table(),
    case DBType of
        mnesia ->
            dgiot_mcache:delete(Name, {DevAddr, DevType, Type});
        _ ->
            dgiot_data:delete(Name, {DevAddr, DevType, Type})
    end.


save_to_cache(Type, DevType, DevAddr, ChildData) ->
    {DBType, Name} = get_cache_table(),
    case DBType of
        mnesia ->
            dgiot_mcache:insert(Name, {DevAddr, DevType, Type}, ChildData);
        _ ->
            dgiot_data:insert(Name, {DevAddr, DevType, Type}, ChildData)
    end.

lookup_from_cache(Type, DevType, DevAddr) ->
    {DBType, Name} = get_cache_table(),
    case DBType of
        mnesia ->
            dgiot_mcache:lookup(Name, {DevAddr, DevType, Type});
        _ ->
            dgiot_data:lookup(Name, {DevAddr, DevType, Type})
    end.


page(Type, DevType, PageNo, PageSize, Filter, RowFun) ->
    Order =
        fun
            ({_, #{<<"state">> := #{<<"devaddr">> := DevA}}}, {_, #{<<"state">> := #{<<"devaddr">> := DevB}}}) ->
                DevA >= DevB;
            (_A, _B) ->
                false
        end,
    NewFilter =
        fun({Key, Value}) ->
            case {Key, Value} of
                {{DevAddr, DevType, Type}, #{<<"state">> := Row} = Map} ->
                    case maps:get(<<"vcon">>, Map, <<>>) of
                        <<>> -> Filter(DevAddr, Row);
                        VCon -> Filter(DevAddr, Row#{<<"vcon">> => VCon})
                    end;
                _ ->
                    false
            end
        end,
    {DBType, Name} = get_cache_table(),
    case DBType of
        mnesia ->
            dgiot_mcache:page(Name, PageNo, PageSize, NewFilter, RowFun, Order);
        _ ->
            dgiot_data:page(Name, PageNo, PageSize, NewFilter, RowFun, Order)
    end.

get_cache_table() ->
    Type = application:get_env(dgiot_device_shadow, cache_type, ets),
    {Type, list_to_atom(lists:concat([Type, "_", smartdev]))}.

