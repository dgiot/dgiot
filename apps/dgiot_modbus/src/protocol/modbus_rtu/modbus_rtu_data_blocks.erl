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
%% See the License for the specific specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(modbus_rtu_data_blocks).
-author("jonhl").

-include("dgiot_modbus.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([
    process_data_blocks/2,
    process_props_recursive/3,
    process_single_prop/3,
    build_cache_recursive/3,
    get_cache_data/3,
    merge_cache_data/2
]).

%% @doc 处理数据块（递归入口）
process_data_blocks(DataBlockCache, Props) ->
    case is_list(Props) of
        true -> process_props_recursive(Props, DataBlockCache, #{});
        false -> process_single_prop(Props, DataBlockCache, #{})
    end.

%% @doc 递归处理属性列表
process_props_recursive([], _DataBlockCache, Acc) -> 
    Acc;

process_props_recursive([Prop | Rest], DataBlockCache, Acc) ->
    NewAcc = process_single_prop(Prop, DataBlockCache, Acc),
    process_props_recursive(Rest, DataBlockCache, NewAcc).

%% @doc 处理单个属性
process_single_prop(#{<<"dataSource">> := DataSource} = Prop, DataBlockCache, Acc) ->
    case DataSource of
        #{<<"operatetype">> := <<"writeHreg">>, <<"data">> := _Data} ->
            DataSource1 = DataSource#{<<"data">> => _Data},
            Prop1 = Prop#{<<"dataSource">> => DataSource1},
            Acc#{Prop1 => Prop1};

        #{<<"operatetype">> := OperateType, <<"address">> := Address, <<"data">> := _Data} = DataSource ->
            case get_cache_data(DataBlockCache, OperateType, Address) of
                {ok, CacheData} ->
                    DataSource1 = DataSource#{<<"data">> => CacheData},
                    Prop1 = Prop#{<<"dataSource">> => DataSource1},
                    Acc#{Prop1 => Prop1};
                {error, _Reason} ->
                    Acc
            end;

        _ ->
            Acc
    end;

process_single_prop(_, _DataBlockCache, Acc) ->
    Acc.

%% @doc 递归构建数据块缓存
build_cache_recursive([], _DataBlockCache, Acc) -> Acc;

build_cache_recursive([#{<<"dataSource">> := DataSource} | Rest], DataBlockCache, Acc) ->
    NewAcc = case DataSource of
        #{<<"operatetype">> := OperateType, <<"address">> := Address, <<"data">> := Data} ->
            Key = {OperateType, Address},
            case maps:get(Key, DataBlockCache, not_found) of
                not_found -> 
                    Acc#{Key => Data};
                OldData ->
                    MergedData = merge_cache_data(OldData, Data),
                    Acc#{Key => MergedData}
            end;
        _ ->
            Acc
    end,
    build_cache_recursive(Rest, DataBlockCache, NewAcc);

build_cache_recursive([_Prop | Rest], DataBlockCache, Acc) ->
    build_cache_recursive(Rest, DataBlockCache, Acc).

%% @doc 获取缓存数据
get_cache_data(DataBlockCache, OperateType, Address) ->
    Key = {OperateType, Address},
    case maps:get(Key, DataBlockCache, not_found) of
        not_found -> {error, not_found};
        Data -> {ok, Data}
    end.

%% @doc 合并缓存数据
merge_cache_data(OldData, NewData) when is_binary(OldData), is_binary(NewData) ->
    <<OldData/binary, NewData/binary>>;

merge_cache_data(OldData, NewData) when is_list(OldData), is_list(NewData) ->
    merge_lists_recursive(OldData, NewData, []);

merge_cache_data(OldData, NewData) ->
    ?LOG(warning, "Cannot merge data types: ~p and ~p", [OldData, NewData]),
    NewData.

%% @doc 递归合并两个列表
merge_lists_recursive([], [], Acc) -> lists:reverse(Acc);
merge_lists_recursive([H1 | T1], [H2 | T2], Acc) ->
    Merged = case {H1, H2} of
        {#{}, #{}} -> maps:merge(H1, H2);
        {L1, L2} when is_list(L1), is_list(L2) -> merge_lists_recursive(L1, L2, []);
        {_V1, V2} -> V2  % 用新值覆盖旧值
    end,
    merge_lists_recursive(T1, T2, [Merged | Acc]).
