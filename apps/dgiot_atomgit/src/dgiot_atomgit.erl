%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

%% @doc dgiot_atomgit Protocol
-module(dgiot_atomgit).
-include("dgiot_atomgit.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([
    atomgitapi/1
    , create_product/0
    , create_device/3
    , save_td/0
    , query/0
]).

-define(APP, ?MODULE).


atomgitapi(#{<<"name">> := _Name, <<"devaddr">> := _Devaddr} = Args) ->
    io:format("~s ~p Args = ~p.~n", [?FILE, ?LINE, Args]),
    {ok, Args};

atomgitapi(Args) ->
    io:format("~s ~p Args = ~p.~n", [?FILE, ?LINE, Args]),
    error.


%% 创建产品
create_product() ->
    dgiot_product:create_product(#{
        <<"name">> => <<"添加产品例子"/utf8>>,
        <<"devType">> => <<"dgiot">>,
        <<"category">> => #{<<"objectId">> => <<"5ca6049839">>, <<"__type">> => <<"Pointer">>, <<"className">> => <<"Category">>},
        <<"desc">> => <<"atom test">>,
        <<"config">> => #{<<"interval">> => -1},
        <<"channel">> => #{},
        <<"thing">> => #{},
        <<"ACL">> => #{<<"role:开发者"/utf8>> => #{<<"read">> => true, <<"write">> => true}},
        <<"nodeType">> => 0,
        <<"productSecret">> => dgiot_utils:random()
    }).

%% 创建设备
create_device(ProductId, DTUMAC, DTUIP) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"ACL">> := Acl, <<"devType">> := DevType}} ->
            dgiot_device:create_device(#{
                <<"devaddr">> => DTUMAC,
                <<"name">> => <<DevType/binary, "_", DTUMAC/binary>>,
                <<"ip">> => DTUIP,
                <<"isEnable">> => true,
                <<"product">> => ProductId,
                <<"ACL">> => Acl,
                <<"status">> => <<"ONLINE">>,
                <<"brand">> => DevType,
                <<"devModel">> => DevType
            });
        _ ->
            pass
    end.

%% 存td库
save_td() ->
%%     通过任务通道存td
    dgiot_task:save_td(<<"产品objectId">>, <<"设备地址">>, #{<<"物模型标识符"/utf8>> => <<"value">>}, #{}),
%%    直接存td
    Sql = dgiot_tdengine:format_sql(<<"产品objectId">>, <<"设备地址">>, [#{<<"物模型标识符"/utf8>> => <<"value">>}]),
    dgiot_tdengine_adapter:save_sql(<<"产品objectId">>, Sql).


%% 查询
query() ->
%%    按产品Id查询产品详情
    dgiot_parse:get_object(<<"Product">>, <<"产品id">>),
%%    查询产品列表
    dgiot_parse:query_object(<<"Product">>, #{<<"count">> => <<"objectId">>, <<"limit">> => 100, <<"where">> => #{}}),
%%    按设备Id查询设备详情
    dgiot_parse:get_object(<<"Device">>, <<"设备id">>),
%%    查询设备列表
    case dgiot_parse:query_object(<<"Device">>, #{<<"count">> => <<"objectId">>, <<"limit">> => 100, <<"where">> => #{}}) of
        {ok, #{<<"count">> := Count, <<"results">> := Results}} ->
            io:format("~s ~p Count = ~p.~n", [?FILE, ?LINE, Count]),
%%            count 总数, results 设备列表
%%            遍历设备列表
            lists:foldl(fun(#{<<"objectId">> := DeviceId, <<"name">> := Name}, _) ->
%%              打印设备objectId, 设备名称
                io:format("~s ~p DeviceId ~p => Name = ~p.~n", [?FILE, ?LINE, DeviceId, Name])
                        end, #{}, Results);
        _ ->
            pass
    end.


















