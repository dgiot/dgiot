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
-module(dgiot_gb26875).
-author("stoneliu").
-include_lib("dgiot_gb26875.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([
    userdevice/3,
    load_thing/0,
    get_thing/2,
    sysdevice/3,
    equdevice/3,
    oplog/3
]).

load_thing() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Path = dgiot_httpc:url_join([Dir, "/priv/ana_8_2_1_3.json"]),
    case catch file:read_file(Path) of
        {Err, Reason} when Err == 'EXIT'; Err == error ->
            ?LOG(error, "read  Path,~p error,~p ~n", [Path, Reason]),
            {error, Reason};
        {ok, Bin} ->
            case jsx:is_json(Bin) of
                true ->
                    Map = jiffy:decode(Bin, [return_maps]),
                    case Map of
                        #{<<"properties">> := Properties} when length(Properties) > 0 ->
                            lists:map(fun(#{<<"identifier">> := Id} = X) ->
                                dgiot_data:insert(?GB26875_ETS, Id, X)
                                      end, Properties);
                        _ ->
                            pass
                    end;
                _ ->
                    Bin
            end
    end.

get_thing(Id, Acc) ->
    case dgiot_data:get(?GB26875_ETS, Id) of
        not_find ->
            Acc;
        X ->
            Acc ++ [X]
    end.

sysdevice(#{<<"systype">> := SysType, <<"sysaddr">> := SysAddr} = Map,
    #{<<"source">> := Ip} = Header, #state{devtype = DevType}) ->
    BinSysAddr = dgiot_utils:to_binary(SysAddr),
    %% 建筑消防设施系统
    ProductId = <<"a73fe5d540">>,
    create_sysdevice(BinSysAddr, ProductId, SysType, Ip, DevType, Header),
    case maps:find(<<"data">>, Map) of
        {ok, Data} ->
%%            io:format("~s ~p ~p ", [?FILE, ?LINE, Data]),
            dgiot_task:save_td(ProductId, BinSysAddr, Data, #{});
        _ -> pass
    end.

equdevice(#{<<"systype">> := _SysType, <<"sysaddr">> := SysAddr, <<"equtype">> := EquType, <<"equaddr">> := EquAddr} = Map,
    #{<<"source">> := Ip} = Header, #state{devtype = DevType}) ->
%%    io:format("~s ~p ~p ~n", [?FILE, ?LINE, Map]),
    Name = maps:get(<<"name">>, Map, EquType),
    BinSysAddr = dgiot_utils:to_binary(SysAddr),
    %%    建筑消防设施部件
    ProductId = <<"8778425df1">>,
    creat_equdevice(BinSysAddr, ProductId, EquAddr, Name, Ip, DevType, Header),
    case maps:find(<<"data">>, Map) of
        {ok, Data} ->
            dgiot_task:save_td(ProductId, EquAddr, Data, #{});
        _ -> pass
    end.

userdevice(#{<<"infotype">> := InforType, <<"userid">> := UserId} = Map,
    #{<<"source">> := Ip} = Header, #state{devtype = DevType}) ->
    %%    用户信息传输装置
    ProductId = <<"b3973b214f">>,
    BinUserId = dgiot_utils:to_binary(UserId),
    creat_userdevice(BinUserId, ProductId, InforType, Ip, DevType, Header),
    case maps:find(<<"data">>, Map) of
        {ok, Data} ->
%%            io:format("~s ~p ~p ", [?FILE, ?LINE, Data]),
            dgiot_task:save_td(ProductId, BinUserId, Data, #{});
        _ -> pass
    end;

userdevice(_, _, _) ->
    pass.

oplog(#{<<"infotype">> := InforType, <<"userid">> := UserId} = Map,
    #{<<"source">> := Ip} = Header, #state{devtype = DevType}) ->
    %%    用户信息传输装置
    ProductId = <<"b3973b214f">>,
    BinUserId = dgiot_utils:to_binary(UserId),
    creat_userdevice(BinUserId, ProductId, InforType, Ip, DevType, maps:merge(Map, Header)),
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"ACL">> := Acl}} ->
            create_devceLog(BinUserId, ProductId, Acl, maps:merge(Map, Header));
        _ ->
            pass
    end;

oplog(#{<<"systype">> := _SysType, <<"sysaddr">> := SysAddr, <<"equtype">> := EquType, <<"equaddr">> := EquAddr} = Map,
    #{<<"source">> := Ip} = Header, #state{devtype = DevType}) ->
    %%    建筑消防设施部件
    ProductId = <<"8778425df1">>,
    BinSysAddr = dgiot_utils:to_binary(SysAddr),
    Name = maps:get(<<"name">>, Map, EquType),
    creat_equdevice(BinSysAddr, ProductId, EquAddr, Name, Ip, DevType, maps:merge(Map, Header)),
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"ACL">> := Acl}} ->
            create_devceLog(BinSysAddr, ProductId, Acl, maps:merge(Map, Header));
        _ ->
            pass
    end;

oplog(#{<<"systype">> := SysType, <<"sysaddr">> := SysAddr} = Map,
    #{<<"source">> := Ip} = Header, #state{devtype = DevType}) ->
    BinSysAddr = dgiot_utils:to_binary(SysAddr),
    %% 建筑消防设施系统
    ProductId = <<"a73fe5d540">>,
    create_sysdevice(BinSysAddr, ProductId, SysType, Ip, DevType, maps:merge(Map, Header)),
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"ACL">> := Acl}} ->
            create_devceLog(BinSysAddr, ProductId, Acl, maps:merge(Map, Header));
        _ ->
            pass
    end;

oplog(_, _, _) ->
    ok.

create_sysdevice(BinSysAddr, ProductId, SysType, Ip, DevType, Header) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"ACL">> := Acl}} ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, BinSysAddr),
            case dgiot_device:lookup(DeviceId) of
                {error, not_find} ->
                    Device =
                        #{
                            <<"devaddr">> => BinSysAddr,
                            <<"name">> => SysType,
                            <<"ip">> => Ip,
                            <<"isEnable">> => true,
                            <<"product">> => ProductId,
                            <<"basedata">> => Header,
                            <<"ACL">> => Acl,
                            <<"status">> => <<"ONLINE">>,
                            <<"brand">> => DevType,
                            <<"devModel">> => <<"城市消防"/utf8>>
                        },
                    dgiot_device:create_device(Device);
                _ ->
                    pass
            end;
        _ ->
            pass
    end.

creat_userdevice(BinUserId, ProductId, InforType, Ip, DevType, Header) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"ACL">> := Acl}} ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, BinUserId),
            case dgiot_device:lookup(DeviceId) of
                {error, not_find} ->
                    Device =
                        #{
                            <<"devaddr">> => dgiot_utils:to_binary(BinUserId),
                            <<"name">> => dgiot_utils:to_binary(InforType),
                            <<"ip">> => Ip,
                            <<"isEnable">> => true,
                            <<"product">> => ProductId,
                            <<"ACL">> => Acl,
                            <<"status">> => <<"ONLINE">>,
                            <<"brand">> => DevType,
                            <<"basedata">> => Header,
                            <<"devModel">> => <<"城市消防"/utf8>>
                        },
%%            io:format("~s ~p Device  ~p ~n", [?FILE, ?LINE, Device]),
                    dgiot_device:create_device(Device);
                _ ->
                    pass
            end;
        _ ->
            pass
    end.

creat_equdevice(BinSysAddr, ProductId, EquAddr, Name, Ip, DevType, Header) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"ACL">> := Acl}} ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, BinSysAddr),
            case dgiot_device:lookup(DeviceId) of
                {error, not_find} ->
                    %% 建筑消防设施系统
                    SysProductId = <<"a73fe5d540">>,
                    SysDeviceId = dgiot_parse_id:get_deviceid(SysProductId, BinSysAddr),
                    Device = #{
                        <<"devaddr">> => dgiot_utils:to_binary(EquAddr),
                        <<"name">> => Name,
                        <<"ip">> => Ip,
                        <<"route">> => #{SysDeviceId => DeviceId},
                        <<"isEnable">> => true,
                        <<"product">> => ProductId,
                        <<"basedata">> => Header,
                        <<"ACL">> => Acl,
                        <<"status">> => <<"ONLINE">>,
                        <<"brand">> => DevType,
                        <<"devModel">> => <<"城市消防"/utf8>>
                    },
%%            io:format("~s ~p Device  ~p ~n", [?FILE, ?LINE, Device]),
                    dgiot_device:create_device(Device);
                _ ->
                    pass
            end;
        _ ->
            pass
    end.


create_devceLog(Devaddr, ProductId, Acl, #{<<"serialid">> := Serialid} = Data) ->
    Devcie = #{
        <<"device">> => dgiot_parse_id:get_deviceid(ProductId, Devaddr),
        <<"devaddr">> => dgiot_utils:to_binary(Serialid),
        <<"product">> => #{
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"Product">>,
            <<"objectId">> => ProductId
        },
        <<"data">> => Data,
        <<"ACL">> => Acl,
        <<"status">> => <<"ONLINE">>
    },
    dgiot_parse:create_object(<<"Devicelog">>, Devcie).
