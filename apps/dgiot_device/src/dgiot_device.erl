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

-module(dgiot_device).
-define(CRLF, "\r\n").
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/dgiot_mnesia.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TIMEOUT, 60000).
-dgiot_data("ets").
-export([init_ets/0]).
-export([create_device/1, create_device/2, get_sub_device/1, get_sub_device/2, get/2]).
-export([load_device/1, sync_parse/1, post/1, put/1, save/1, save/2, save/3, lookup/1, lookup/2, delete/1, delete/2, save_prod/2, lookup_prod/1, get_online/1]).
-export([encode/1, decode/3, save_subdevice/2, get_subdevice/2, get_file/4]).

init_ets() ->
    dgiot_data:init(?DGIOT_PRODUCT),
    ok.

load_device(Order) ->
    Success = fun(Page) ->
        lists:map(fun(Device) ->
            dgiot_device:save(Device)
                  end, Page)
              end,
    Query = #{
        <<"order">> => Order,
        <<"where">> => #{}
    },
    dgiot_parse_loader:start(<<"Device">>, Query, 0, 100, 1000000, Success).

post(Device) ->
    DeviceId = maps:get(<<"objectId">>, Device),
    Status =
        case maps:get(<<"status">>, Device, <<"OFFLINE">>) of
            <<"OFFLINE">> -> false;
            _ -> true
        end,
    dgiot_mnesia:insert(DeviceId, {[Status, dgiot_datetime:now_secs(), get_acl(Device)], node()}).

put(Device) ->
    DeviceId = maps:get(<<"objectId">>, Device),
    case lookup(DeviceId) of
        {ok, {[Status, _, Acl], Node}} ->
            case maps:find(<<"ACL">>, Device) of
                error ->
                    dgiot_mnesia:insert(DeviceId, {[Status, dgiot_datetime:now_secs(), Acl], Node});
                {ok, _} ->
                    dgiot_mnesia:insert(DeviceId, {[Status, dgiot_datetime:now_secs(), get_acl(Device)], Node})
            end;
        _ ->
            pass
    end.

save(Device) ->
    DeviceId = maps:get(<<"objectId">>, Device),
    UpdatedAt =
        case maps:get(<<"updatedAt">>, Device, dgiot_datetime:now_secs()) of
            <<Data:10/binary, "T", Time:8/binary, _/binary>> ->
                dgiot_datetime:to_unixtime(dgiot_datetime:to_localtime(<<Data/binary, " ", Time/binary>>));
            Now -> Now
        end,
    Status =
        case maps:get(<<"status">>, Device, <<"OFFLINE">>) of
            <<"OFFLINE">> -> false;
            _ -> true
        end,
    dgiot_mnesia:insert(DeviceId, {[Status, UpdatedAt, get_acl(Device)], node()}).

get_acl(Device) ->
    ACL = maps:get(<<"ACL">>, Device, #{}),
    lists:foldl(fun(X, Acc) ->
        Acc ++ [binary_to_atom(X)]
                end, [], maps:keys(ACL)).

save(DeviceId, _Data) ->
    case lookup(DeviceId) of
        {ok, {[Status, _Now, Acl], Node}} ->
            dgiot_mnesia:insert(DeviceId, {[Status, dgiot_datetime:now_secs(), Acl], Node});
        _ -> pass
    end.

save(ProductId, DevAddr, _Data) ->
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    save(DeviceId, _Data).

sync_parse(OffLine) ->
    Fun = fun(X) ->
        {_, DeviceId, V} = X,
        Now = dgiot_datetime:now_secs(),
        case V of
            {[true, Last, _], Node} when (Now - Last) > (OffLine * 1000) ->
                case dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"status">> => <<"OFFLINE">>}) of
                    {ok, _R} ->
                        dgiot_mnesia:insert(DeviceId, {[false, Last], Node});
                    _ ->
                        pass
                end,
                timer:sleep(50);
            {[false, Last, _], Node} when (Now - Last) < (OffLine * 1000) ->
                case dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"status">> => <<"ONLINE">>}) of
                    {ok, _R} ->
                        dgiot_mnesia:insert(DeviceId, {[true, Last], Node});
                    _ ->
                        pass
                end,
                timer:sleep(50);
            _ ->
                pass
        end,
        false
          end,
    dgiot_mnesia:search(Fun, #{<<"skip">> => 0, <<"limit">> => 1000000}).

lookup(DeviceId) ->
    case dgiot_mnesia:lookup(DeviceId) of
        {aborted, Reason} ->
            {error, Reason};
        {ok, [{mnesia, _K, V}]} ->
            {ok, V};
        _ ->
            {error, not_find}
    end.

lookup(ProductId, DevAddr) ->
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    lookup(DeviceId).

save_subdevice({ProductId, DevAddr}, {DtuAddr, SlaveId}) ->
    dgiot_data:insert({DtuAddr, SlaveId}, {ProductId, DevAddr}).


get_subdevice(DtuAddr, SlaveId) ->
%%    todo 返回productid,写对应save
    dgiot_data:get({DtuAddr, SlaveId}).


delete(DeviceId) ->
    dgiot_mnesia:delete(DeviceId).

delete(ProductId, DevAddr) ->
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    dgiot_mnesia:delete(DeviceId).

%% 存储产品
%%-define(SMART_PROD, mnesia_smartprod).
%%-record(dgiot_prod, {
%%    key,      % [ProductId], [产品ID]
%%    product   % 产品基本数据,map类型
%%}).
save_prod(ProductId, Product) ->
    case dgiot_data:insert(?DGIOT_PRODUCT, ProductId, Product) of
        true -> ok;
        {error, Reason} ->
            {error, Reason}
    end.

lookup_prod(ProductId) ->
    case dgiot_data:get(?DGIOT_PRODUCT, ProductId) of
        not_find ->
            not_find;
        Value ->
            {ok, Value}
    end.

get_sub_device(DtuAddr) ->
    Query = #{<<"keys">> => [<<"route">>, <<"devaddr">>, <<"product">>],
        <<"where">> => #{<<"route.", DtuAddr/binary>> => #{<<"$regex">> => <<".+">>}},
        <<"order">> => <<"devaddr">>, <<"limit">> => 1000,
        <<"include">> => <<"product">>},
    case dgiot_parse:query_object(<<"Device">>, Query) of
        {ok, #{<<"results">> := []}} -> [];
        {ok, #{<<"results">> := List}} -> List;
        _ -> []
    end.

get_sub_device(DtuAddr, SessionToken) ->
    Query = #{<<"keys">> => [<<"route">>, <<"devaddr">>, <<"product">>],
        <<"where">> => #{<<"route.", DtuAddr/binary>> => #{<<"$regex">> => <<".+">>}},
        <<"order">> => <<"devaddr">>, <<"limit">> => 1000,
        <<"include">> => <<"product">>},
    case dgiot_parse:query_object(<<"Device">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := []}} -> [];
        {ok, #{<<"results">> := List}} -> List;
        _ -> []
    end.

create_device(#{
    <<"status">> := Status,
    <<"brand">> := Brand,
    <<"devModel">> := DevModel,
    <<"name">> := Name,
    <<"devaddr">> := DevAddr,
    <<"product">> := ProductId
} = Device, SessionToken) ->
    #{<<"objectId">> := DeviceId} =
        dgiot_parse:get_objectid(<<"Device">>, #{<<"product">> => ProductId, <<"devaddr">> => DevAddr}),
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"objectId">> := ObjectId}} ->
            {ok, Result} = dgiot_parse:update_object(<<"Device">>, ObjectId,
                #{
                    <<"isEnable">> => maps:get(<<"isEnable">>, Device, true),
                    <<"status">> => Status
                },
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]),
            {ok, Result#{<<"objectId">> => ObjectId}};
        _R ->
            {{Y, M, D}, {_, _, _}} = dgiot_datetime:local_time(),
            Batch_name = dgiot_utils:to_list(Y) ++ dgiot_utils:to_list(M) ++ dgiot_utils:to_list(D),
            NewDevice = Device#{
                <<"isEnable">> => maps:get(<<"isEnable">>, Device, true),
                <<"product">> => #{
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"Product">>,
                    <<"objectId">> => ProductId
                },
                <<"location">> => maps:get(<<"location">>, Device, #{<<"__type">> => <<"GeoPoint">>, <<"longitude">> => 120.161324, <<"latitude">> => 30.262441}),
                <<"basedata">> => maps:get(<<"basedata">>, Device, #{}),
                <<"detail">> => #{
                    <<"desc">> => Name,
                    <<"brand">> => Brand,
                    <<"devModel">> => DevModel,
                    <<"batchId">> => #{
                        <<"batch_name">> => dgiot_utils:to_binary(Batch_name),
                        <<"createdtime">> => dgiot_datetime:now_secs()
                    }
                }
            },
            dgiot_parse:create_object(<<"Device">>, maps:without([<<"brand">>, <<"devModel">>], NewDevice),
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}])
    end.

create_device(#{
    <<"status">> := Status,
    <<"brand">> := Brand,
    <<"devModel">> := DevModel,
    <<"name">> := Name,
    <<"devaddr">> := DevAddr,
    <<"product">> := ProductId} = Device) ->
    #{<<"objectId">> := DeviceId} = dgiot_parse:get_objectid(<<"Device">>, #{<<"product">> => ProductId, <<"devaddr">> => DevAddr}),
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, Result} ->
            Body = #{
                <<"ip">> => maps:get(<<"ip">>, Device, <<"">>),
                <<"status">> => Status},
            dgiot_parse:update_object(<<"Device">>, DeviceId, Body),
            {ok, Result};
        _R ->
            {{Y, M, D}, {_, _, _}} = dgiot_datetime:local_time(),
            Batch_name = dgiot_utils:to_list(Y) ++ dgiot_utils:to_list(M) ++ dgiot_utils:to_list(D),
            NewDevice = Device#{
                <<"location">> => maps:get(<<"location">>, Device, #{<<"__type">> => <<"GeoPoint">>, <<"longitude">> => 120.161324, <<"latitude">> => 30.262441}),
                <<"basedata">> => maps:get(<<"basedata">>, Device, #{}),
                <<"isEnable">> => maps:get(<<"isEnable">>, Device, true),
                <<"product">> => #{
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"Product">>,
                    <<"objectId">> => ProductId
                },
                <<"detail">> => #{
                    <<"desc">> => Name,
                    <<"brand">> => Brand,
                    <<"devModel">> => DevModel,
                    <<"batchId">> => #{
                        <<"batch_name">> => dgiot_utils:to_binary(Batch_name),
                        <<"createdtime">> => dgiot_datetime:now_secs()
                    }
                }
            },
            ?LOG(info, "~p", [NewDevice]),
            R = dgiot_parse:create_object(<<"Device">>, maps:without([<<"brand">>, <<"devModel">>], NewDevice)),
            ?LOG(info, "~p", [R]),
            R
    end.

get(ProductId, DevAddr) ->
    Keys = [<<"objectId">>, <<"status">>, <<"isEnable">>],
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, Device} ->
            case maps:get(<<"isEnable">>, Device, false) of
                false ->
                    {error, forbiden};
                true ->
                    {ok, maps:with(Keys, Device)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

encode(Frame) when is_map(Frame) ->
    case maps:get(<<"data">>, Frame, no) of
        no ->
            dgiot_protocol:encode(Frame);
        Data ->
            HxData = dgiot_utils:binary_to_hex(Data),
            case re:run(HxData, <<"0000180080E1">>) of
                nomatch ->
                    dgiot_protocol:encode(Frame);
                _ ->
                    {ignore, Frame}
            end
    end;
encode(Data) when is_binary(Data) ->
    {ok, Data}.

decode([], _, _State) ->
    {error, not_decode};
decode([MsgType | Other], Bin, State) ->
    case dgiot_protocol:decode(MsgType, Bin, State) of
        {ok, Rest, Messages} ->
            {ok, Rest, Messages};
        {error, _} ->
            %?LOG(error,"decode:~p, not this protocol:~p", [Bin, MsgType]),
            decode(Other, Bin, State)
    end.


get_online(DeviceId) ->
    OffLine = dgiot_data:get({device, offline}),
    Now = dgiot_datetime:now_ms(),
    case lookup(DeviceId) of
        {ok, {[_, Ts, _], _}} when Now - Ts < (OffLine * 1000) ->
            true;
        _ ->
            false
    end.


get_file(ProductId, DevAddr, Url1, Ext) ->

    {file, Here} = code:is_loaded(?MODULE),
    Root = dgiot_evidence:get_filehome(unicode:characters_to_list(Here)),
    Name = dgiot_datetime:now_secs(),
    Path = Root ++ "/" ++ dgiot_utils:to_list(Name) ++ Ext,
    os:cmd("wget -c " ++ Url1 ++ " -O /" ++ Path),

    AppName = get_appname(ProductId, DevAddr),
    SessionToken = dgiot_parse_handler:get_token(AppName),
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),

    inets:start(),
    Url = get_url(AppName),
    case file:read_file(Path) of
        {ok, Stream} ->
            FileName = dgiot_utils:to_binary(filename:basename(Path)),

            Boundary = <<"-----------------------acebdf135724681">>,
            Header = <<"--", Boundary/binary, ?CRLF, "Content-Disposition: form-data;name=\"">>,

            Data1 = <<"output\"", ?CRLF, ?CRLF, "json", ?CRLF>>,
            ParamBody1 = <<Header/binary, Data1/binary>>,

            Data2 = <<"scene\"", ?CRLF, ?CRLF, AppName/binary, ?CRLF>>,
            ParamBody2 = <<Header/binary, Data2/binary>>,

            Data3 = <<"path\"", ?CRLF, ?CRLF, DeviceId/binary, ?CRLF>>,
            ParamBody3 = <<Header/binary, Data3/binary>>,

            Data4 = <<"auth_token\"", ?CRLF, ?CRLF, SessionToken/binary, ?CRLF>>,
            ParamBody4 = <<Header/binary, Data4/binary>>,

            Data5 = <<"filename\"", ?CRLF, ?CRLF, FileName/binary, ?CRLF>>,
            ParamBody5 = <<Header/binary, Data5/binary>>,

            Tail = <<"--", Boundary/binary, "--", ?CRLF, ?CRLF>>,

            FileBody = <<Header/binary, "file\"; filename=\"", FileName/binary, "\"", ?CRLF,
                "Content-Type: application/octet-stream", ?CRLF, ?CRLF, Stream/binary, ?CRLF, Tail/binary>>,

            ParamBody = <<ParamBody1/binary, ParamBody2/binary, ParamBody3/binary, ParamBody4/binary, ParamBody5/binary>>,

            Body = <<ParamBody/binary, FileBody/binary>>,
            Size = byte_size(Body),
            ContentType = <<"multipart/form-data; boundary=", Boundary/binary>>,
            case httpc:request(post, {dgiot_utils:to_list(Url), [{"Content-Length", integer_to_list(Size)}], binary_to_list(ContentType), Body}, [], []) of
                {ok, {{"HTTP/1.1", 200, "OK"}, _, Json}} ->
                    case jsx:decode(dgiot_utils:to_binary(Json), [{labels, binary}, return_maps]) of
                        #{<<"md5">> := _Md5} ->
                            os:cmd("rm -rf /temp/" ++ dgiot_utils:to_list(Name) ++ ".jpg"),
                            Name;
                        Error1 -> Error1
                    end;
                Error -> Error
            end;
        {error, Reason} ->
            ?LOG(info, "Reason ~p ", [Reason]),
            {error, Reason}
    end.

get_url(AppName) ->
    Roleid = dgiot_parse:get_roleid(AppName),
    case dgiot_parse:get_object(<<"_Role">>, Roleid) of
        {ok, #{<<"tag">> := #{<<"appconfig">> := #{<<"file">> := Url}}}} ->
            Url;
        _ -> <<"">>
    end.


get_appname(ProductId, DevAddr) ->
    case dgiot_device:lookup(ProductId, DevAddr) of
        {ok, {[_, _, [Acl | _]], _}} ->
            BinAcl = atom_to_binary(Acl),
            case BinAcl of
                <<"role:", Name/binary>> ->
                    Name;
                _ ->
                    <<"admin">>
            end;
        _ ->
            <<"admin">>
    end.
