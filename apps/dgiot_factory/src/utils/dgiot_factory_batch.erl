%%%-------------------------------------------------------------------
%%% @author wolong
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 3月 2023 10:58
%%%-------------------------------------------------------------------
-module(dgiot_factory_batch).
-author("wolong").

%% API
-export([merge_data/4]).

merge_data(FlatMap, PersonType, Token, TaskDeviceId) ->
    case dgiot_parse:get_object(<<"Device">>, TaskDeviceId) of
        {ok, #{<<"name">> := OrderName, <<"product">> := #{<<"objectId">> := TaskProductId}}} ->
            {ok, {BatchProductId, BatchDeviceId, BatchAddr}} = process_roll_dev(TaskProductId, TaskDeviceId, OrderName, Token, FlatMap),
            NewData = init_data(TaskProductId, TaskDeviceId, BatchDeviceId, FlatMap, PersonType, Token),
            {BatchProductId, BatchDeviceId, BatchAddr, NewData};
        _ ->
            error
    end.

init_data(_TaskProductId, TaskDeviceId, BatchDeviceId, FlatMap, _PersonType, Token) ->
    dgiot_map:merge(FlatMap, #{<<"person">> => #{<<"sessiontoken">> => Token, <<"deviceid">> => TaskDeviceId, <<"sheetsid">> => BatchDeviceId}}).

process_roll_dev(TaskProductId, TaskDeviceId, OrderName, SessionToken, FlatMap) ->
    {BatchProductId, BatchDeviceId, BatchAddr} =
        case get_roll_dev_id(TaskProductId, FlatMap) of
            {A, B, C} ->
                {A, B, C};
            _ -> {<<"1">>, <<"1">>, <<"1">>}
        end,
    case dgiot_device_cache:lookup(BatchDeviceId) of
        {ok, #{<<"acl">> := Acl}} ->
            NewAcl = get_new_acl(SessionToken, Acl),
            dgiot_parse:update_object(<<"Device">>, BatchDeviceId, #{<<"ACL">> => NewAcl, <<"isEnable">> => true}),
            dgiot_device:save_subdevice(BatchDeviceId, TaskDeviceId, 1),
            {ok, {BatchProductId, BatchDeviceId, BatchAddr}};
        _ ->
            NewAcl = get_new_acl(SessionToken, []),
            Device = #{
                <<"objectId">> => BatchDeviceId,
                <<"devaddr">> => BatchAddr,
                <<"name">> => OrderName,
                <<"ACL">> => NewAcl,
                <<"basedata">> => #{},
                <<"product">> => #{
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"Product">>,
                    <<"objectId">> => BatchProductId
                }},
            dgiot_device_cache:post(Device),
            dgiot_parse:create_object(<<"Device">>, Device),
            dgiot_metrics:inc(dgiot_factory, <<"batch_num">>, 1),
            dgiot_device:save_subdevice(BatchDeviceId, TaskDeviceId, 1),
            {ok, {BatchProductId, BatchDeviceId, BatchAddr}}

    end.

get_roll_dev_id(ProductId, FlatMap) ->
    BatchProductId = get_sub_product(ProductId),
    case maps:find(<<"sheetsid">>, maps:get(<<"person">>, FlatMap, #{})) of
        {ok, BatchDeviceId} ->
            case dgiot_device:lookup(BatchDeviceId) of
                {ok, #{<<"devaddr">> := BatchAddr}} ->
                    {BatchProductId, BatchDeviceId, BatchAddr};
                _ ->
                    case dgiot_parse:get_object(<<"Device">>, BatchDeviceId) of
                        {ok, #{<<"devaddr">> := BatchAddr}} ->
                            {BatchProductId, BatchDeviceId, BatchAddr};
                        _ ->
                            error
                    end
            end;
        _ ->
            BatchAddr = dgiot_utils:to_binary(dgiot_datetime:nowstamp()),
            BatchDeviceId = list_to_binary(string:to_upper(binary_to_list(dgiot_parse_id:get_deviceid(BatchProductId, BatchAddr)))),
            {BatchProductId, BatchDeviceId, BatchAddr}
    end.

get_new_acl(SessionToken, Acl) ->
    case dgiot_auth:get_session(SessionToken) of
        #{<<"roles">> := Roles} ->
            UserRoleList = maps:fold(
                fun(_, V, Acc) ->
                    case maps:find(<<"name">>, V) of
                        {ok, Role} ->
                            Acc ++ [<<"role:", Role/binary>>];
                        _ ->
                            Acc
                    end
                end, [], Roles),
            NewRoleList = dgiot_utils:unique_2(Acl ++ UserRoleList),
            lists:foldl(
                fun(X, Acc) ->
                    maps:merge(Acc, #{dgiot_utils:to_binary(X) => #{<<"read">> => true, <<"write">> => true}})
                end, #{}, NewRoleList);
        Err -> {400, Err}
    end.

get_sub_product(ProductId) ->
    case dgiot_hook:run_hook({factory, get_sub_product}, ProductId) of
        {ok, [{ok, SubProduct}]} ->
            SubProduct;
        _ ->
            ProductId
    end.
