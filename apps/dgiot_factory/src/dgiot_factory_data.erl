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

-module(dgiot_factory_data).
-author("jonhl").
-include_lib("dgiot/include/logger.hrl").
-define(PRE, <<"_">>).
-define(Database(Name), <<?PRE/binary, Name/binary>>).
-define(Table(Name), <<?PRE/binary, Name/binary>>).
-define(SHEETID(SHEET), <<SHEET/binary, "_id">>).
-define(PRODUCTID, <<"ec71804a3d">>).
-define(TYPE, <<"TD">>).

-export([handle_data/1, save_data/4,add_data/4]).



handle_data([ProductId, DeviceId, Type, FlatMap]) ->
    NumData = dgiot_factory_utils:turn_num(FlatMap, ProductId, Type),
    save_data(ProductId, DeviceId, Type, NumData).

save_data(ProductId, DeviceId, <<"product">> = Type, Payload) ->
    case dgiot_data:lookup({ProductId, ?TYPE}) of
        {ok, Channel} ->
            case dgiot_device_cache:lookup(DeviceId) of
                {ok, #{<<"devaddr">> := DevAddr}} ->
                    handle_product_condition(Channel, ProductId, DeviceId, DevAddr, Type, Payload);
                _ ->
                    {error, not_find_toal_num}
            end;
        _ ->
            {error, not_find_channel}
    end;


save_data(ProductId, DeviceId, <<"semiproduct">> = Type, Payload) ->
%%    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
%%        {ok, #{<<"devaddr">> := DevAddr}} ->
    case dgiot_device_cache:lookup(DeviceId) of
        {ok, #{<<"devaddr">> := DevAddr}} ->
            Id = get_id(DevAddr, Type),
            dgiot_task:save_td_no_match(ProductId, DevAddr, Payload#{?SHEETID(Type) => Id}, #{});

        _ ->
            {error, <<"not_fin_device">>}
    end;

save_data(ProductId, DeviceId, <<"quality">> = Type, Payload) ->
    case dgiot_device_cache:lookup(DeviceId) of
        {ok, #{<<"devaddr">> := DevAddr}} ->
            Id = get_id(DevAddr, Type),
            dgiot_task:save_td_no_match(ProductId, DevAddr, Payload#{?SHEETID(Type) => Id}, #{}),
            handle_quality(Payload, DeviceId);
        _ ->
            {error, <<"not_find_device">>}
    end;

save_data(ProductId, DeviceId, <<"material">>, Payload) ->
    case handle_material(ProductId, DeviceId, Payload) of
        {ok, Res} ->
            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"devaddr">> := DevAddr}} ->
                    dgiot_task:save_td_no_match(ProductId, DevAddr, Res, #{});
                _ ->
                    {error, <<"not_find_device">>}
            end;
        _ ->
            error
    end;



save_data(ProductId, DeviceId, Type, Payload) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"devaddr">> := DevAddr}} ->
            Id = get_id(DevAddr, Type),
            dgiot_task:save_td_no_match(ProductId, DevAddr, Payload#{?SHEETID(Type) => Id}, #{});

        _ ->
            {error, <<"not_fin_device">>}
    end.


handle_product_condition(_Channel, ProductId, DeviceId, DevAddr, Type, #{<<"product_pnumber">> := Pnumber, <<"product_subtime">> := SubTime, <<"product_condition">> := 1} = Payload) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"startTime">> := LastSubTime, <<"progress">> := Progress, <<"content">> := #{<<"baseInfo">> := #{<<"Number">> := Total}}}} ->
            WorkTime = (SubTime - LastSubTime) div 1000,
            Id = get_id(DevAddr, Type),
            UpId = string:to_upper(dgiot_utils:to_list(Id)),
            dgiot_task:save_td_no_match(ProductId, DevAddr, Payload#{<<"product_mhour">> => abs(WorkTime), <<"product_id">> => UpId}, #{}),
            save_progress(DeviceId, Progress, Total, Pnumber, SubTime);

        _ ->
            error
    end;
handle_product_condition(_Channel, ProductId, DeviceId, DevAddr, _Type, #{<<"product_condition">> := 2, <<"product_id">> := Product_id, <<"product_pnumber">> := Pnumber} = Payload) ->
    dgiot_task:save_td_no_match(ProductId, DevAddr, Payload#{<<"product_id">> => dgiot_utils:to_list(Product_id)}, #{}),
    handle_storehouse(2, Pnumber, DeviceId);

handle_product_condition(_Channel, ProductId, DeviceId, DevAddr, _Type, #{<<"product_condition">> := 3, <<"product_id">> := Product_id, <<"product_pnumber">> := Pnumber} = Payload) ->
    dgiot_task:save_td_no_match(ProductId, DevAddr, Payload#{<<"product_id">> => dgiot_utils:to_list(Product_id)}, #{}),
    handle_storehouse(3, Pnumber, DeviceId);
%%            handle_dingdan(DeviceId),
%%    dgiot_jienuo_meter:test(Payload, DeviceId);

handle_product_condition(_, _, _, _, _, _) ->

    error.


handle_material(_, DeviceId, #{<<"material_type">> := <<"picking">>, <<"material_spec">> := MaterialName, <<"material_date">> := Date} = Payload) ->
    Id = get_id(DeviceId, [<<"material">>, MaterialName, Date]),
    {ok, Payload#{<<"material_id">> => Id}};




handle_material(ProductId, DeviceId, #{<<"material_type">> := <<"retriving">>, <<"material_spec">> := MaterialName, <<"material_date">> := Date, <<"material_loss">> := Loss} = Payload) ->
    case maps:find(<<"persion_sessiontoken">>, Payload) of
        {ok, Token} ->
            case dgiot_product_tdengine:get_channel(Token) of
                {ok, Channel} ->
                    case dgiot_factory_getdata:get_work_sheet(ProductId, <<"material">>,undefined ,undefined ,Channel, DeviceId, #{<<"material_spec">> => MaterialName, <<"material_date">> => Date}, 1, 0, <<"true">>) of
                        {ok, {_, Res}} ->
                            case length(Res) of
                                1 ->
                                    F = lists:nth(1, Res),
                                    {ok, F#{<<"material_loss">> => Loss}};
                                _ ->
                                    error
                            end;
                        _ ->
                            error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end;

handle_material(_, _, _) ->

    error.


%%handle_dingdan(DeviceId) ->
%%    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
%%        {ok, #{<<"progress">> := Progress, <<"detail">> := Detail, <<"content">> := #{<<"baseInfo">> := #{<<"Number">> := Total}}}} ->
%%            case Progress >= Total of
%%                true ->
%%                    EndTime = dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"),
%%                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"realstate">> => 8, <<"detail">> => Detail#{<<"taskend">> => EndTime}});
%%                _ ->
%%                    pass
%%            end
%%    end.


handle_storehouse(2, Pnumber, DeviceId) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"storehouse">> := #{<<"unstored">> := Unstored, <<"stored">> := Stored}}} ->
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"storehouse">> => #{<<"unstored">> => Unstored + Pnumber, <<"stored">> => Stored}});
        _ ->
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"storehouse">> => #{<<"unstored">> => Pnumber, <<"stored">> => 0}})

    end;

handle_storehouse(3, Pnumber, DeviceId) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"storehouse">> := #{<<"unstored">> := Unstored, <<"stored">> := Stored}}} ->
            case Unstored - Pnumber < 0 of
                true ->
                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"storehouse">> => #{<<"unstored">> => 0, <<"stored">> => Stored + Pnumber}});
                _ ->
                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"storehouse">> => #{<<"unstored">> => Unstored - Pnumber, <<"stored">> => Stored + Pnumber}})
            end;
        _ ->
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"storehouse">> => #{<<"unstored">> => 0, <<"stored">> => 1}})
    end;

handle_storehouse(_, _, _) ->
    pass.



get_id(DevAddr, Type) ->
    Time = dgiot_utils:to_binary(dgiot_datetime:timestamp()),
    Bin = dgiot_utils:to_binary(Type),
    <<ObjID:10/binary, _/binary>> = dgiot_utils:to_md5(<<Bin/binary, DevAddr/binary, Time/binary>>),
    Res = string:to_upper(dgiot_utils:to_list(ObjID)),
    dgiot_utils:to_binary(Res).


handle_quality(#{<<"quality_people">> := Operator, <<"quality_status">> := Status, <<"quality_quality">> := Quality, <<"quality_alarmid">> := NotificationId,
    <<"quality_pnumber">> := Pnumber}, DeviceId) ->
    case {Status, Quality} of
        {3, 1} ->
            handle_alert(NotificationId, Operator, Quality),
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"realstatus">> => 4});
        {3, 0} ->
            handle_alert(NotificationId, Operator, Quality),
            handle_progress(DeviceId, -Pnumber),
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"realstatus">> => 2});
        {5, 1} ->
            handle_alert(NotificationId, Operator, Quality),
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"realstatus">> => 6});
        {5, 0} ->
            handle_alert(NotificationId, Operator, Quality),
            handle_progress(DeviceId, -Pnumber),
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"realstatus">> => 2});
        {6, 1} ->
            handle_alert(NotificationId, Operator, Quality);
        _ ->
            {ok, ok}
    end;
handle_quality(_, _) ->
    error.


handle_progress(DeviceId, Pnumber) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"progress">> := Progress}} ->
            NewProgress = case Progress + Pnumber < 0 of
                              true ->
                                  0;
                              false ->
                                  Progress + Pnumber
                          end,
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"progress">> => NewProgress});
        _ ->
            pass
    end.

handle_alert(NotificationId, Operator, 1) ->
    case dgiot_parse:get_object(<<"Notification">>, NotificationId) of
        {ok, #{<<"content">> := #{<<"alarm">> := Alarm}}} ->
            dgiot_parse:update_object(<<"Notification">>, NotificationId, #{<<"status">> => 1, <<"content">> => #{<<"alarm">> => Alarm#{<<"operator">> => Operator}, <<"alertstatus">> => 2}});
        _ ->
            pass
    end;
handle_alert(NotificationId, Operator, 0) ->
    case dgiot_parse:get_object(<<"Notification">>, NotificationId) of
        {ok, #{<<"content">> := #{<<"alarm">> := Alarm}}} ->
            dgiot_parse:update_object(<<"Notification">>, NotificationId, #{<<"status">> => 1, <<"content">> => #{<<"alarm">> => Alarm#{<<"operator">> => Operator}}});
        _ ->
            pass
    end.

save_progress(DeviceId, Progress, Total, Pnumber, SubTime) ->
    NewProgress = case Progress + Pnumber > Total of
                      true ->
                          Total;
                      false ->
                          Progress + Pnumber
                  end,
    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"progress">> => NewProgress, <<"startTime">> => SubTime}).

add_data(Channel, ProductId, DevAddr, Data) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"thing">> := Properties}} ->
            case dgiot_tdengine:format_data(ProductId, DevAddr, Properties, Data) of
                #{<<"db">> := DB, <<"tableName">> := Table, <<"using">> := Using, <<"tags">> := Tags, <<"fields">> := _Fields, <<"values">> := Values} ->
                    case dgiot_channelx:call(?TYPE, Channel, config) of
                        {ok, Context} ->
                            Tag = dgiot_utils:to_binary(Tags),
                            Sql = <<"INSERT INTO ", DB/binary, ".", Table/binary, " using ", Using/binary, ".", Using/binary, " TAGS (", Tag/binary, ") VALUES ", Values/binary, ";">>,
%%                            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql);
                            Res = dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql),
                            Res;
                        _ ->
                            error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end.
