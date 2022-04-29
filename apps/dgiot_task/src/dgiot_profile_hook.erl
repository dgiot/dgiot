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

-module(dgiot_profile_hook).
-author("jonhliu").
-include("dgiot_task.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([post/2, put/4, delete/3]).


post('before', _BeforeData) ->
    ok;
post('after', _AfterData) ->
    ok.

put('before', _BeforeData, _ProductId, _Env) ->
    ok;
put('after', AfterData, DeviceId, <<"incremental">>) ->
    case jsx:decode(AfterData, [{labels, binary}, return_maps]) of
        #{<<"profile">> := Profile, <<"devaddr">> := Devaddr, <<"product">> := #{<<"objectId">> := ProductId}} ->
            Modifyprofile = get_modifyprofile(DeviceId, Profile),
%%            设置参数
            case dgiot_device:get_online(DeviceId) of
                true ->
                    Topic = <<"profile/", ProductId/binary, "/", Devaddr/binary>>,
                    dgiot_mqtt:publish(DeviceId, Topic, jsx:encode(Modifyprofile)),
                    dgiot_data:insert(?PROFILE, DeviceId, Profile);
                false ->
                    dgiot_data:insert(?MODIFYPROFILE, DeviceId, {Profile, ProductId, Devaddr})
            end;
        _ ->
            pass
    end,
    ok;

put('after', AfterData, DeviceId, _) ->
    case jsx:decode(AfterData, [{labels, binary}, return_maps]) of
        #{<<"profile">> := Profile, <<"devaddr">> := Devaddr, <<"product">> := #{<<"objectId">> := ProductId}} = Arg ->
            Sessiontoken = maps:get(<<"sessiontoken">>, Arg, <<"">>),
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, Devaddr),
%%            设置参数
            case dgiot_device:get_online(DeviceId) of
                true ->
                    case dgiot_parse:get_object(<<"Product">>, ProductId) of
                        {ok, #{<<"name">> := ProductName, <<"thing">> := #{<<"properties">> := Properties}}} ->
                            NewPayLoad =
                                lists:foldl(fun(X, Acc) ->
                                    case X of
                                        #{<<"identifier">> := Identifier, <<"name">> := Name, <<"accessMode">> := <<"rw">>, <<"dataForm">> := DataForm, <<"dataSource">> := #{<<"_dlinkindex">> := Index} = DataSource} ->
                                            case maps:find(Identifier, Profile) of
                                                {ok, V} ->
                                                    Acc#{
                                                        Index => #{
                                                            <<"sessiontoken">> => Sessiontoken,
                                                            <<"value">> => V,
                                                            <<"identifier">> => Identifier,
                                                            <<"name">> => Name,
                                                            <<"productname">> => ProductName,
                                                            <<"dataSource">> => DataSource,
                                                            <<"dataForm">> => DataForm
                                                        }};
                                                _ ->
                                                    Acc
                                            end;
                                        _ -> Acc
                                    end
                                            end, #{}, Properties),
                            Topic = <<"profile/", ProductId/binary, "/", Devaddr/binary>>,
                            dgiot_mqtt:publish(DeviceId, Topic, jsx:encode(NewPayLoad)),
%%                            io:format("~s ~p NewPayLoad = ~p.~n", [?FILE, ?LINE, NewPayLoad]),
                            dgiot_data:insert(?PROFILE, DeviceId, Profile);
                        false ->
                            dgiot_data:insert(?MODIFYPROFILE, DeviceId, {Profile, ProductId, Devaddr})
                    end;
                _ ->
                    pass
            end;
        _Other ->
            pass
    end.

delete('before', _BeforeData, _ProductId) ->
    ok;
delete('after', _AfterData, _ProductId) ->
    ok.

get_modifyprofile(DeviceId, Profile) ->
    case dgiot_data:get(?PROFILE, DeviceId) of
        not_find ->
            dgiot_data:insert(?PROFILE, DeviceId, Profile),
            Profile;
        OldProfile ->
            maps:fold(fun(K, V, Acc) ->
                case maps:find(K, OldProfile) of
                    error ->
                        Acc#{K => V};
                    {ok, V} ->
                        Acc;
                    _ ->
                        Acc#{K => V}
                end
                      end, #{}, Profile)

    end.