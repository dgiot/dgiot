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

-module(dgiot_factory_material).
-author("wolong").
-include("dgiot_factory.hrl").
%% API
-export([get_material_record/1, post_material/2]).

get_material_record(DeviceId) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"material">> := Material}} ->
            {ok, Material};
        _ ->
            case dgiot_hook:run_hook({factory, get_material}, [DeviceId]) of
                {ok, [{ok, Material}]} ->
                    {ok, Material};
                _ ->
                    error
            end
    end.


post_material(DeviceId, #{<<"material_name">> := Name} = Data) when is_map(Data) ->
    case get_material_record(DeviceId) of
        {ok, Material} ->
            case maps:find(Name, Material) of
                {ok, _Res} ->
                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"material">> => maps:merge(Material, #{Name => Data})});
                _ ->
                    error
            end;
        _ ->
            error
    end;

post_material(DeviceId, Data) when is_list(Data) ->
    case get_material_record(DeviceId) of
        {ok, Material} ->
            Res = lists:foldl(
                fun(X, Acc) ->
                    case hanlde_pickandretrive(X, Acc) of
                        {ok, Name, Res} ->
                            Acc#{Name => Res};
                        _ ->
                            Acc
                    end
                end, Material, Data),
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"material">> => Res});
        _ ->
            error
    end;
post_material(_, _) ->
    error.


hanlde_pickandretrive(#{<<"material_name">> := Name, <<"material_date">> := Date,
    <<"material_people">> := People, <<"material_type">> := <<"picking">>, <<"material_number">> := Num}, Material) ->
    case maps:find(Name, Material) of
        {ok, Res} ->
            case Res of
                #{<<"material_pick">> := Pick, <<"material_picked">> := Picked} ->
                    After = dgiot_utils:to_float(Picked) + dgiot_utils:to_float(Num),
                    {ok, Name, maps:merge(Res, #{<<"material_picked">> => After, <<"material_pick">> => Pick ++ [#{<<"material_date">> => Date, <<"material_people">> => People, <<"material_number">> => Num}]})};
                _ ->
                    error
            end;
        _ ->
            error
    end;
hanlde_pickandretrive(#{<<"material_name">> := Name, <<"material_date">> := Date,
    <<"material_people">> := People, <<"material_type">> := <<"retriving">>, <<"material_number">> := Num}, Material) ->
    case maps:find(Name, Material) of
        {ok, Res} ->
            case Res of
                #{<<"material_retrive">> := Retrive, <<"material_picked">> := Picked} ->
                    After = dgiot_utils:to_float(Picked) - dgiot_utils:to_float(Num),
                    {ok, Name, maps:merge(Res, #{<<"material_picked">> => After, <<"material_retrive">> => Retrive ++ [#{<<"material_date">> => Date, <<"material_people">> => People, <<"material_number">> => Num}]})};
                _ ->
                    error
            end;
        _ ->
            error
    end;
hanlde_pickandretrive(_, _) ->
    error.
