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

%% @doc dgiot_factory Application
-module(dgiot_factory_app).
-emqx_plugin(?MODULE).
-behaviour(application).
-include("dgiot_factory.hrl").

-export([ init_materialets/0]).
%% Application callbacks
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    init_ets(),
    dgiot_factory_sup:start_link().

stop(_State) ->
    ok.
init_ets() ->
    dgiot_data:init(?MATERIALETS),
    dgiot_data:init(?WORKERTREE),
    init_materialets().

init_materialets() ->
%%    case dgiot_parse:query_object(?MATERIALTABLE, #{<<"where">> => #{<<"validate">> => true}}) of
        case dgiot_parse:query_object(<<"material">>, #{<<"where">> => #{<<"validate">> => true}}) of
        {ok, #{<<"results">> := Res}} ->
            lists:foldl(
                fun(X, _) ->
                    case X of
                        #{<<"FMaterialId">> := Id, <<"FDeliveryDate">> := Date} ->
                            Data = maps:remove(<<"createdAt">>,maps:remove(<<"updatedAt">> ,X)),
                            dgiot_data:insert(?MATERIALETS, {Id, Date}, Data);
                        _ ->
                            pass
                    end
                end, [], Res);
        _ ->
            pas
    end.
