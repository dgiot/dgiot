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

-module(dgiot_parse_git).
-author("kenneth").
-include("dgiot_parse.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(DEFField, re:split(application:get_env(?MODULE, delete_field, ""), ",")).

%% API
-export([
    commit/3,
    push/3
]).

commit(Path, 'POST', _Body) ->
    notify(Path, <<"init">>);
commit(Path, 'PUT', Body) ->
    notify(Path, Body);
commit(_Path, _, _) ->
    pass.

notify(Path, Message) ->
    Classes = [<<"View">>, <<"Article">>, <<"Dict">>, <<"Product">>, <<"ProductTemplet">>, <<"Category">>, <<"Menu">>, <<"Permission">>],
    case binary:split(Path, <<$/>>, [global, trim]) of
        [<<>>, <<"classes">>, Class, ObjectId] ->
            case lists:member(Class, Classes) of
                true ->
                    notify(Class, ObjectId, Message);
                false ->
                    pass
            end;
        _ ->
            pass
    end.

notify(Class, ObjectId, Message) ->
    notify(?DEFAULT, Class, ObjectId, Message).

notify(Channel, Class, ObjectId, Message) ->
    dgiot_channelx:do_message(dgiot_utils:to_binary(Channel), {git, Class, ObjectId, Message}).

push(Class, ObjectId, Message) ->
    case dgiot_parse:get_object(Class, ObjectId) of
        {ok, #{<<"ACL">> := Acl} = Body} ->
            dgiot_parse:create_object(<<"Git">>, #{
                <<"id">> => ObjectId,
                <<"class">> => Class,
                <<"ACL">> => Acl,
                <<"data">> => dgiot_utils:to_binary(Body),
                <<"ts">> => dgiot_datetime:now_secs(),
                <<"type">> => <<"commit">>,
                <<"message">> => dgiot_utils:to_binary(Message)
            });
        {ok, Body} ->
             dgiot_parse:create_object(<<"Git">>, #{
                <<"id">> => ObjectId,
                <<"class">> => Class,
                <<"data">> => dgiot_utils:to_binary(Body),
                <<"ts">> => dgiot_datetime:now_secs(),
                <<"type">> => <<"commit">>,
                <<"message">> => dgiot_utils:to_binary(Message)
            });
        _ ->
            pass
    end.
