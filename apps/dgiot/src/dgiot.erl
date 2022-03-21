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

-module(dgiot).
-author("johnliu").
-include("dgiot.hrl").
-export([get_env/1,get_env/2, get_env/3, init_plugins/0, child_spec/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
get_env(Key) ->
    case get_env(Key, not_find) of
        not_find -> throw({error, not_find});
        Value -> Value
    end.

get_env(Key, Default) ->
    get_env(?MODULE, Key, Default).

get_env(App, Key, Default) ->
    application:get_env(App, Key, Default).

init_plugins() ->
   SysApp =  lists:foldl(fun(X,Acc) ->
        case X of
            {Plugin,_,_} ->
                BinPlugin = dgiot_utils:to_binary(Plugin),
                case BinPlugin of
                    <<"dgiot_",_>> -> Acc ++ [Plugin];
                    _ -> Acc
                end;
             _ ->   Acc
        end
        end,?SYS_APP,ekka_boot:all_module_attributes(dgiot_plugin)),
    dgiot_data:insert({dgiot,sys_app},SysApp).



child_spec(Mod, supervisor) ->
    #{id => Mod,
        start => {Mod, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [Mod]
    };

child_spec(Mod, worker) ->
    #{id => Mod,
        start => {Mod, start_link, []},
        restart => permanent,
        shutdown => 15000,
        type => worker,
        modules => [Mod]
    }.