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

%% @doc dgaiot_openai Protocol
-module(dgaiot_openai).
-include("dgaiot_openai.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([do_requset/3,
    process_text/2
]).

-define(APP, ?MODULE).

do_requset(Type, Key, Args) ->
    Url = "https://api.openai.com/v1/" ++ dgiot_utils:to_list(Type),
    Authorization = "Bearer " ++ dgiot_utils:to_list(Key),
    Headers = [
        {"Authorization", Authorization}
    ],
    case httpc:request(post, {Url, Headers, "application/json", jsx:encode(Args)}, [], []) of
        {ok, {{"HTTP/1.1", 200, "OK"}, _, Json}} ->
            case jsx:decode(dgiot_utils:to_binary(Json), [{labels, binary}, return_maps]) of
                #{<<"choices">> := _Choices} = Response ->
                    Response#{<<"code">> => <<"200">>};
                Error ->
                    #{<<"code">> => <<"500">>, <<"error">> => Error}
            end;
        Error ->
            #{<<"code">> => <<"500">>, <<"error">> => Error}
    end.

process_text(Key, #{<<"model">> := Model, <<"prompt">> := Prompt, <<"system_content">> := System_content}) ->
    case validate_input(Prompt) of
        ok ->
            RequestBody = jsx:encode(#{
                <<"model">> => Model,
                <<"messages">> => [
                    #{<<"role">> => <<"system">>, <<"content">> => System_content},
                    #{<<"role">> => <<"user">>, <<"content">> => Prompt}
                ],
                <<"stream">> => false
            }),
            Headers = [
                {"Content-Type", "application/json"},
                {"Authorization", "Bearer " ++ Key}
            ],
            CACertsPath = certifi:cacerts(),
            Options = [
                {ssl, [{verify, verify_peer}, {cacerts, CACertsPath}]},
                {timeout, 10000}
            ],
%%            io:format("Options ~p ~n",[Options]),
            case ibrowse:send_req("https://api.deepseek.com/chat/completions", Headers, post, RequestBody, Options) of
                {ok, 200, _, Body} ->
                    try dgiot_json:decode(dgiot_utils:to_binary(Body), [return_maps]) of
                        Result ->
                            {ok, Result}
                    catch
                        _:_ -> {error, {json_decode_error, Body}}
                    end;
                {ok, "200", _, Body} ->
                    try dgiot_json:decode(dgiot_utils:to_binary(Body), [return_maps]) of
                        Result ->
                            {ok, Result}
                    catch
                        _:_ -> {error, {json_decode_error, Body}}
                    end;
                {ok, Code, _, _} ->
                    io:format("Code ~p ~n", [Code]),
                    {error, {http_error, Code}};
                {error, Reason} ->
                    lager:error("Request to DeepSeek API failed: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

validate_input(Input) when is_binary(Input), byte_size(Input) > 0 ->
    ok;
validate_input(_) ->
    {error, invalid_input}.
