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

-module(dgiot_notification).
-author("kenneth").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([send_sms/3, send_sms/4, send_sms/5]).

-export([send_verification_code/2, check_verification_code/2]).

% 验证类消息
send_verification_code(NationCode, Key) ->
    case catch dgiot_cache:get(Key) of
        Random when is_integer(Random) ->
            {error, unicode:characters_to_binary(<<"验证码未过期，查看验证码！"/utf8>>)};
        _ ->
            rand:seed(exs1024),
            Rand = 10000 + erlang:round(rand:uniform() * 10000),
            TTL = 3,
            case dgiot_notification:send_sms(NationCode, Key, "340847", [Rand, TTL]) of
                {ok, _Ext} ->
                    dgiot_cache:set(Key, Rand, TTL * 60),
                    {ok, #{<<"expire">> => TTL * 60}};
                {error, Err} ->
                    {error, Err}
            end
    end.

check_verification_code(Key, Code) ->
    case catch dgiot_cache:get(Key) of
        Code ->
            dgiot_cache:delete(Key),
            true;
        _ ->
            false
    end.



send_sms(Mobile, TplId, Params) ->
    send_sms("+86", Mobile, TplId, Params).
send_sms(NationCode, Mobile, TplId, Params) ->
    send_sms(NationCode, Mobile, TplId, Params, <<>>).
send_sms(NationCode, Mobile, TplId, Params, Ext) ->
    Random = integer_to_list(1000 + rand:uniform(1000)),
    AppId = dgiot:get_env(tencent_sms_appid),
    AppKey = dgiot:get_env(tencent_sms_appkey),
    BaseUrl = "https://yun.tim.qq.com/v5/tlssmssvr/sendsms?sdkappid=~s&random=~s",
    Url = io_lib:format(BaseUrl, [AppId, Random]),
    Now = dgiot_datetime:nowstamp(),
    case re:run(NationCode, <<"\\+(\\d{1,3})">>, [{capture, all, binary}]) of
        {match, [_, NationCode1]} ->
            Data = #{
                <<"tpl_id">> => case is_binary(TplId) of true -> TplId; false -> list_to_binary(TplId) end,
                <<"ext">> => Ext,
                <<"extend">> => <<>>,
                <<"params">> => Params,
                <<"sign">> => <<>>,
                <<"tel">> => #{
                    <<"mobile">> => case is_binary(Mobile) of true -> Mobile; false -> list_to_binary(Mobile) end,
                    <<"nationcode">> => NationCode1
                },
                <<"time">> => Now
            },
            SigStr = io_lib:format("appkey=~s&random=~s&time=~s&mobile=~s", [AppKey, Random, integer_to_list(Now), Mobile]),
            Sig = string:to_lower(binary_to_list(<<<<Y>> || <<X:4>> <= crypto:hash(sha256, SigStr), Y <- integer_to_list(X, 16)>>)),
            Body = Data#{<<"sig">> => list_to_binary(Sig)},
            Request = {Url, [], "application/json", jsx:encode(Body)},
            case catch httpc:request(post, Request, [], [{body_format, binary}]) of
                {ok, {{_HTTPVersion, 200, "OK"}, _Header, ResBody}} ->
                    case jsx:decode(ResBody, [{labels, binary}, return_maps]) of
                        #{<<"result">> := 0, <<"ext">> := Ext} ->
                            {ok, Ext};
                        #{<<"errmsg">> := ErrMsg, <<"result">> := Code} ->
                            ?LOG(error,"Send SMS ERROR: ~p->~ts, Request:~p~n", [list_to_binary(Url), unicode:characters_to_binary(ErrMsg), Body]),
                            {error, #{code => Code, error => ErrMsg}}
                    end;
                {Err, Reason} when Err == error; Err == 'EXIT' ->
                    ?LOG(error,"Send SMS ERROR: ~p, ~p, ~p~n", [Url, Body, Reason]),
                    {error, #{code => 1, error => list_to_binary(io_lib:format("~p", [Reason]))}}
            end;
        _ ->
            {error, #{code => 1, error => <<"NationCode is illegality">>}}
    end.
