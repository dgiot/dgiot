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

-module(dgiot_logger).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").

-export([
    set_loglevel/3,
    send/2,
    test/0]).
%% Logs
-export([debug/1
    , debug/2
    , debug/3
    , info/1
    , info/2
    , info/3
    , warning/1
    , warning/2
    , warning/3
    , error/1
    , error/2
    , error/3
    , critical/1
    , critical/2
    , critical/3
]).

test() ->
    Test = <<"test">>,
%%    ?MLOG(info, #{<<"test">> => Test}),
    ?MLOG(info, #{<<"test">> => Test}, ['acl_test']).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------
-spec(debug(unicode:chardata()) -> ok).
debug(Msg) ->
    emqx_logger:debug(Msg).

-spec(debug(io:format(), [term()]) -> ok).
debug(Format, Args) ->
    emqx_logger:debug(Format, Args).

-spec(debug(logger:metadata(), io:format(), [term()]) -> ok).
debug(Metadata, Format, Args) when is_map(Metadata) ->
    emqx_logger:debug(Format, Args, Metadata).


-spec(info(unicode:chardata()) -> ok).
info(Msg) ->
    emqx_logger:info(Msg).

-spec(info(io:format(), [term()]) -> ok).
info(Format, Args) ->
    emqx_logger:info(Format, Args).

-spec(info(logger:metadata(), io:format(), [term()]) -> ok).
info(Metadata, Format, Args) when is_map(Metadata) ->
    emqx_logger:info(Format, Args, Metadata).


-spec(warning(unicode:chardata()) -> ok).
warning(Msg) ->
    emqx_logger:warning(Msg).

-spec(warning(io:format(), [term()]) -> ok).
warning(Format, Args) ->
    emqx_logger:warning(Format, Args).

-spec(warning(logger:metadata(), io:format(), [term()]) -> ok).
warning(Metadata, Format, Args) when is_map(Metadata) ->
    emqx_logger:warning(Format, Args, Metadata).


-spec(error(unicode:chardata()) -> ok).
error(Msg) ->
    emqx_logger:error(Msg).
-spec(error(io:format(), [term()]) -> ok).
error(Format, Args) ->
    emqx_logger:error(Format, Args).
-spec(error(logger:metadata(), io:format(), [term()]) -> ok).
error(Metadata, Format, Args) when is_map(Metadata) ->
    emqx_logger:error(Format, Args, Metadata).


-spec(critical(unicode:chardata()) -> ok).
critical(Msg) ->
    logger:critical(Msg).

-spec(critical(io:format(), [term()]) -> ok).
critical(Format, Args) ->
    logger:critical(Format, Args).

-spec(critical(logger:metadata(), io:format(), [term()]) -> ok).
critical(Metadata, Format, Args) when is_map(Metadata) ->
    logger:critical(Format, Args, Metadata).


send(Meta, Payload) when is_list(Payload) ->
    send(Meta, iolist_to_binary(Payload));

send(#{error_logger := _Error_logger}, Payload) ->
    case jsx:is_json(Payload) of
        true ->
            Map = jiffy:decode(Payload, [return_maps]),
            Mfa = maps:get(<<"mfa">>, Map, <<"all">>),
            Line = get_line(Map),
            Topic = <<"$SYS/log/", Mfa/binary, "/", Line/binary>>,
            NewMap = maps:with([<<"time">>, <<"pid">>, <<"msg">>, <<"mfa">>, <<"line">>, <<"level">>, <<"clientid">>, <<"topic">>, <<"peername">>], Map),
            dgiot_mqtt:publish(Mfa, Topic, get_body(NewMap, [error_logger]));
        false ->
            Topic1 = <<"$SYS/error_logger/all">>,
            dgiot_mqtt:publish(self(), Topic1, Payload)
    end;

send(#{topic := _Topic} = Meta, Payload) ->
    send(Meta#{domain => [topic_trace]}, Payload);

send(#{clientid := _ClientId} = Meta, Payload) ->
    send(Meta#{domain => [clientid_trace]}, Payload);

send(Meta, Payload) ->
    Map = jiffy:decode(Payload, [return_maps]),
    Mfa = maps:get(<<"mfa">>, Map, <<"all">>),
    Domain = maps:get(domain, Meta, [public_log]),
    TraceTopic =
        case maps:find(<<"topic">>, Map) of
            {ok, TraceTopic1} ->
                <<TraceTopic1/binary, "/">>;
            _ -> <<"">>
        end,
    Topic =
        case maps:find(<<"clientid">>, Map) of
            {ok, ClientId1} ->
                <<"$SYS/trace/", TraceTopic/binary, ClientId1/binary>>;
            _ ->
                Line = get_line(Map),
                <<"$SYS/log/", Mfa/binary, "/", Line/binary>>
        end,
    NewMap = maps:with([<<"time">>, <<"pid">>, <<"msg">>, <<"mfa">>, <<"line">>, <<"level">>, <<"clientid">>, <<"topic">>, <<"peername">>], Map),
    dgiot_mqtt:publish(Mfa, Topic, jiffy:encode(get_body(NewMap, Domain))).

get_body(#{<<"msg">> := Msg} = Map, Domain) when is_map(Msg) ->
    Map#{<<"type">> => <<"json">>, <<"domain">> => Domain};
get_body(Map, Domain) ->
    Map#{<<"type">> => <<"text">>, <<"domain">> => Domain}.


get_line(Map) ->
    case maps:find(<<"line">>, Map) of
        {ok, Line1} ->
            dgiot_utils:to_binary(Line1);
        _ -> <<"0">>
    end.


%% 获取系统日志等级  emqx_logger:get_primary_log_level().
%% 设置系统日志等级  emqx_logger:set_log_level(debug).

%% 获取app日志等级  emqx_logger:get_primary_log_level().
%% 设置app日志等级  logger:set_application_level(dgiot,debug).

%% 获取module日志等级  logger:get_module_level(dgiot)
%% 设置module日志等级  logger:set_module_level(dgiot,debug)
set_loglevel(<<"system">>, <<"dgiot">>, Level) ->
    emqx_logger:set_log_level(binary_to_atom(Level));

set_loglevel(<<"app">>, Name, Level) ->
    logger:set_application_level(binary_to_atom(Name), binary_to_atom(Level));

set_loglevel(<<"module">>, Name, Level) ->
    logger:set_module_level(binary_to_atom(Name), binary_to_atom(Level));

set_loglevel(Type, _Name, _Level) ->
    {error, <<Type/binary, " error">>}.
