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

%% Logs
-export([ debug/1
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
