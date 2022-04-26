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

-module(dgiot_rest).
-include("dgiot_api.hrl").
-include_lib("dgiot/include/logger.hrl").
-author("johnliu").

%% basic handler

%% Cowboy REST callbacks
-export([allowed_methods/2]).
-export([init/2]).
-export([allow_missing_post/2]).
-export([content_types_accepted/2]).
-export([charsets_provided/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([is_authorized/2]).
-export([known_content_type/2]).
-export([malformed_request/2]).
-export([valid_content_headers/2]).
-export([valid_entity_length/2]).

%% Handlers
-export([handle_request/2]).
-export([handle_multipart/2]).
-export([call/3]).

-dgiot_data("ets").
-export([init_ets/0, get_log/4, get_OperationID/2]).

-record(state, {
    operationid :: atom(),
    is_mock :: boolean(),
    logic_handler :: atom(),
    context = #{} :: #{}
}).

-type state() :: state().
-type response() :: {Result :: stop | binary(), dgiot_req:req(), state()}.


%% Common handler callbacks.
-callback init(Req :: dgiot_req:req(), Config :: map()) ->
    {Mod :: module(), NewReq :: dgiot_req:req(), Context :: map()}.
-optional_callbacks([init/2]).

-callback check_auth(Args :: map(), Req :: dgiot_req:req()) ->
    {true, Context :: #{binary() => any()}, Req :: dgiot_req:req()} |
    {false, Result :: #{binary() => any()}, Req :: dgiot_req:req()} |
    {switch_handler, Mod :: module(), Req :: dgiot_req:req()}.
-optional_callbacks([check_auth/2]).

-callback handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: dgiot_req:req()) ->
    {Code :: dgiot_req:http_status(), Header :: dgiot_req:http_headers(), Body :: map()}.


-spec init(Req :: dgiot_req:req(), Opts :: map()) ->
    {cowboy_rest, Req :: dgiot_req:req(), Opts :: state()}.
init(Req, #{logic_handler := LogicHandler} = Map) ->
    State = #state{
        logic_handler = LogicHandler
    },
    Method = dgiot_req:method(Req),
    case Method of
        <<"OPTIONS">> ->
            default_init(#{operationid => options}, State, Req);
        _ ->
            case call(LogicHandler, init, [Req, Map]) of
                {no_call, Req1} ->
                    Index = maps:get(Method, Map),
                    {ok, {_, Config}} = dgiot_router:get_state(Index),
                    default_init(Config, State, Req1);
                no_call ->
                    Index = maps:get(Method, Map),
                    {ok, {_, Config}} = dgiot_router:get_state(Index),
                    default_init(Config, State, Req);
                {?MODULE, Req1, NewConfig} ->
                    default_init(NewConfig, State, Req1)
            end
    end.


default_init(Config, State, Req) ->
    Path = dgiot_req:path(Req),
    IsMock =
        case Path of
            <<"/mock/", _/binary>> -> true;
            _ -> false
        end,
    OperationId = maps:get(operationid, Config, not_allowed),
    {cowboy_rest, Req, State#state{
        operationid = OperationId,
        is_mock = IsMock,
        context = maps:without([operationid], Config)
    }}.


-spec allowed_methods(Req :: dgiot_req:req(), State :: state()) ->
    {Value :: [binary()], Req :: dgiot_req:req(), State :: state()}.
allowed_methods(Req, State = #state{operationid = OperationId}) ->
    Method = dgiot_req:method(Req),
    case Method == <<"OPTIONS">> orelse OperationId =/= not_allowed of
        true ->
            {[Method], Req, State};
        false ->
            {[], Req, State}
    end.


-spec is_authorized(Req :: dgiot_req:req(), State :: state()) ->
    {
        Value :: true | {false, AuthHeader :: iodata()},
        Req :: dgiot_req:req(),
        State :: state()
    }.
is_authorized(Req0, #state{operationid = options} = State) ->
    case ?ACCESS_CONTROL_ALLOW_HEADERS of
        false ->
            dgiot_req:reply(403, ?HEADER, <<>>, Req0);
        Header ->
            do_response(200, ?HEADER#{
                <<"access-control-allow-headers">> => Header
            }, #{}, Req0, State)
    end;

is_authorized(Req0, State = #state{
    is_mock = IsMock,
    operationid = OperationID,
    logic_handler = LogicHandler,
    context = Context
}) ->
    case IsMock of
        true ->
            {true, Req0, State};
        false ->
            AuthList = maps:get(authorize, Context, []),
            AuthOperationID = get_OperationID(OperationID, LogicHandler),
%%            io:format("~s ~p ~p ~p ~p ~n",[?FILE, ?LINE, OperationID, AuthOperationID, LogicHandler]),
            CheckResult = dgiot_auth:pre_check(AuthOperationID, LogicHandler, AuthList, Req0),
            case CheckResult of
                {ok, Args, Req} ->
                    case do_authorized(LogicHandler, AuthOperationID, Args, Req) of
                        {true, NContext, Req1} ->
                            {true, Req1, State#state{context = maps:merge(Context, NContext)}};
                        {forbidden, Err, Req1} when AuthList =/= [] ->
                            do_response(403, #{}, Err, Req1, State);
                        {false, Err, Req1} when AuthList =/= [] ->
                            do_response(401, #{}, Err, Req1, State);
                        {_, _Err, Req1} ->
                            {true, Req1, State}
                    end;
                {error, Err, Req1} when AuthList =/= [] ->
                    do_response(401, #{}, #{error => Err}, Req1, State);
                {error, _Err, Req1} ->
                    {true, Req1, State}
            end
    end.

get_OperationID(OperationID, LogicHandler) ->
    case proplists:get_value(exports,LogicHandler:module_info()) of
        undefined ->
            OperationID;
        Exports ->
            case proplists:get_value(get_OperationID, Exports) of
                undefined -> OperationID;
                _ ->
                    {_Type, NewOperationID} = LogicHandler:get_OperationID(OperationID),
                    dgiot_utils:to_atom(NewOperationID)
            end
    end.

-spec content_types_accepted(Req :: dgiot_req:req(), State :: state()) ->
    {
        Value :: [{binary(), AcceptResource :: atom()}],
        Req :: dgiot_req:req(),
        State :: state()
    }.
content_types_accepted(Req, #state{context = #{consumes := Consumes}} = State) ->
    Accepts =
        lists:foldl(
            fun
                (<<"*">>, Accepted) -> [{'*', handle_request} | Accepted];
                (<<"multipart/form-data">>, Accepted) -> [{'*', handle_multipart} | Accepted];
                (Consume, Accepted) -> [{Consume, handle_request} | Accepted]
            end, [], Consumes),
    {Accepts, Req, State}.


-spec valid_content_headers(Req :: dgiot_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: dgiot_req:req(), State :: state()}.
valid_content_headers(Req, State) ->
    {true, Req, State}.


-spec content_types_provided(Req :: dgiot_req:req(), State :: state()) ->
    {
        Value :: [{binary(), ProvideResource :: atom()}],
        Req :: dgiot_req:req(),
        State :: state()
    }.
content_types_provided(Req, #state{context = #{produces := Produces}} = State) ->
    Product =
        lists:foldl(
            fun
                (<<"*">>, Accepted) -> [{'*', handle_request} | Accepted];
                (Consume, Accepted) -> [{Consume, handle_request} | Accepted]
            end, [], Produces),
    {Product, Req, State}.


charsets_provided(Req, State) ->
    {[<<"UTF-8">>], Req, State}.


-spec malformed_request(Req :: dgiot_req:req(), State :: state()) ->
    {Value :: false, Req :: dgiot_req:req(), State :: state()}.
malformed_request(Req, State) ->
    {false, Req, State}.


-spec allow_missing_post(Req :: dgiot_req:req(), State :: state()) ->
    {Value :: false, Req :: dgiot_req:req(), State :: state()}.
allow_missing_post(Req, State) ->
    {false, Req, State}.


-spec delete_resource(Req :: dgiot_req:req(), State :: state()) -> response().
delete_resource(Req, State) ->
    handle_request(Req, State).


-spec known_content_type(Req :: dgiot_req:req(), State :: state()) ->
    {Value :: true, Req :: dgiot_req:req(), State :: state()}.
known_content_type(Req, State) ->
    {true, Req, State}.


-spec valid_entity_length(Req :: dgiot_req:req(), State :: state()) ->
    {Value :: true, Req :: dgiot_req:req(), State :: state()}.
valid_entity_length(Req, State) ->
    %% @TODO check the length
    {true, Req, State}.


-spec handle_multipart(dgiot_req:req(), state()) -> response().
handle_multipart(Req, State) ->
    case handle_multipart(Req, #{}, State) of
        {ok, Populated, Req1} ->
            case catch do_request(Populated, Req1, State) of
                {'EXIT', Reason} ->
                    Err = list_to_binary(io_lib:format("~p", [Reason])),
                    do_response(500, #{}, #{error => Err}, Req, State);
                {Status, Body} ->
                    do_response(Status, #{}, Body, Req, State);
                {Status, Headers, Body} ->
                    do_response(Status, Headers, Body, Req, State);
                {Status, Headers, Body, NewReq} ->
                    do_response(Status, Headers, Body, NewReq, State)
            end;
        {error, Reason} ->
            Err = list_to_binary(io_lib:format("~p", [Reason])),
            do_response(500, #{}, #{error => Err}, Req, State)
    end.

-spec handle_request(dgiot_req:req(), state()) -> response().
handle_request(Req, #state{context = Context} = State) ->
    Version = maps:get(version, Context),
    CurVersion = dgiot_req:get_qs(<<"version">>, Req, Version),
    NewState = State#state{context = Context#{version => CurVersion}},
    try
        {ok, Populated, Req1} = dgiot_rest_check:check_request(Context, Req),
        case do_request(Populated, Req1, NewState) of
            {Status, Body} ->
                do_response(Status, #{}, Body, Req, State);
            {Status, Headers, Body} ->
                do_response(Status, Headers, Body, Req, State);
            {Status, Headers, Body, NewReq} ->
                do_response(Status, Headers, Body, NewReq, State)
        end
    catch
        throw:{wrong_param, _Name, required, Reason} ->
            do_response(400, #{}, #{error => Reason}, Req, State);
        throw:{wrong_param, Reason, schema, wrong_type} ->
            Err = list_to_binary(io_lib:format("~p, wrong type", [Reason])),
            do_response(500, #{}, #{error => Err}, Req, State);
        Reason ->
            Err = list_to_binary(io_lib:format("~p", [Reason])),
            do_response(500, #{}, #{error => Err}, Req, State)
    end.

do_request(Populated, Req0, State = #state{
    operationid = OperationID,
    is_mock = IsMock,
    logic_handler = LogicHandler,
    context = Context
}) ->
    Args = [OperationID, Populated, Context, Req0],
    ?LOG(debug, "~p ~p", [OperationID, dgiot_data:get(?DGIOT_SWAGGER, OperationID)]),
    Result =
        case IsMock of
            true ->
                call(LogicHandler, mock, Args);
            false ->
                call(LogicHandler, handle, Args)
        end,
    case Result of
        no_call when IsMock ->
            default_mock_handler(OperationID, Populated, Context, Req0);
        no_call ->
            Why = list_to_binary(io_lib:format("~p function handle not exported", [LogicHandler])),
            do_response(500, #{}, #{error => Why}, Req0, State);
        _ ->
            Result
    end.


handle_multipart(Req, Acc, State) ->
    case cowboy_req:read_part(Req) of
        undefined ->
            {ok, Acc, Req};
        {ok, Headers, Req1} ->
            handle_multipart(cow_multipart:form_data(Headers), Req1, Acc, State);
        {done, Req1} ->
            {ok, Acc, Req1}
    end.
handle_multipart({data, Name}, Req, Acc, State) ->
    {ok, Data, Req1} = cowboy_req:read_part_body(Req),
    handle_multipart(Req1, Acc#{Name => Data}, State);
handle_multipart({file, Name, Filename, ContentType}, Req, Acc, State) ->
    DocRoot = list_to_binary(dgiot_http_server:get_env(?APP, docroot)),
    {{Y, M, D}, {H, N, S}} = calendar:local_time(),
    Now = list_to_binary(lists:concat([Y, M, D, H, N, S])),
    Exe = filename:extension(Filename),
    FilePath = <<"upload/", Now/binary, Exe/binary>>,
    Path = filename:join([DocRoot, FilePath]),
    case filelib:ensure_dir(Path) of
        ok ->
            {ok, Bin, Req1} = cowboy_req:read_part_body(Req),
            case file:write_file(Path, Bin, [append]) of
                ok ->
                    handle_multipart(Req1, Acc#{
                        Name => #{
                            <<"path">> => <<"/", FilePath/binary>>,
                            <<"fullpath">> => Path,
                            <<"filename">> => Filename,
                            <<"contentType">> => ContentType
                        }}, State);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


do_authorized(LogicHandler, OperationID, Args, Req) ->
%%    io:format("~s ~p   LogicHandler = ~p OperationID = ~p ~n", [?FILE, ?LINE, LogicHandler, OperationID]),
    case call(LogicHandler, check_auth, [OperationID, Args, Req]) of
        no_call ->
            dgiot_auth:check_auth(OperationID, Args, Req);
        {true, NContext, Req1} ->
            {true, NContext, Req1};
        {forbidden, Err, Req1} ->
            {forbidden, Err, Req1};
        {false, Err, Req1} ->
            {false, Err, Req1};
        {switch_handler, NLogicHandler, Req1} ->
            call(NLogicHandler, check_auth, [OperationID, Args, Req1])
    end.

default_mock_handler(_OperationID, _Populated, Context, Req) ->
    Response = maps:get(check_response, Context, #{}),
    ?LOG(info, "do mock:~p~n", [Response]),
    dgiot_mock:do_mock(Response, Req).


do_response(Status, Headers, Body, Req, State) when is_map(Body); is_list(Body) ->
    %% @todo 这个地方有点问题
    %% catch dgiot_rest_check:check_response(Status, State#state.context, Body),
    do_response(Status, Headers, jsx:encode(Body), Req, State);
do_response(Status, Headers, Body, Req0, State) when is_binary(Body) ->
    NewHeaders = maps:merge(Headers, ?HEADER),
    Req =
        case Status == 500 of
            true ->
                case application:get_env(?APP, developer_mode, false) of
                    true ->
                        ?LOG(info, "Response 500:~p~n", [Body]),
                        dgiot_req:reply(Status, NewHeaders, Body, Req0);
                    false ->
                        Res = #{<<"error">> => <<"Server Internal error">>},
                        dgiot_req:reply(Status, NewHeaders, jsx:encode(Res), Req0)
                end;
            false ->
                dgiot_req:reply(Status, NewHeaders, Body, Req0)
        end,
    {stop, Req, State}.

call(Mod, Fun, Args) ->
    case erlang:module_loaded(Mod) of
        true ->
            case erlang:function_exported(Mod, Fun, length(Args)) of
                true ->
                    {Time, Result} = timer:tc(Mod, Fun, Args),
                    get_log(Fun, Args, Time, Result),
                    Result;
                false ->
                    no_call
            end;
        false ->
            no_module
    end.

init_ets() ->
    dgiot_data:init(?DGIOT_SWAGGER).

get_log(handle, [_OperationID, Body, #{<<"sessionToken">> := SessionToken} = _Context, Req], Time, Result)
    when is_map(Body) ->
    {Username, Acl} =
        case dgiot_auth:get_session(SessionToken) of
            #{<<"username">> := Name, <<"ACL">> := Acl1} -> {Name, Acl1};
            _ -> {<<"">>, #{}}
        end,
    log(Req, Time, Result, Body#{<<"username">> => Username, <<"ACL">> => Acl});
get_log(check_auth, [_OperationID, Args, Req], Time, Result) ->
    log(Req, Time, Result, Args);
get_log(_Fun, _, _, _) ->
    pass.

log(#{peer := {PeerName, _}, headers := Headers} = Req, Time, Result, Map) when is_map(Map) ->
    Ip = dgiot_utils:get_ip(PeerName),
    RealIp = maps:get(<<"x-real-ip">>, Headers, Ip),
    NewReq = maps:with([method, path], Req),
    UserName = maps:get(<<"username">>, Map, <<"">>),
    Body = maps:without([<<"username">>, <<"password">>], Map),
    {Code, Reason} =
        case Result of
            {200, _, _, _} ->
                {200, <<"success">>};
            {200, _, _} ->
                {200, <<"success2">>};
            {200, _} ->
                {200, <<"success3">>};
            _ ->
                {<<"error">>, <<"error">>}
        end,
    ?MLOG(debug, NewReq#{
        <<"code">> => Code,
        <<"reason">> => Reason,
        <<"ip">> => RealIp,
        <<"username">> => UserName,
        <<"body">> => Body,
        <<"elapsedtime">> => Time},
        ['parse_api']);

log(_Req, _Time, _Result, _Map) ->
    pass.




