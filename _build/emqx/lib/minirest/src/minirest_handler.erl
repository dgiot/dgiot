%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(minirest_handler).

-export([ init/1
        , dispatch/4
        ]).

-type(config() :: #{apps => list(atom()), modules => list(module())}).

-export_type([config/0]).

-define(LOG(Level, Format, Args), logger:Level("Minirest(Handler): " ++ Format, Args)).

-spec(init(config()) -> {?MODULE, dispatch, list()}).
init(Config) ->
    Routes = lists:map(fun(App) ->
        {ok, Modules} = application:get_key(App, modules),
        routes(App, Config, Modules)
    end, maps:get(apps, Config, [])),
    {?MODULE, dispatch, [lists:flatten([Routes, routes(Config)]), maps:get(filter, Config, undefined)]}.

routes(Config) ->
    routes(undefined, Config, maps:get(modules, Config, [])).

routes(App, Config, Modules) ->
    lists:map(fun(Module) ->
        [API#{module  => Module,
              pattern => string:tokens(Path, "/"),
              app     => App}
         || {rest_api, [API = #{path := Path,
                                name := Name}]} <- Module:module_info(attributes),
                                                   not lists:member(Name, maps:get(except, Config, []))]
    end, Modules).

%% Get API List
dispatch("/", Req, Routes, _Filter) ->
    case binary_to_atom(cowboy_req:method(Req), utf8) of
        'GET' ->
            jsonify(200, [{code, 0}, {data, [format_route(Route) || Route <- Routes]}], Req);
        _ ->
            reply(400, <<"Bad Request">>, Req)
    end;

%% Dispatch request to REST APIs
dispatch(Path, Req, Routes, Filter) ->
    try match_route(binary_to_atom(cowboy_req:method(Req), utf8), Path, Routes) of
        {ok, Route} ->
            dispatch(Req, Route, Filter);
        false ->
            reply(404, <<"Not found.">>, Req)
    catch
        _Error:_Reason ->
            reply(404, <<"Not found.">>, Req)
    end.

dispatch(Req, Route, undefined) ->
    dispatch(Req, Route);

dispatch(Req, Route, Filter) ->
    case Filter(Route) of
        true -> dispatch(Req, Route);
        false -> reply(404, <<"Not found.">>, Req)
    end.

dispatch(Req, #{module := Mod, func := Fun, bindings := Bindings}) ->
    case catch parse_params(Req) of
        {'EXIT', Reason} ->
            error_logger:error_msg("Params error: ~p", [Reason]),
            reply(400, <<"Bad Request">>, Req);
        Params ->
            jsonify(erlang:apply(Mod, Fun, [Bindings, Params]), Req)
    end.

format_route(#{name := Name, method := Method, path := Path, descr := Descr}) ->
    [{name, Name}, {method, Method}, {path, format_path(Path)}, {descr, iolist_to_binary(Descr)}].

%% Remove the :type field.
format_path(Path) ->
    re:replace(Path, <<":[^:]+(:[^/]+)">>, <<"\\1">>, [global, {return, binary}]).

match_route(_Method, _Path, []) ->
    false;
match_route(Method, Path, [Route|Routes]) ->
    case match_route(Method, Path, Route) of
        {ok, Bindings} ->
            {ok, Route#{bindings => Bindings}};
        false ->
            match_route(Method, Path, Routes)
    end;
match_route(Method, Path, #{method := Method, pattern := Pattern}) ->
    match_path(string:tokens(Path, "/"), Pattern, #{});
match_route(_Method, _Path, _Route) ->
    false.

match_path([], [], Bindings) ->
    {ok, Bindings};
match_path([], [_H|_T], _) ->
    false;
match_path([_H|_T], [], _) ->
    false;
match_path([H1|T1], [":" ++ H2|T2], Bindings) ->
    match_path(T1, T2, case string:tokens(H2, ":") of
                           [Type, Name] ->
                               Bindings#{list_to_atom(Name) => parse_var(Type, H1)};
                           [Name] ->
                               Bindings#{list_to_atom(Name) => H1}
                       end);
match_path([H|T1], [H|T2], Bindings) ->
    match_path(T1, T2, Bindings);
match_path(_Path, _Pattern, _Bindings) ->
    false.

parse_params(Req) ->
    QueryParams = cowboy_req:parse_qs(Req),
    BodyParams =
        case cowboy_req:has_body(Req) of
            true  -> {_, Body, _} = cowboy_req:read_body(Req),
                     jsx:decode(Body);
            false -> []
        end,
    QueryParams ++ BodyParams.

parse_var("atom", S) -> list_to_existing_atom(S);
parse_var("int", S)  -> list_to_integer(S);
parse_var("bin", S)  -> iolist_to_binary(S).

jsonify(ok, Req) ->
    jsonify(200, <<"ok">>, Req);
jsonify({ok, Response}, Req) ->
    jsonify(200, Response, Req);
jsonify({error, Reason}, Req) ->
    jsonify(500, Reason, Req);
jsonify({Code, Response}, Req) when is_integer(Code) ->
    jsonify(Code, Response, Req);
jsonify({Code, Headers, Response}, Req) when is_integer(Code) ->
    jsonify(Code, Headers, Response, Req).

jsonify(Code, Response, Req) ->
    jsonify(Code, #{}, Response, Req).
jsonify(Code, Headers, Response, Req) ->
    try jsx:encode(Response) of
        Json ->
            cowboy_req:reply(Code, maps:merge(#{<<"content-type">> => <<"application/json">>}, Headers), Json, Req)
    catch
        error:Reason:_Stacktrace ->
            ?LOG(error, "Encode ~p failed with ~p", [Response, Reason])
    end.

reply(Code, Text, Req) ->
    cowboy_req:reply(Code, #{<<"content-type">> => <<"text/plain">>}, Text, Req).
