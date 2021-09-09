-module(dgiot_showdoc).
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_http.hrl").

-export([send/4, save/1]).

%%接受到请求
send(Req, _ResHTTPCode, _ResHeaders, ResBody) ->
    Path = Req:get(path),
    PathList = string:tokens(Path, "/"),
    Len = length(PathList),
    N = Len - 1,
    Mod = lists:nth(N, PathList),
    Fun = lists:nth(Len, PathList),
    Url = "http://[ip]:[port]/iotapi/functions/" ++ Fun,
    Method = Req:get(method),
    Param =
        case Method of
            'GET' ->
                GetParam = Req:parse_qs(),
                [{<<"mod">>, list_to_binary(Mod)}, {<<"version">>, <<"v1">>} | [{list_to_binary(A), list_to_binary(B)} || {A, B} <- GetParam]];
            'POST' ->
                [{<<"mod">>, list_to_binary(Mod)}, {<<"version">>, <<"v1">>} | Req:parse_post()]
        end,
    save(#{
        <<"method">> => list_to_binary(atom_to_list(Method)),
        <<"name">> => list_to_binary(Mod ++ "_" ++ Fun),
        <<"path">> => list_to_binary(Path),
        <<"url">> => list_to_binary(Url),
        <<"param">> => Param,
        <<"return">> => ResBody}).

save(#{<<"method">> := Method,
    <<"name">> := Name,
    <<"path">> := Path,
    <<"url">> := Url,
    <<"param">> := Param,
    <<"return">> := Return}) ->
    Param1 = [#{
        <<"key">> => Key,
        <<"desc">> => <<>>,
        <<"type">> => <<>>,
        <<"required">> => <<>>
    } || {Key, _} <- Param],
    Return1 = [#{
        <<"key">> => Key,
        <<"desc">> => <<>>,
        <<"type">> => <<>>,
        <<"required">> => <<>>
    } || {Key, _} <- jiffy:decode(Return)],
    Parse = #{
        <<"catalog">> => <<>>,
        <<"title">> => <<>>,
        <<"description">> => <<>>,
        <<"path">> => dgiot_utils:to_md5(Path),
        <<"method">> => Method,
        <<"url">> => Url,
        <<"param">> => Param1,
        <<"return">> => Return,
        <<"return_param">> => Return1},
    to_content(Parse#{<<"name">> => Name}),
    case dgiot_parse:query_object(<<"Api">>,
        #{<<"where">> => #{
            <<"path">> => dgiot_utils:to_md5(Path)},
            <<"keys">> => <<"objectId">>,
            <<"limit">> => <<"1">>}) of
        {error, Reson} ->
            {error, Reson};
        {ok, #{<<"results">> := MapList}} ->
            case MapList of
                [] ->
                    dgiot_parse:create_object(<<"Api">>, Parse);
                [#{<<"objectId">> := ObjectId} | _] ->
                    dgiot_parse:update_object(<<"Api">>, ObjectId, Parse)
            end
    end.

to_content(#{
    <<"catalog">> := Catalog,
    <<"title">> := Title,
    <<"description">> := Desc,
    <<"name">> := Name,
    <<"method">> := Method,
    <<"url">> := Url,
    <<"param">> := Param,
    <<"return">> := Return,
    <<"return_param">> := Return1}) ->
    L1 = "%%/\r\n",
    L2 = "% showdoc\r\n",
    L3 = "% @catalog " ++ binary_to_list(Catalog),
    L4 = "% @title " ++ binary_to_list(Title),
    L5 = "% @description " ++ binary_to_list(Desc),
    L6 = "% @method " ++ binary_to_list(Method),
    L7 = "% @url " ++ binary_to_list(Url),
    L8 = "% @return " ++ binary_to_list(Return),
    L9 = "% @remark",
    L10 = "% @number 99",
    L11 = "%/\r\n",
    PL = ["% @param " ++ binary_to_list(K) ++ " " ++ binary_to_list(R) ++ " " ++ binary_to_list(T) ++ " " ++ binary_to_list(D) || #{
        <<"key">> := K,
        <<"desc">> := D,
        <<"type">> := T,
        <<"required">> := R} <- Param],
    RL = ["% @return_param " ++ binary_to_list(K) ++ " " ++ binary_to_list(T) ++ " " ++ binary_to_list(D) || #{
        <<"key">> := K,
        <<"desc">> := D,
        <<"type">> := T,
        <<"required">> := _R} <- Return1],
    L = [L1, L2, L3, L4, L5, L6, L7] ++ PL ++ [L8 | RL] ++ [L9, L10, L11],
    Cont = lists:concat([A ++ "\r\n" || A <- L]),
    {file, Here} = code:is_loaded(?MODULE),
    FilePath = dgiot_utils:to_list(Here) ++ binary_to_list(Name) ++ ".txt",
    file:write_file(FilePath, Cont),
    ?LOG(info, "============write file success====~p=======]~n", [FilePath]).

% showdoc
% @catalog 测试文档/用户相关
% @title 用户注册
% @description 用户注册的接口
% @method get
% @url http://doc.iotn2n.com/home/user/login
% @param username 必选 string 用户名
% @param password 必选 string 密码
% @param name 可选 string 用户昵称
% @return {"error_code":0,"data":{"uid":"1","username":"12154545","name":"吴系挂","groupid":2,"reg_time":"1436864169","last_login_time":"0"}}
% @return_param groupid int 用户组id
% @return_param name string 用户昵称
% @remark 这里是备注信息
% @number 99
%/
