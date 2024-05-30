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

-module(dgiot_csv).
-author("johnliu").
-include("dgiot.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([
    read_from_csv/2
    , save_csv_ets/2
    , read_csv/3
    , save_csv_ets/1
    , post_properties/2
]).

read_from_csv(Path, Fun) ->
    case file:open(Path, [read]) of
        {ok, IoDevice} ->
            R = read_csv(IoDevice, Fun, ","),
            file:close(IoDevice),
            R;
        {error, Reason} ->
            {error, Reason}
    end.

read_csv(IoDevice, Fun, Delimiter) ->
    case file:read_line(IoDevice) of
        {ok, Row} ->
            Cols = [list_to_binary(Col) || Col <- string:tokens(lists:sublist(Row, 1, length(Row) - 1), Delimiter)],
            Fun(Cols),
            read_csv(IoDevice, Fun, Delimiter);
        eof ->
            {ok, read_complete};
        {error, Reason} ->
            ?LOG(error, "~p", [Reason])
    end.

save_csv_ets(Module, FilePath) ->
    Url = "http://127.0.0.1:1250" ++ dgiot_utils:to_list(FilePath),
    <<FileName:10/binary, _/binary>> = dgiot_utils:to_md5(FilePath),
    {file, Here} = code:is_loaded(Module),
    DownloadPath = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/csv/"]) ++ dgiot_utils:to_list(FileName) ++ ".csv",
    os:cmd("rm -rf " ++ DownloadPath),
    case dgiot_httpc:download(Url, DownloadPath) of
        {ok, saved_to_file} ->
            AtomName = dgiot_utils:to_atom(FileName),
            dgiot_data:delete(AtomName),
            dgiot_data:init(AtomName),
            put(count, -1),
            Fun = fun(X) ->
                Count = get(count),
                case Count > 0 of
                    true ->
                        dgiot_data:insert(AtomName, Count, X ++ [0]);
                    _ ->
                        pass
                end,
                put(count, Count + 1)
                  end,
            read_from_csv(DownloadPath, Fun),
            FileName;
        _ ->
            FileName
    end.


save_csv_ets(#{<<"fullpath">> := Fullpath}) ->
    <<FileName:10/binary, _/binary>> = dgiot_utils:to_md5(Fullpath),
    AtomName = dgiot_utils:to_atom(FileName),
    dgiot_data:delete(AtomName),
    dgiot_data:init(AtomName),
    put(count, -1),
    Fun = fun(X) ->
        Count = get(count),
        case Count > 0 of
            true ->
                dgiot_data:insert(AtomName, Count, X ++ [0]);
            _ ->
                pass
        end,
        put(count, Count + 1)
          end,
    read_from_csv(Fullpath, Fun),
    AtomName.


post_properties(<<"plc">>, AtomName) ->
    Things = ets:match(AtomName, {'$1', ['$2', '$3', '$4', '$5', '$6', '$7', '$8', '$9', '$10', '$11' | '_']}),
    lists:foldl(fun([Index, Devicetype, Name, Identifier, Address, Originaltype, AccessMode, Min_Max, Unit, Type, Specs | _], Acc) ->
        Acc ++ [#{
            <<"name">> => Name,
            <<"index">> => Index,
            <<"isstorage">> => true,
            <<"isshow">> => true,
            <<"dataForm">> => #{
                <<"address">> => <<"0">>,
                <<"rate">> => 1,
                <<"order">> => Index,
                <<"round">> => <<"all">>,
                <<"offset">> => 0,
                <<"control">> => <<"%{d}">>,
                <<"iscount">> => <<"0">>,
                <<"protocol">> => <<"S7">>,
                <<"strategy">> => <<"1">>,
                <<"collection">> => <<"%{s}">>,
                <<"countround">> => <<"all">>,
                <<"countstrategy">> => 3,
                <<"countcollection">> => <<"%{s}">>
            },
            <<"dataType">> => get_dataType(to_lower(Type), Min_Max, Unit, Specs),
            <<"required">> => true,
            <<"accessMode">> => get_accessmode(AccessMode),
            <<"dataSource">> => #{
                <<"_dlinkindex">> => <<"">>,
                <<"address">> => Address,
                <<"originaltype">> => Originaltype
            },
            <<"devicetype">> => Devicetype,
            <<"identifier">> => to_lower(Identifier),
            <<"moduleType">> => <<"properties">>,
            <<"isaccumulate">> => false
        }]
                end, [], Things);

post_properties(<<"dlink">>, AtomName) ->
    Things = ets:match(AtomName, {'$1', ['$2', '$3', '$4', '$5', '$6', '$7', '$8', '$9', '$10', '$11' | '_']}),
    lists:foldl(fun([Index, Devicetype, Name, Identifier, Key, Len, AccessMode, Min_Max, Unit, Type, Specs | _], Acc) ->
        Acc++ [#{
                <<"name">> => Name,
                <<"index">> => Index,
                <<"isstorage">> => true,
                <<"isshow">> => true,
                <<"dataForm">> => #{
                    <<"address">> => <<"0">>,
                    <<"rate">> => 1,
                    <<"order">> => 0,
                    <<"round">> => <<"all">>,
                    <<"offset">> => 0,
                    <<"control">> => <<"%{d}">>,
                    <<"iscount">> => <<"0">>,
                    <<"protocol">> => <<"DLINK">>,
                    <<"strategy">> => <<"主动上报"/utf8>>,
                    <<"collection">> => <<"%{s}">>,
                    <<"countround">> => <<"all">>,
                    <<"countstrategy">> => 3,
                    <<"countcollection">> => <<"%{s}">>
                },
                <<"dataType">> => get_dataType(to_lower(Type), Min_Max, Unit, Specs),
                <<"required">> => true,
                <<"accessMode">> => get_accessmode(AccessMode),
                <<"dataSource">> => #{
                    <<"_dlinkindex">> => <<"1">>,
                    <<"dis">> => [
                        #{<<"key">> => Key, <<"data">> => Len}
                    ]
                },
                <<"devicetype">> => Devicetype,
                <<"identifier">> => to_lower(Identifier),
                <<"moduleType">> => <<"properties">>,
                <<"isaccumulate">> => false
            }]
                end, [], Things);

post_properties(<<"modbusxtcp">>, AtomName) ->
    Things = ets:match(AtomName, {'$1', ['$2', '$3', '$4', '$5', '$6', '$7', '$8', '$9', '$10' | '_']}),
    lists:foldl(fun([Index, Devicetype, Name, Identifier, Address, Min_Max, Unit, Type, Originaltype, Specs | _], Acc) ->
        Acc++ [#{
                <<"name">> => Name,
                <<"index">> => Index,
                <<"isstorage">> => true,
                <<"isshow">> => true,
                <<"dataForm">> => #{
                    <<"address">> => <<"0">>,
                    <<"rate">> => 1,
                    <<"order">> => 0,
                    <<"round">> => <<"all">>,
                    <<"offset">> => 0,
                    <<"control">> => <<"%{d}">>,
                    <<"iscount">> => <<"0">>,
                    <<"protocol">> => <<"MODBUSXTCP">>,
                    <<"strategy">> => <<"主动上报"/utf8>>,
                    <<"collection">> => <<"%{s}">>,
                    <<"countround">> => <<"all">>,
                    <<"countstrategy">> => 3,
                    <<"countcollection">> => <<"%{s}">>
                },
                <<"dataType">> => get_dataType(to_lower(Type), Min_Max, Unit, Specs),
                <<"required">> => true,
                <<"accessMode">> =>  <<"r">>,
                <<"dataSource">> => #{
                    <<"_dlinkindex">> => <<"">>,
                    <<"address">> => Address,
                    <<"originaltype">> => Originaltype
                },
                <<"devicetype">> => Devicetype,
                <<"identifier">> => to_lower(Identifier),
                <<"moduleType">> => <<"properties">>,
                <<"isaccumulate">> => false
            }]
                end, [], Things);

post_properties(_, _) ->
    error.

get_accessmode(<<229, 143, 170, 232, 175, 187>>) ->
    <<"r">>;

get_accessmode(_AccessMode) ->
    <<"rw">>.

to_lower(Value) ->
    Str1 = re:replace(Value, <<"\\.">>, <<"_">>, [global, {return, list}]),
    list_to_binary(string:to_lower(Str1)).

get_min_max(Min_Max) ->
    case binary:split(Min_Max, <<$->>, [global, trim]) of
        [<<>>, Min, Max] ->
            {-dgiot_utils:to_int(Min), dgiot_utils:to_int(Max)};
        [Min, Max] ->
            {dgiot_utils:to_int(Min), dgiot_utils:to_int(Max)};
        _ ->
            {-65535, 65535}
    end.

get_dataType(<<"float">>, Min_Max, Unit, _) ->
    {Min, Max} = get_min_max(Min_Max),
    #{
        <<"das">> => [],
        <<"type">> => <<"float">>,
        <<"specs">> => #{
            <<"min">> => Min,
            <<"max">> => Max,
            <<"step">> => 0,
            <<"unit">> => get_unit(Unit),
            <<"precision">> => 3
        }
    };

get_dataType(<<"enum">>, _, _, Specs) ->
    Newspecs = get_specs(Specs),
    #{
        <<"das">> => [],
        <<"type">> => <<"enum">>,
        <<"specs">> => Newspecs
    };

get_dataType(Type, Min_Max, Unit, _) ->
    {Min, Max} = get_min_max(Min_Max),
    #{
        <<"das">> => [],
        <<"type">> => Type,
        <<"specs">> => #{
            <<"min">> => Min,
            <<"max">> => Max,
            <<"step">> => 0,
            <<"unit">> => get_unit(Unit),
            <<"precision">> => 3
        }
    }.


get_specs(Specs) ->
    case binary:split(Specs, <<$;>>, [global, trim]) of
        List when length(List) > 0 ->
            lists:foldl(fun(Map, Acc) ->
                case binary:split(Map, <<$:>>, [global, trim]) of
                    [Key, Value] ->
                        Acc#{Key => Value};
                    _ ->
                        Acc
                end
                        end, #{}, List);
        _ ->
            #{}
    end.

get_unit(<<"null">>) ->
    <<"">>;

get_unit(Unit) ->
    Unit.
