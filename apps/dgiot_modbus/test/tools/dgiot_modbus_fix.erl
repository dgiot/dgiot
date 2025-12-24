%%%-------------------------------------------------------------------
%%% @doc
%%% Modbus产品配置修复模块
%%% 
%%% 功能：修复角度属性的dataSource配置
%%% 
%%% 使用方式：
%%% 1. 编译：_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'
%%% 2. 运行：_build/emqx/rel/emqx/bin/emqx eval 'dgiot_modbus_fix:run().'
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_modbus_fix).

-export([run/0, fix_angular_properties/0, verify_fix/0]).

-include_lib("dgiot/include/logger.hrl").

%% 产品ID
-define(PRODUCT_ID, <<"feeb43bffb">>).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 运行修复流程
run() ->
    ?LOG(info, "开始修复Modbus产品配置..."),
    
    case fix_angular_properties() of
        ok ->
            ?LOG(info, "产品配置修复成功"),
            verify_fix(),
            ok;
        {error, Reason} ->
            ?LOG(error, "产品配置修复失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 修复角度属性配置
fix_angular_properties() ->
    ?LOG(info, "获取产品配置..."),
    
    case dgiot_parse:get_object(<<"Product">>, ?PRODUCT_ID) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props} = Thing}} ->
            ?LOG(info, "当前属性数量: ~p", [length(Props)]),
            
            % 修复角度属性
            FixedProps = fix_properties(Props),
            
            % 更新产品配置
            NewThing = Thing#{<<"properties">> => FixedProps},
            
            ?LOG(info, "更新产品配置..."),
            case dgiot_parse:update_object(<<"Product">>, ?PRODUCT_ID, #{<<"thing">> => NewThing}) of
                {ok, _} ->
                    ?LOG(info, "产品配置更新成功"),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "更新失败: ~p", [Reason]),
                    {error, Reason}
            end;
        Error ->
            ?LOG(error, "获取产品失败: ~p", [Error]),
            Error
    end.

%% @doc 验证修复结果
verify_fix() ->
    ?LOG(info, "验证修复结果..."),
    
    case dgiot_product:lookup_prod(?PRODUCT_ID) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foreach(fun(Prop) ->
                Identifier = maps:get(<<"identifier">>, Prop, <<>>),
                case lists:member(Identifier, [<<"angular_x">>, <<"angular_y">>, <<"angular_z">>]) of
                    true ->
                        DataSource = maps:get(<<"dataSource">>, Prop, #{}),
                        Key = maps:get(<<"key">>, DataSource, <<>>),
                        SlaveId = maps:get(<<"slaveid">>, DataSource, <<>>),
                        ?LOG(info, "~s: key=~s, slaveid=~s", [Identifier, Key, SlaveId]);
                    false ->
                        ok
                end
            end, Props);
        Error ->
            ?LOG(error, "验证失败: ~p", [Error])
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
%% @doc 修复属性配置
fix_properties(Props) ->
    lists:map(fun(Prop) ->
        case maps:get(<<"identifier">>, Prop) of
            <<"angular_x">> ->
                fix_angular_property(Prop, <<"angular_x">>, <<"0X07">>);
            <<"angular_y">> ->
                fix_angular_property(Prop, <<"angular_y">>, <<"0X08">>);
            <<"angular_z">> ->
                fix_angular_property(Prop, <<"angular_z">>, <<"0X09">>);
            _ ->
                Prop
        end
    end, Props).

%% @private
%% @doc 修复单个角度属性
fix_angular_property(Prop, Identifier, Address) ->
    ?LOG(info, "修复属性: ~s", [Identifier]),
    
    % 获取当前dataSource
    DataSource = maps:get(<<"dataSource">>, Prop, #{}),
    
    % 修正dataSource
    FixedDataSource = DataSource#{
        <<"key">> => <<"block_data">>,
        <<"slaveid">> => <<"0001">>,
        <<"address">> => Address,
        <<"operatetype">> => <<"readHregs">>,
        <<"originaltype">> => <<"short16_AB">>,
        <<"registersnumber">> => <<"1">>
    },
    
    % 获取当前dataForm
    DataForm = maps:get(<<"dataForm">>, Prop, #{}),
    
    % 修正dataForm
    FixedDataForm = DataForm#{
        <<"strategy">> => <<"计算值">>,
        <<"collection">> => <<"%{s}/32768*2000">>,
        <<"protocol">> => <<"MODBUSRTU">>
    },
    
    Prop#{
        <<"dataSource">> => FixedDataSource,
        <<"dataForm">> => FixedDataForm
    }.
