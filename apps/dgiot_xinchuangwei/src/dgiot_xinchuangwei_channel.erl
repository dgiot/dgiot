%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%  前置机客户端
%%% @end
%%% Created : 07. 五月 2021 12:00
%%%-------------------------------------------------------------------
-module(dgiot_xinchuangwei_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-define(TYPE, <<"XINCHUANGWEI">>).
-define(MAX_BUFF_SIZE, 1024).
-include("dgiot_xinchuangwei.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).


%% 注册通道类型
-channel(?TYPE).
-channel_type(#{
    type => 1,
    title => #{
        zh => <<"新创微控制器采集通道"/utf8>>
    },
    description => #{
        zh => <<"新创微控制器采集通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"port">> => #{
        order => 1,
        type => integer,
        required => true,
        default => 20110,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"侦听端口"/utf8>>
        }
    },
    <<"regtype">> => #{
        order => 2,
        type => string,
        required => true,
        default => <<"上传IMEI"/utf8>>,
        title => #{
            zh => <<"注册类型"/utf8>>
        },
        description => #{
            zh => <<"上传IMEI"/utf8>>
        }
    },
    <<"dtutype">> => #{
        order => 3,
        type => string,
        required => true,
        default => <<"TAS">>,
        title => #{
            zh => <<"控制器厂商"/utf8>>
        },
        description => #{
            zh => <<"控制器厂商"/utf8>>
        }
    },
    <<"heartbeat">> => #{
        order => 4,
        type => integer,
        required => true,
        default => 10,
        title => #{
            zh => <<"心跳周期"/utf8>>
        },
        description => #{
            zh => <<"心跳周期"/utf8>>
        }
    },
    <<"productname">> => #{
        order => 5,
        type => string,
        required => true,
        default => <<"新创微控制器"/utf8>>,
        title => #{
            zh => <<"产品名称"/utf8>>
        },
        description => #{
            zh => <<"产品名称，用于创建产品"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/xinchuangweilog.png">>,
        title => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        },
        description => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        }
    }
}).


start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

%% 通道初始化
init(?TYPE, ChannelId, #{
    <<"port">> := Port,
    <<"heartbeat">> := Heartbeat,
    <<"regtype">> := Type,
    <<"product">> := Products,
    <<"dtutype">> := DTUTYPE,
    <<"productname">> := Productname
} = Args) ->
    _Env = get_newenv(Args),
    [{App, DtuProductId} | _] = get_app(Products, DTUTYPE, Productname),
    State = #state{
        id = ChannelId,
        regtype = Type,
        app = App,
        dtuproduct = DtuProductId
    },
    dgiot_data:insert({ChannelId, heartbeat}, Heartbeat),
    {ok, State, dgiot_xinchuangwei_tcp:start(Port, State)};

init(?TYPE, _ChannelId, _Args) ->
    {ok, #{}, #{}}.

%% 当前通道
handle_init(State) ->
    dgiot_parse:subscribe(<<"Device/*">>, put),
    dgiot_parse:subscribe(<<"Product">>, post),
    {ok, State}.
%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "Channel ~p", [Event]),
    {ok, State}.

handle_message({sync_parse, Args}, State) ->
    case jsx:decode(Args, [return_maps]) of
        #{<<"basedata">> := Basedata, <<"devaddr">> := Devaddr, <<"product">> := #{<<"objectId">> := ProductId}} ->
%%            设置参数
            Payloads = dgiot_xinchuangwei_decoder:set_params(Basedata, ProductId),
            Modbuspayloads = modbus_rtu:set_params(Basedata, ProductId),
            Topic = <<"setXinchuangwei/", ProductId/binary, "/", Devaddr/binary>>,
            dgiot_mqtt:publish(self(), Topic, lists:append(Payloads, Modbuspayloads));
        #{<<"name">> := _Name, <<"productSecret">> := _ProductSecret, <<"objectId">> := _ObjectId, <<"ACL">> := _ACL} ->
            pass;
        _ ->
            pass
    end,
    {ok, State};

handle_message(Message, State) ->
    ?LOG(info, "Channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(warning, "Channel[~p,~p] stop", [ChannelType, ChannelId]),
    ok.

get_app(Products, DTUTYPE, Productname) ->
    lists:map(fun({_ProductId, #{<<"ACL">> := Acl}}) ->
        Predicate = fun(E) ->
            case E of
                <<"role:", _/binary>> -> true;
                _ -> false
            end
                    end,
        DtuProductId =
            case dgiot_xinchuangwei:create_product(#{<<"DTUTYPE">> => dgiot_utils:to_binary(DTUTYPE), <<"ACL">> => Acl, <<"productname">> => Productname}) of
                {_, #{<<"objectId">> := DtuProductId1}} ->
%%                    dgiot_product:load(DtuProductId1),
                    DtuProductId1;
                _ -> <<"">>
            end,
        dgiot_product:load(DtuProductId),
        [<<"role:", App/binary>> | _] = lists:filter(Predicate, maps:keys(Acl)),
        {App, DtuProductId}
              end, Products).

get_newenv(Args) ->
    maps:without([
        <<"behaviour">>,
        <<"MaxOverFlow">>,
        <<"Size">>,
        <<"applicationtText">>,
        <<"product">>], Args).
