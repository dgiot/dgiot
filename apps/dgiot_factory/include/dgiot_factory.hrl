-define(DEFAULT, #{<<"default">> => #{<<"type">> => <<"day">>,
    <<"work_shift">> => #{<<"morning_shift">> => #{<<"begin">> => <<"8:00">>, <<"end">> => <<"12:00">>},
        <<"afternonn_shift">> => #{<<"begin">> => <<"13:00">>, <<"end">> => <<"19:00">>},
        <<"evening_shift">> => #{<<"begin">> => <<"19:00">>, <<"end">> => <<"6:30">>
        }}}, <<"other">> => #{}}).


-define(PERSON ,<<"person">>).
-define(MATERIALTABLE,<<"material">>).
-define(MATERIALETS,material).
-define(WORKERTREE, workertree).
-define(FACTORY_ORDER,factory_order).
-define(FACTORY_QUALITY,factory_quality).
-define(FACTORY_NOTIFICATION,factory_notification).
-define(WORKERSHIFT,workershift).
-define(MAXWORKERNUM,1024).
-define(MAXUNIT,4).

