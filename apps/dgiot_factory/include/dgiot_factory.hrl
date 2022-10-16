-define(DEFAULT, #{<<"default">> => #{<<"type">> => <<"day">>,
    <<"work_shift">> => #{<<"morning_shift">> => #{<<"begin">> => <<"8:00">>, <<"end">> => <<"12:00">>},
        <<"afternonn_shift">> => #{<<"begin">> => <<"13:00">>, <<"end">> => <<"19:00">>},
        <<"evening_shift">> => #{<<"begin">> => <<"19:00">>, <<"end">> => <<"6:30">>
        }}}, <<"other">> => #{}}).




-define(MATERIALETS,material).
-define(WORKER, worker).
-define(FACTORY_ORDER,factory_order).
-define(FACTORY_QUALITY,factory_quality).
-define(FACTORY,factory).
-define(WORKERSHIFT,workershift).
-define(MAXWORKERNUM,1024).
-define(MAXUNIT,4).

-define(ONEDAY, 86400).

