%%%-------------------------------------------------------------------
%%% @author kenneth
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 四月 2020 16:35
%%%-------------------------------------------------------------------
-author("kenneth").
-define(TYPE, <<"MYSQL">>).
-define(DEFAULT, <<"default">>).
-define(PRE, <<"sw_">>).
-define(Table(Name), <<?PRE/binary, Name/binary>>).

-define(TYPES, [
    <<"TINYINT">>,
    <<"SMALLINT">>,
    <<"MEDIUMINT">>,
    <<"INT">>,
    <<"INTEGER">>,
    <<"BIGINT">>,
    <<"FLOAT">>,
    <<"DOUBLE">>,
    <<"DECIMAL">>,
    <<"DATE">>,
    <<"TIME">>,
    <<"YEAR">>,
    <<"DATETIME">>,
    <<"TIMESTAMP">>,
    <<"CHAR">>,
    <<"VARCHAR">>,
    <<"TINYTEXT">>,
    <<"BLOB">>,
    <<"TEXT">>,
    <<"MEDIUMBLOB">>,
    <<"MEDIUMTEXT">>,
    <<"LONGBLOB">>,
    <<"LONGTEXT">>
]).
