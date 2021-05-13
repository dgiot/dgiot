%% -----------------------------------------------------------------------------
%%
%% sqlparse.yrl: SQL - parser definition.
%%
%% Copyright (c) 2012-18 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

%% -*- erlang -*-
Header "%% Copyright (C) K2 Informatics GmbH"
"%% @private"
"%% @Author Bikram Chatterjee"
"%% @Email bikram.chatterjee@k2informatics.ch".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Nonterminals
 all_distinct
 all_or_any_predicate
 any_all_some
 asc_desc
 assignment
 assignment_commalist
 assignment_statement
 atom
 base_table_element
 base_table_element_commalist
 between_predicate
 case_when_exp
 case_when_opt_as_exp
 case_when_then
 case_when_then_list
 close_statement
 cluster_name
 column
 column_commalist
 column_def
 column_def_list
 column_def_opt
 column_ref
 column_ref_commalist
 commit_statement
 comparison_predicate
 create_index_def
 create_index_filter
 create_index_norm
 create_index_opts
 create_index_spec
 create_index_spec_column
 create_index_spec_items
 create_opts
 create_role_def
 create_table_def
 cursor
 cursor_def
 data_type
 db_user_proxy
 delete_statement_positioned
 delete_statement_searched
 do_clause
 incase_clause
 drop_cluster_def
 drop_cluster_extensions
 drop_context_def
 drop_database_def
 drop_database_link_def
 drop_directory_def
 drop_function_def
 drop_index_def
 drop_index_extensions
 drop_materialized_view_def
 drop_package_def
 drop_procedure_def
 drop_profile_def
 drop_role_def
 drop_sequence_def
 drop_synonym_def
 drop_table_def
 drop_table_extensions
 drop_tablespace_def
 drop_tablespace_extensions
 drop_trigger_def
 drop_type_def
 drop_type_body_def
 drop_view_def
 else
 escape
 existence_test
 exists
 extra
 fetch_statement
 from_clause
 from_column
 from_column_commalist
 fun_arg
 fun_arg_named
 fun_args
 fun_args_named
 function_name
 function_ref
 grant_def
 grantee_identified_by
 grantee_revokee
 grantee_revokee_commalist
 group_by_clause
 having_clause
 hierarchical_query_clause
 hint
 identified
 identifier
 in_predicate
 index_name
 inner_cross_join
 insert_atom
 insert_atom_commalist
 insert_statement
 into
 is_not_null
 is_null
 join
 join_clause
 join_list
 join_on_or_using_clause
 join_ref
 like_predicate
 literal
 manipulative_statement
 materialized
 materialized_view_name
 nocycle
 not_between
 not_in
 not_like
 object_privilege
 object_privilege_list
 object_with_grant_option
 object_with_revoke_option
 on_obj_clause
 open_statement
 order_by_clause
 ordering_spec
 ordering_spec_commalist
 outer_join
 outer_join_type
 package_name
 parameter
 parameter_ref
 plsql_block
 plsql_block_sql_list
 plsql_body
 predicate
 procedure_call
 procedure_name
 proxy_auth_req
 proxy_clause
 proxy_with
 query_exp
 query_partition_clause
 query_spec
 query_term
 quota
 quota_list
 returning
 revoke_def
 role_list
 rollback_statement
 scalar_exp
 scalar_exp_commalist
 scalar_opt_as_exp
 scalar_opt_as_exp_1
 scalar_opt_as_exp_2
 scalar_sub_exp
 schema
 schema_element
 schema_element_list
 search_condition
 select_field
 select_field_commalist
 select_statement
 selection
 sequence_name
 sgn_num
 spec_item
 spec_list
 sql
 sql_list
 statement_pragma
 statement_pragma_list
 storage
 subquery
 synonym_name
 system_privilege
 system_privilege_list
 system_with_grant_option
 table
 table_alias
 table_coll_expr
 table_constraint_def
 table_dblink
 table_exp
 table_list
 table_ref
 target
 target_commalist
 tbl_scope
 tbl_type
 test_for_null
 trigger_name
 truncate_cluster
 truncate_table
 type_name
 unary_add_or_subtract
 update_statement_positioned
 update_statement_searched
 user_opt
 user_opts_list
 user_role
 values_or_query_spec
 view_def
 when_action
 where_clause
.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% symbolic tokens
%% literal keyword tokens
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Terminals
 ADMIN
 ALL
 ALTER
 AND
 ANY
 APPROXNUM
 AS
 ASC
 AUTHENTICATION
 AUTHORIZATION
 BAG
 BEGIN
 BETWEEN
 BFILE
 BINARY_DOUBLE
 BINARY_FLOAT
 BITMAP
 BLOB
 BODY
 BY
 CALL
 CASCADE
 CASE
 CHAR
 CHECK
 CLOB
 CLOSE
 CLUSTER
 COMMIT
 COMPARISON
 CONNECT
 CONSTRAINT
 CONSTRAINTS
 CONTENTS
 CONTEXT
 CONTINUE
 CREATE
 CROSS
 CURRENT
 CURSOR
 DATABASE
 DATAFILES
 DATE
 DBLINK
 DEFAULT
 DEFERRED
 DELEGATE
 DELETE
 DESC
 DIRECTORY
 DISTINCT
 DROP
 ELSE
 END
 ENTERPRISE
 ESCAPE
 EXCEPT
 EXECUTE
 EXISTS
 EXTERNALLY
 FETCH
 FILTER_WITH
 FORCE
 FOREIGN
 FOUND
 FROM
 FULL
 FUNCTION
 FUNS
 GLOBALLY
 GOTO
 GRANT
 GROUP
 HASHMAP
 HAVING
 HIERARCHY
 HINT
 IDENTIFIED
 IF
 IMMEDIATE
 IN
 INCASE
 INCLUDING
 INDEX
 INDICATOR
 INNER
 INSERT
 INTERSECT
 INTNUM
 INTO
 INVALIDATION
 IS
 JOIN
 JSON
 KEEP
 KEY
 KEYLIST
 LEFT
 LIKE
 LINK
 LOCAL
 LOG
 LONG
 MATERIALIZED
 MINUS
 NAME
 NATURAL
 NCHAR
 NCLOB
 NO
 NOCYCLE
 NONE
 NORM_WITH
 NOT
 NULLX
 NUMBER
 NVARCHAR2
 OF
 ON
 ONLINE
 OPEN
 OPTION
 OR
 ORDER
 ORDERED_SET
 OUTER
 PACKAGE
 PARAMETER
 PARTITION
 PRESERVE
 PRIMARY
 PRIOR
 PRIVILEGES
 PROCEDURE
 PROFILE
 PUBLIC
 PURGE
 QUOTA
 RAW
 REFERENCES
 REQUIRED
 RETURN
 RETURNING
 REUSE
 REVOKE
 RIGHT
 ROLE
 ROLES
 ROLLBACK
 ROWID
 SCHEMA
 SELECT
 FOREACH
 DO
 SEQUENCE
 SET
 SOME
 SQLERROR
 START
 STORAGE
 STRING
 SYNONYM
 TABLE
 TABLES
 TABLESPACE
 TEMPORARY
 THEN
 THROUGH
 TIMESTAMP
 TO
 TRIGGER
 TRUNCATE
 TYPE
 UNION
 UNIQUE
 UNLIMITED
 UPDATE
 UROWID
 USING
 VALIDATE
 VALUES
 VARCHAR2
 VIEW
 WHEN
 WHENEVER
 WHERE
 WITH
 WORK
 XMLTYPE
 '('
 ')'
 '*'
 '+'
 ','
 '-'
 '.'
 '/'
 ':='
 ';'
 '='
 '=>'
 'div'
 '||'
.

Rootsymbol plsql_block_sql_list.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% precedence
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Left        100 OR.
Left        200 AND.
Left        300 NOT.
Nonassoc    400 '=' COMPARISON.
Left        500 '+' '-' '||'.
Left        600 '*' '/' 'div'.
Left        700 unary_add_or_subtract.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plsql_block_sql_list -> plsql_block : '$1'.
plsql_block_sql_list -> sql_list    : '$1'.

plsql_block -> plsql_body : '$1'.

sql_list ->          sql ';'       :         [{'$1',{extra, <<>>}}].
sql_list ->          sql ';' extra :         [{'$1','$3'}].
sql_list -> sql_list sql ';'       : '$1' ++ [{'$2',{extra, <<>>}}].
sql_list -> sql_list sql ';' extra : '$1' ++ [{'$2','$4'}].

extra -> NAME  ';' : {extra, unwrap_bin('$1')}.

%% =============================================================================
%% Helper definitions - test purposes.
%% -----------------------------------------------------------------------------
% sql -> table_ref : '$1'.
%% =============================================================================

sql -> manipulative_statement : '$1'.
sql -> schema                 : '$1'.

plsql_body -> BEGIN statement_pragma_list END ';' : {'plsql_body', '$2'}.

statement_pragma_list ->                       statement_pragma ';' :         [{'$1', ';'}].
statement_pragma_list -> statement_pragma_list statement_pragma ';' : '$1' ++ [{'$2', ';'}].

statement_pragma -> assignment_statement           : '$1'.
statement_pragma -> close_statement                : '$1'.
statement_pragma -> commit_statement               : '$1'.
statement_pragma -> cursor_def                     : '$1'.
statement_pragma -> delete_statement_positioned    : '$1'.
statement_pragma -> delete_statement_searched      : '$1'.
statement_pragma -> fetch_statement                : '$1'.
statement_pragma -> function_ref                   : '$1'.
statement_pragma -> insert_statement               : '$1'.
statement_pragma -> open_statement                 : '$1'.
statement_pragma -> procedure_call                 : '$1'.
statement_pragma -> rollback_statement             : '$1'.
statement_pragma -> select_statement               : '$1'.
statement_pragma -> update_statement_positioned    : '$1'.
statement_pragma -> update_statement_searched      : '$1'.
statement_pragma -> WHENEVER NOT FOUND when_action : {when_not_found, '$4'}.
statement_pragma -> WHENEVER SQLERROR  when_action : {when_sql_err, '$3'}.

assignment_statement -> parameter ':=' scalar_opt_as_exp_1 : {':=', '$1', '$3'}.

procedure_call -> CALL function_ref : {'call procedure',  '$2'}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% schema definition language
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

schema -> CREATE SCHEMA AUTHORIZATION identifier                     : {'create schema authorization', binary_to_list('$4'), []}.
schema -> CREATE SCHEMA AUTHORIZATION identifier schema_element_list : {'create schema authorization', binary_to_list('$4'), '$5'}.

schema_element_list ->                     schema_element :         ['$1'].
schema_element_list -> schema_element_list schema_element : '$1' ++ ['$2'].

schema_element -> create_table_def : '$1'.
schema_element -> view_def         : '$1'.
schema_element -> grant_def        : '$1'.

create_role_def -> CREATE ROLE identifier : {'create role', '$3'}.

create_table_def -> CREATE             TABLE table '('                              ')' : {'create table', '$3', [],   []}.
create_table_def -> CREATE             TABLE table '(' base_table_element_commalist ')' : {'create table', '$3', '$5', []}.
create_table_def -> CREATE create_opts TABLE table '('                              ')' : {'create table', '$4', [],   '$2'}.
create_table_def -> CREATE create_opts TABLE table '(' base_table_element_commalist ')' : {'create table', '$4', '$6', '$2'}.

create_index_def -> CREATE                   INDEX            ON table_alias                                                         : {'create index', {},   {},   '$4', [],   {},   {}}.
create_index_def -> CREATE                   INDEX            ON table_alias                                     create_index_filter : {'create index', {},   {},   '$4', [],   {},   '$5'}.
create_index_def -> CREATE                   INDEX            ON table_alias                   create_index_norm                     : {'create index', {},   {},   '$4', [],   '$5', {}}.
create_index_def -> CREATE                   INDEX            ON table_alias                   create_index_norm create_index_filter : {'create index', {},   {},   '$4', [],   '$5', '$6'}.
create_index_def -> CREATE                   INDEX            ON table_alias create_index_spec                                       : {'create index', {},   {},   '$4', '$5', {},   {}}.
create_index_def -> CREATE                   INDEX            ON table_alias create_index_spec                   create_index_filter : {'create index', {},   {},   '$4', '$5', {},   '$6'}.
create_index_def -> CREATE                   INDEX            ON table_alias create_index_spec create_index_norm                     : {'create index', {},   {},   '$4', '$5', '$6', {}}.
create_index_def -> CREATE                   INDEX            ON table_alias create_index_spec create_index_norm create_index_filter : {'create index', {},   {},   '$4', '$5', '$6', '$7'}.
create_index_def -> CREATE                   INDEX index_name ON table_alias                                                         : {'create index', {},   '$3', '$5', [],   {},   {}}.
create_index_def -> CREATE                   INDEX index_name ON table_alias                                     create_index_filter : {'create index', {},   '$3', '$5', [],   {},   '$6'}.
create_index_def -> CREATE                   INDEX index_name ON table_alias                   create_index_norm                     : {'create index', {},   '$3', '$5', [],   '$6', {}}.
create_index_def -> CREATE                   INDEX index_name ON table_alias                   create_index_norm create_index_filter : {'create index', {},   '$3', '$5', [],   '$6', '$7'}.
create_index_def -> CREATE                   INDEX index_name ON table_alias create_index_spec                                       : {'create index', {},   '$3', '$5', '$6', {},   {}}.
create_index_def -> CREATE                   INDEX index_name ON table_alias create_index_spec                   create_index_filter : {'create index', {},   '$3', '$5', '$6', {},   '$7'}.
create_index_def -> CREATE                   INDEX index_name ON table_alias create_index_spec create_index_norm create_index_filter : {'create index', {},   '$3', '$5', '$6', '$7', '$8'}.
create_index_def -> CREATE                   INDEX index_name ON table_alias create_index_spec create_index_norm                     : {'create index', {},   '$3', '$5', '$6', '$7', {}}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias                                                         : {'create index', '$2', {},   '$5', [],   {},   {}}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias                                     create_index_filter : {'create index', '$2', {},   '$5', [],   {},   '$6'}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias                   create_index_norm                     : {'create index', '$2', {},   '$5', [],   '$6', {}}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias                   create_index_norm create_index_filter : {'create index', '$2', {},   '$5', [],   '$6', '$7'}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias create_index_spec                                       : {'create index', '$2', {},   '$5', '$6', {},   {}}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias create_index_spec                   create_index_filter : {'create index', '$2', {},   '$5', '$6', {},   '$7'}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias create_index_spec create_index_norm                     : {'create index', '$2', {},   '$5', '$6', '$7', {}}.
create_index_def -> CREATE create_index_opts INDEX            ON table_alias create_index_spec create_index_norm create_index_filter : {'create index', '$2', {},   '$5', '$6', '$7', '$8'}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias                                                         : {'create index', '$2', '$4', '$6', [],   {},   {}}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias                                     create_index_filter : {'create index', '$2', '$4', '$6', [],   {},   '$7'}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias                   create_index_norm                     : {'create index', '$2', '$4', '$6', [],   '$7', {}}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias                   create_index_norm create_index_filter : {'create index', '$2', '$4', '$6', [],   '$7', '$8'}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias create_index_spec                                       : {'create index', '$2', '$4', '$6', '$7', {},   {}}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias create_index_spec                   create_index_filter : {'create index', '$2', '$4', '$6', '$7', {},   '$8'}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias create_index_spec create_index_norm                     : {'create index', '$2', '$4', '$6', '$7', '$8', {}}.
create_index_def -> CREATE create_index_opts INDEX index_name ON table_alias create_index_spec create_index_norm create_index_filter : {'create index', '$2', '$4', '$6', '$7', '$8', '$9'}.

create_index_opts -> BITMAP  : bitmap.
create_index_opts -> KEYLIST : keylist.
create_index_opts -> HASHMAP : hashmap.
create_index_opts -> UNIQUE  : unique.

index_name ->                identifier : '$1'.
index_name -> identifier '.' identifier : list_to_binary(['$1', ".", '$3']).

create_index_spec -> '(' create_index_spec_items ')' : '$2'.

create_index_spec_items -> create_index_spec_column                             : ['$1'].
create_index_spec_items -> create_index_spec_column ',' create_index_spec_items : ['$1' | '$3'].

create_index_spec_column -> identifier      : '$1'.
create_index_spec_column -> identifier JSON : jpparse(list_to_binary(['$1',unwrap('$2')])).

create_index_norm -> NORM_WITH STRING : {norm, unwrap_bin('$2')}.

create_index_filter -> FILTER_WITH STRING : {filter, unwrap_bin('$2')}.

create_opts ->           tbl_type : '$1'.
create_opts -> tbl_scope          : '$1'.
create_opts -> tbl_scope tbl_type : '$1' ++ '$2'.

tbl_scope -> LOCAL   : [{scope, <<"local">>}].
tbl_scope -> CLUSTER : [{scope, <<"cluster">>}].
tbl_scope -> SCHEMA  : [{scope, <<"schema">>}].

tbl_type -> SET         : [{type, <<"set">>}].
tbl_type -> ORDERED_SET : [{type, <<"ordered_set">>}].
tbl_type -> BAG         : [{type, <<"bag">>}].
tbl_type -> NAME        : [{type, unwrap_bin('$1')}].

proxy_clause -> GRANT  CONNECT THROUGH db_user_proxy    : {'grant connect', '$4'}.
proxy_clause -> REVOKE CONNECT THROUGH db_user_proxy    : {'revoke connect', '$4'}.

db_user_proxy -> proxy_with                : '$1'.
db_user_proxy ->            proxy_auth_req : '$1'.
db_user_proxy -> proxy_with proxy_auth_req : {'$1', '$2'}.

proxy_with -> WITH NO ROLES                  : 'with no roles'.
proxy_with -> WITH ROLE            role_list : {'with role', '$3'}.
proxy_with -> WITH ROLE ALL EXCEPT role_list : {'with role all except', '$5'}.

proxy_auth_req -> AUTHENTICATION REQUIRED : 'authentication required'.

spec_list -> spec_item           : ['$1'].
spec_list -> spec_item spec_list : ['$1'|'$2'].

spec_item -> identified : '$1'.
spec_item -> user_opt   : '$1'.
spec_item -> user_role  : '$1'.

user_role -> DEFAULT ROLE ALL                  : 'default role all'.
user_role -> DEFAULT ROLE ALL EXCEPT role_list : {'default role all except', '$5'}.
user_role -> DEFAULT ROLE NONE                 : 'default role none'.
user_role -> DEFAULT ROLE            role_list : {'default role', '$3'}.

role_list -> NAME               : [unwrap_bin('$1')].
role_list -> NAME ',' role_list : [unwrap_bin('$1') | '$3'].

identified -> IDENTIFIED            BY identifier : {'identified by',       '$3'}.
identified -> IDENTIFIED EXTERNALLY               : {'identified extern',   {}}.
identified -> IDENTIFIED EXTERNALLY AS identifier : {'identified extern',   '$4'}.
identified -> IDENTIFIED GLOBALLY                 : {'identified globally', {}}.
identified -> IDENTIFIED GLOBALLY   AS identifier : {'identified globally', '$4'}.

user_opts_list -> user_opt                : ['$1'].
user_opts_list -> user_opt user_opts_list : ['$1'] ++ '$2'.

user_opt -> DEFAULT   TABLESPACE identifier : [{'default tablespace',   '$3'}].
user_opt -> TEMPORARY TABLESPACE identifier : [{'temporary tablespace', '$3'}].
user_opt -> quota_list                      : [{quotas,  '$1'}].
user_opt -> PROFILE              identifier : [{profile, '$2'}].

quota_list -> quota            : ['$1'].
quota_list -> quota quota_list : ['$1'] ++ '$2'.

quota -> QUOTA UNLIMITED         ON identifier : {'unlimited on', '$4'}.
quota -> QUOTA INTNUM            ON identifier : {limited, unwrap_bin('$2'), <<"">>, '$4'}.
quota -> QUOTA INTNUM identifier ON identifier : {limited, unwrap_bin('$2'), '$3',   '$5'}.

table_list ->                table :         ['$1'].
table_list -> table_list ',' table : '$1' ++ ['$3'].

exists -> IF EXISTS : 'exists'.

base_table_element_commalist ->                                  base_table_element :         ['$1'].
base_table_element_commalist -> base_table_element_commalist ',' base_table_element : '$1' ++ ['$3'].

base_table_element -> column_def           : '$1'.
base_table_element -> table_constraint_def : '$1'.

column_def -> column data_type                 : {'$1', '$2', []}.
column_def -> column data_type column_def_list : {'$1', '$2', '$3'}.

column_def_list ->                 column_def_opt : ['$1'].
column_def_list -> column_def_list column_def_opt : '$1' ++ ['$2'].

column_def_opt -> NOT NULLX                                 : 'not null'.
column_def_opt -> NOT NULLX UNIQUE                          : 'not null unique'.
column_def_opt -> NOT NULLX PRIMARY KEY                     : 'not null primary key'.
column_def_opt -> DEFAULT function_ref                      : {default, '$2'}.
column_def_opt -> DEFAULT identifier                        : {default, '$2'}.
column_def_opt -> DEFAULT literal                           : {default, '$2'}.
column_def_opt -> DEFAULT NULLX                             : {default, 'null'}.
column_def_opt -> CHECK '(' search_condition ')'            : {check, '$3'}.
column_def_opt -> REFERENCES table                          : {ref, '$2'}.
column_def_opt -> REFERENCES table '(' column_commalist ')' : {ref, {'$2', '$4'}}.

table_constraint_def ->                       UNIQUE      '(' column_commalist ')'                                           : {unique,        [],   '$3'}.
table_constraint_def ->                       PRIMARY KEY '(' column_commalist ')'                                           : {'primary key', [],   '$4'}.
table_constraint_def ->                       FOREIGN KEY '(' column_commalist ')' REFERENCES table                          : {'foreign key', [],   '$4', {'ref', '$7'}}.
table_constraint_def ->                       FOREIGN KEY '(' column_commalist ')' REFERENCES table '(' column_commalist ')' : {'foreign key', [],   '$4', {'ref', {'$7', '$9'}}}.
table_constraint_def ->                       CHECK '(' search_condition ')'                                                 : {check,         [],   '$3'}.
table_constraint_def -> CONSTRAINT identifier UNIQUE      '(' column_commalist ')'                                           : {unique,        '$2', '$5'}.
table_constraint_def -> CONSTRAINT identifier PRIMARY KEY '(' column_commalist ')'                                           : {'primary key', '$2', '$6'}.
table_constraint_def -> CONSTRAINT identifier FOREIGN KEY '(' column_commalist ')' REFERENCES table                          : {'foreign key', '$2', '$6', {'ref', '$9'}}.
table_constraint_def -> CONSTRAINT identifier FOREIGN KEY '(' column_commalist ')' REFERENCES table '(' column_commalist ')' : {'foreign key', '$2', '$6', {'ref', {'$9', '$11'}}}.
table_constraint_def -> CONSTRAINT identifier CHECK '(' search_condition ')'                                                 : {check,         '$2', '$5'}.

column_commalist -> column                      : ['$1'].
column_commalist -> column ',' column_commalist : ['$1' | '$3'].

view_def -> CREATE VIEW table                          AS query_spec                   : {'create view', '$3', [],   {as, '$5', []}}.
view_def -> CREATE VIEW table                          AS query_spec WITH CHECK OPTION : {'create view', '$3', [],   {as, '$5', "with check option"}}.
view_def -> CREATE VIEW table '(' column_commalist ')' AS query_spec                   : {'create view', '$3', '$5', {as, '$8', []}}.
view_def -> CREATE VIEW table '(' column_commalist ')' AS query_spec WITH CHECK OPTION : {'create view', '$3', '$5', {as, '$8', "with check option"}}.

grant_def -> GRANT ALL PRIVILEGES        on_obj_clause TO grantee_revokee_commalist                          : {grant, ['all privileges'], '$4',         {to, '$6'},   ''}.
grant_def -> GRANT object_privilege_list on_obj_clause TO grantee_revokee_commalist                          : {grant, '$2',               '$3',         {to, '$5'},   ''}.
grant_def -> GRANT ALL PRIVILEGES        on_obj_clause TO grantee_identified_by                              : {grant, ['all privileges'], '$4',         {to, ['$6']}, ''}.
grant_def -> GRANT object_privilege_list on_obj_clause TO grantee_identified_by                              : {grant, '$2',               '$3',         {to, ['$5']}, ''}.
grant_def -> GRANT ALL PRIVILEGES        on_obj_clause TO grantee_revokee_commalist object_with_grant_option : {grant, ['all privileges'], '$4',         {to, '$6'},   '$7'}.
grant_def -> GRANT object_privilege_list on_obj_clause TO grantee_revokee_commalist object_with_grant_option : {grant, '$2',               '$3',         {to, '$5'},   '$6'}.
grant_def -> GRANT ALL PRIVILEGES        on_obj_clause TO grantee_identified_by     object_with_grant_option : {grant, ['all privileges'], '$4',         {to, ['$6']}, '$7'}.
grant_def -> GRANT object_privilege_list on_obj_clause TO grantee_identified_by     object_with_grant_option : {grant, '$2',               '$3',         {to, ['$5']}, '$6'}.
grant_def -> GRANT ALL PRIVILEGES                      TO grantee_revokee_commalist                          : {grant, ['all privileges'], {on, <<"">>}, {to, '$5'},   ''}.
grant_def -> GRANT system_privilege_list               TO grantee_revokee_commalist                          : {grant, '$2',               {on, <<"">>}, {to, '$4'},   ''}.
grant_def -> GRANT ALL PRIVILEGES                      TO grantee_identified_by                              : {grant, ['all privileges'], {on, <<"">>}, {to, ['$5']}, ''}.
grant_def -> GRANT system_privilege_list               TO grantee_identified_by                              : {grant, '$2',               {on, <<"">>}, {to, ['$4']}, ''}.
grant_def -> GRANT ALL PRIVILEGES                      TO grantee_revokee_commalist system_with_grant_option : {grant, ['all privileges'], {on, <<"">>}, {to, '$5'},   '$6'}.
grant_def -> GRANT system_privilege_list               TO grantee_revokee_commalist system_with_grant_option : {grant, '$2',               {on, <<"">>}, {to, '$4'},   '$5'}.
grant_def -> GRANT ALL PRIVILEGES                      TO grantee_identified_by     system_with_grant_option : {grant, ['all privileges'], {on, <<"">>}, {to, ['$5']}, '$6'}.
grant_def -> GRANT system_privilege_list               TO grantee_identified_by     system_with_grant_option : {grant, '$2',               {on, <<"">>}, {to, ['$4']}, '$5'}.

revoke_def -> REVOKE ALL PRIVILEGES        on_obj_clause FROM grantee_revokee_commalist                           : {revoke, ['all privileges'], '$4',         {from, '$6'}, ''}.
revoke_def -> REVOKE object_privilege_list on_obj_clause FROM grantee_revokee_commalist                           : {revoke, '$2',               '$3',         {from, '$5'}, ''}.
revoke_def -> REVOKE ALL PRIVILEGES        on_obj_clause FROM grantee_revokee_commalist object_with_revoke_option : {revoke, ['all privileges'], '$4',         {from, '$6'}, '$7'}.
revoke_def -> REVOKE object_privilege_list on_obj_clause FROM grantee_revokee_commalist object_with_revoke_option : {revoke, '$2',               '$3',         {from, '$5'}, '$6'}.
revoke_def -> REVOKE ALL PRIVILEGES                      FROM grantee_revokee_commalist                           : {revoke, ['all privileges'], {on, <<"">>}, {from, '$5'}, ''}.
revoke_def -> REVOKE system_privilege_list               FROM grantee_revokee_commalist                           : {revoke, '$2',               {on, <<"">>}, {from, '$4'}, ''}.

grantee_identified_by -> identifier IDENTIFIED BY STRING : {'identified by', '$1', unwrap_bin('$4')}.

grantee_revokee -> identifier : '$1'.
grantee_revokee -> PUBLIC     : 'public'.

grantee_revokee_commalist ->                               grantee_revokee :         ['$1'].
grantee_revokee_commalist -> grantee_revokee_commalist ',' grantee_revokee : '$1' ++ ['$3'].

object_privilege -> ALL        : 'all'.
object_privilege -> ALTER      : 'alter'.
object_privilege -> DELETE     : 'delete'.
object_privilege -> EXECUTE    : 'execute'.
object_privilege -> INDEX      : 'index'.
object_privilege -> INSERT     : 'insert'.
object_privilege -> REFERENCES : 'references'.
object_privilege -> SELECT     : 'select'.
object_privilege -> FOREACH    : 'foreach'.
object_privilege -> UPDATE     : 'update'.

object_privilege_list -> object_privilege                           : ['$1'].
object_privilege_list -> object_privilege ',' object_privilege_list : ['$1'|'$3'].

object_with_grant_option -> WITH GRANT     OPTION : 'with grant option'.
object_with_grant_option -> WITH HIERARCHY OPTION : 'with hierarchy option'.

object_with_revoke_option -> CASCADE CONSTRAINTS : 'cascade constraints'.
object_with_revoke_option -> FORCE               : 'force'.

on_obj_clause -> ON DIRECTORY identifier : {'on directory','$3'}.
on_obj_clause -> ON table                : {on,            '$2'}.

system_privilege -> ADMIN                        : 'admin'.
system_privilege -> ALTER ANY INDEX              : 'alter any index'.
system_privilege -> ALTER ANY MATERIALIZED VIEW  : 'alter any materialized view'.
system_privilege -> ALTER ANY TABLE              : 'alter any table'.
system_privilege -> ALTER ANY VIEW               : 'alter any view'.
system_privilege -> CREATE ANY INDEX             : 'create any index'.
system_privilege -> CREATE ANY MATERIALIZED VIEW : 'create any materialized view'.
system_privilege -> CREATE ANY TABLE             : 'create any table'.
system_privilege -> CREATE ANY VIEW              : 'create any view'.
system_privilege -> CREATE MATERIALIZED VIEW     : 'create materialized view'.
system_privilege -> CREATE TABLE                 : 'create table'.
system_privilege -> CREATE VIEW                  : 'create view'.
system_privilege -> DELETE ANY TABLE             : 'delete any table'.
system_privilege -> DROP ANY INDEX               : 'drop any index'.
system_privilege -> DROP ANY MATERIALIZED VIEW   : 'drop any materialized view'.
system_privilege -> DROP ANY TABLE               : 'drop any table'.
system_privilege -> DROP ANY VIEW                : 'drop any view'.
system_privilege -> INSERT ANY TABLE             : 'insert any table'.
system_privilege -> SELECT ANY TABLE             : 'select any table'.
system_privilege -> FOREACH ANY TABLE            : 'foreach any table'.
system_privilege -> UPDATE ANY TABLE             : 'update any table'.
system_privilege -> NAME                         : strl2atom(['$1']).

system_privilege_list -> system_privilege                           : ['$1'].
system_privilege_list -> system_privilege ',' system_privilege_list : ['$1'|'$3'].

system_with_grant_option -> WITH ADMIN    OPTION : 'with admin option'.
system_with_grant_option -> WITH DELEGATE OPTION : 'with delegate option'.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cursor definition
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cursor_def -> CURSOR cursor IS query_exp : {cursor_def, '$2', '$4'}.

order_by_clause -> ORDER BY ordering_spec_commalist : {'order by', '$3'}.

ordering_spec_commalist ->                             ordering_spec :         ['$1'].
ordering_spec_commalist -> ordering_spec_commalist ',' ordering_spec : '$1' ++ ['$3'].

ordering_spec -> scalar_exp          : {'$1', <<>>}.
ordering_spec -> scalar_exp asc_desc : {'$1', '$2'}.

asc_desc -> ASC  : <<"asc">>.
asc_desc -> DESC : <<"desc">>.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% manipulative statements
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

manipulative_statement -> create_index_def            : '$1'.
manipulative_statement -> create_role_def             : '$1'.
manipulative_statement -> create_table_def            : '$1'.
manipulative_statement -> delete_statement_positioned : '$1'.
manipulative_statement -> delete_statement_searched   : '$1'.
manipulative_statement -> drop_cluster_def            : '$1'.
manipulative_statement -> drop_context_def            : '$1'.
manipulative_statement -> drop_database_def           : '$1'.
manipulative_statement -> drop_database_link_def      : '$1'.
manipulative_statement -> drop_directory_def          : '$1'.
manipulative_statement -> drop_function_def           : '$1'.
manipulative_statement -> drop_index_def              : '$1'.
manipulative_statement -> drop_materialized_view_def  : '$1'.
manipulative_statement -> drop_package_def            : '$1'.
manipulative_statement -> drop_procedure_def          : '$1'.
manipulative_statement -> drop_profile_def            : '$1'.
manipulative_statement -> drop_role_def               : '$1'.
manipulative_statement -> drop_sequence_def           : '$1'.
manipulative_statement -> drop_synonym_def            : '$1'.
manipulative_statement -> drop_table_def              : '$1'.
manipulative_statement -> drop_tablespace_def         : '$1'.
manipulative_statement -> drop_trigger_def            : '$1'.
manipulative_statement -> drop_type_def               : '$1'.
manipulative_statement -> drop_type_body_def          : '$1'.
manipulative_statement -> drop_view_def               : '$1'.
manipulative_statement -> grant_def                   : '$1'.
manipulative_statement -> insert_statement            : '$1'.
manipulative_statement -> revoke_def                  : '$1'.
manipulative_statement -> select_statement            : '$1'.
manipulative_statement -> truncate_cluster            : '$1'.
manipulative_statement -> truncate_table              : '$1'.
manipulative_statement -> update_statement_positioned : '$1'.
manipulative_statement -> update_statement_searched   : '$1'.
manipulative_statement -> view_def                    : '$1'.

close_statement -> CLOSE cursor : {close, '$2'}.

commit_statement -> COMMIT      : 'commit'.
commit_statement -> COMMIT WORK : 'commit work'.

delete_statement_positioned -> DELETE FROM table_dblink WHERE CURRENT OF cursor           : {delete, '$3',{where_current_of, '$7'}, {returning, {}}}.
delete_statement_positioned -> DELETE FROM table_dblink WHERE CURRENT OF cursor returning : {delete, '$3',{where_current_of, '$7'}, '$8'}.

delete_statement_searched -> DELETE FROM table_dblink                        : {delete, '$3', [],   {returning, {}}}.
delete_statement_searched -> DELETE FROM table_dblink              returning : {delete, '$3', [],   '$4'}.
delete_statement_searched -> DELETE FROM table_dblink where_clause           : {delete, '$3', '$4', {returning, {}}}.
delete_statement_searched -> DELETE FROM table_dblink where_clause returning : {delete, '$3', '$4', '$5'}.

drop_cluster_def -> DROP CLUSTER cluster_name                         : {'drop cluster', '$3', {}}.
drop_cluster_def -> DROP CLUSTER cluster_name drop_cluster_extensions : {'drop cluster', '$3', '$4'}.

drop_cluster_extensions -> INCLUDING TABLES                     : {'including tables'}.
drop_cluster_extensions -> INCLUDING TABLES CASCADE CONSTRAINTS : {'including tables cascade constraints'}.

cluster_name ->                identifier : '$1'.
cluster_name -> identifier '.' identifier : list_to_binary(['$1',".",'$3']).

drop_context_def -> DROP CONTEXT identifier : {'drop context', '$3'}.

drop_database_def -> DROP DATABASE : {'drop database'}.

drop_database_link_def -> DROP        DATABASE LINK DBLINK : {'drop database link', unwrap_bin('$4'), {}}.
drop_database_link_def -> DROP PUBLIC DATABASE LINK DBLINK : {'drop database link', unwrap_bin('$5'), public}.

drop_directory_def -> DROP DIRECTORY identifier : {'drop directory', '$3'}.

drop_function_def -> DROP FUNCTION function_name : {'drop function', '$3'}.

function_name ->                NAME : unwrap_bin('$1').
function_name -> identifier '.' NAME : list_to_binary(['$1',".",unwrap_bin('$3')]).

drop_index_def -> DROP INDEX            FROM table                       : {'drop index', {},   '$4'}.
drop_index_def -> DROP INDEX            FROM table drop_index_extensions : {'drop index', {},   '$4', '$5'}.
drop_index_def -> DROP INDEX index_name                                  : {'drop index', '$3', []}.
drop_index_def -> DROP INDEX index_name            drop_index_extensions : {'drop index', '$3', [],   '$4'}.
drop_index_def -> DROP INDEX index_name FROM table                       : {'drop index', '$3', '$5'}.
drop_index_def -> DROP INDEX index_name FROM table drop_index_extensions : {'drop index', '$3', '$5', '$6'}.

drop_index_extensions ->              DEFERRED  INVALIDATION : {'deferred invalidation'}.
drop_index_extensions ->              IMMEDIATE INVALIDATION : {'immediate invalidation'}.
drop_index_extensions ->        FORCE                        : {'force'}.
drop_index_extensions ->        FORCE DEFERRED  INVALIDATION : {'force deferred invalidation'}.
drop_index_extensions ->        FORCE IMMEDIATE INVALIDATION : {'force immediate invalidation'}.
drop_index_extensions -> ONLINE                              : {'online'}.
drop_index_extensions -> ONLINE       DEFERRED  INVALIDATION : {'online deferred invalidation'}.
drop_index_extensions -> ONLINE       IMMEDIATE INVALIDATION : {'online immediate invalidation'}.
drop_index_extensions -> ONLINE FORCE                        : {'online force'}.
drop_index_extensions -> ONLINE FORCE DEFERRED  INVALIDATION : {'online force deferred invalidation'}.
drop_index_extensions -> ONLINE FORCE IMMEDIATE INVALIDATION : {'online force immediate invalidation'}.

drop_materialized_view_def -> DROP MATERIALIZED VIEW materialized_view_name                : {'drop materialized view', '$4', {}}.
drop_materialized_view_def -> DROP MATERIALIZED VIEW materialized_view_name PRESERVE TABLE : {'drop materialized view', '$4', 'preserve table'}.

materialized_view_name ->                identifier : '$1'.
materialized_view_name -> identifier '.' identifier : list_to_binary(['$1',".",'$3']).

drop_package_def -> DROP PACKAGE      package_name : {'drop package', '$3', {}}.
drop_package_def -> DROP PACKAGE BODY package_name : {'drop package', '$4', body}.

package_name ->                identifier : '$1'.
package_name -> identifier '.' identifier : list_to_binary(['$1',".",'$3']).

drop_procedure_def -> DROP PROCEDURE procedure_name : {'drop procedure', '$3'}.

procedure_name ->                identifier : '$1'.
procedure_name -> identifier '.' identifier : list_to_binary(['$1',".",'$3']).

drop_profile_def -> DROP PROFILE identifier         : {'drop profile', '$3', {}}.
drop_profile_def -> DROP PROFILE identifier CASCADE : {'drop profile', '$3', cascade}.

drop_role_def -> DROP ROLE identifier : {'drop role', '$3'}.

drop_sequence_def -> DROP SEQUENCE sequence_name : {'drop sequence', '$3'}.

sequence_name ->                identifier : '$1'.
sequence_name -> identifier '.' identifier : list_to_binary(['$1',".",'$3']).

drop_synonym_def -> DROP        SYNONYM synonym_name       : {'drop synonym', '$3', {},     {}}.
drop_synonym_def -> DROP        SYNONYM synonym_name FORCE : {'drop synonym', '$3', {},     force}.
drop_synonym_def -> DROP PUBLIC SYNONYM synonym_name       : {'drop synonym', '$4', public, {}}.
drop_synonym_def -> DROP PUBLIC SYNONYM synonym_name FORCE : {'drop synonym', '$4', public, force}.

synonym_name ->                identifier : '$1'.
synonym_name -> identifier '.' identifier : list_to_binary(['$1',".",'$3']).

drop_table_def -> DROP            TABLE        table_list                       : {'drop table', {'tables', '$3'}, {},   {},   []}.
drop_table_def -> DROP            TABLE        table_list drop_table_extensions : {'drop table', {'tables', '$3'}, {},   '$4', []}.
drop_table_def -> DROP            TABLE exists table_list                       : {'drop table', {'tables', '$4'}, '$3', {},   []}.
drop_table_def -> DROP            TABLE exists table_list drop_table_extensions : {'drop table', {'tables', '$4'}, '$3', '$5', []}.
drop_table_def -> DROP identifier TABLE        table_list                       : {'drop table', {'tables', '$4'}, {},   {},   binary_to_list('$2')}.
drop_table_def -> DROP identifier TABLE        table_list drop_table_extensions : {'drop table', {'tables', '$4'}, {},   '$5', binary_to_list('$2')}.
drop_table_def -> DROP identifier TABLE exists table_list                       : {'drop table', {'tables', '$5'}, '$4', {},   binary_to_list('$2')}.
drop_table_def -> DROP identifier TABLE exists table_list drop_table_extensions : {'drop table', {'tables', '$5'}, '$4', '$6', binary_to_list('$2')}.

drop_table_extensions ->                     PURGE : {'purge'}.
drop_table_extensions -> CASCADE CONSTRAINTS       : {'cascade constraints'}.
drop_table_extensions -> CASCADE CONSTRAINTS PURGE : {'cascade constraints purge'}.

drop_tablespace_def -> DROP TABLESPACE identifier                            : {'drop tablespace', '$3', {}}.
drop_tablespace_def -> DROP TABLESPACE identifier drop_tablespace_extensions : {'drop tablespace', '$3', '$4'}.

drop_tablespace_extensions ->            INCLUDING CONTENTS                                    : {'including contents'}.
drop_tablespace_extensions ->            INCLUDING CONTENTS                CASCADE CONSTRAINTS : {'including contents cascade constraints'}.
drop_tablespace_extensions ->            INCLUDING CONTENTS AND  DATAFILES CASCADE CONSTRAINTS : {'including contents and datafiles cascade constraints'}.
drop_tablespace_extensions ->            INCLUDING CONTENTS KEEP DATAFILES CASCADE CONSTRAINTS : {'including contents keep datafiles cascade constraints'}.
drop_tablespace_extensions -> DROP QUOTA                                                       : {'drop quota'}.
drop_tablespace_extensions -> DROP QUOTA INCLUDING CONTENTS                                    : {'drop quota including contents'}.
drop_tablespace_extensions -> DROP QUOTA INCLUDING CONTENTS                CASCADE CONSTRAINTS : {'drop quota including contents cascade constraints'}.
drop_tablespace_extensions -> DROP QUOTA INCLUDING CONTENTS AND  DATAFILES CASCADE CONSTRAINTS : {'drop quota including contents and datafiles cascade constraints'}.
drop_tablespace_extensions -> DROP QUOTA INCLUDING CONTENTS KEEP DATAFILES CASCADE CONSTRAINTS : {'drop quota including contents keep datafiles cascade constraints'}.
drop_tablespace_extensions -> KEEP QUOTA                                                       : {'keep quota'}.
drop_tablespace_extensions -> KEEP QUOTA INCLUDING CONTENTS                                    : {'keep quota including contents'}.
drop_tablespace_extensions -> KEEP QUOTA INCLUDING CONTENTS                CASCADE CONSTRAINTS : {'keep quota including contents cascade constraints'}.
drop_tablespace_extensions -> KEEP QUOTA INCLUDING CONTENTS AND  DATAFILES CASCADE CONSTRAINTS : {'keep quota including contents and datafiles cascade constraints'}.
drop_tablespace_extensions -> KEEP QUOTA INCLUDING CONTENTS KEEP DATAFILES CASCADE CONSTRAINTS : {'keep quota including contents keep datafiles cascade constraints'}.

drop_trigger_def -> DROP TRIGGER trigger_name : {'drop trigger', '$3'}.

trigger_name ->                identifier : '$1'.
trigger_name -> identifier '.' identifier : list_to_binary(['$1',".",'$3']).

drop_type_def -> DROP TYPE type_name          : {'drop type', '$3', {}}.
drop_type_def -> DROP TYPE type_name FORCE    : {'drop type', '$3', force}.
drop_type_def -> DROP TYPE type_name VALIDATE : {'drop type', '$3', validate}.

type_name ->                identifier : '$1'.
type_name -> identifier '.' identifier : list_to_binary(['$1',".",'$3']).

drop_type_body_def -> DROP TYPE BODY type_name : {'drop type body', '$4'}.

drop_view_def -> DROP VIEW table                     : {'drop view', '$3', {}}.
drop_view_def -> DROP VIEW table CASCADE CONSTRAINTS : {'drop view', '$3', 'cascade constraints'}.

fetch_statement -> FETCH cursor INTO target_commalist : {fetch, '$2', {into, '$4'}}.

insert_statement -> INSERT INTO table_dblink                                                         : {insert, '$3', {},           {},   {returning, {}}}.
insert_statement -> INSERT INTO table_dblink                                               returning : {insert, '$3', {},           {},   '$4'}.
insert_statement -> INSERT INTO table_dblink                          values_or_query_spec           : {insert, '$3', {cols, []},   '$4', {returning, {}}}.
insert_statement -> INSERT INTO table_dblink                          values_or_query_spec returning : {insert, '$3', {cols, []},   '$4', '$5'}.
insert_statement -> INSERT INTO table_dblink '(' column_commalist ')' values_or_query_spec           : {insert, '$3', {cols, '$5'}, '$7', {returning, {}}}.
insert_statement -> INSERT INTO table_dblink '(' column_commalist ')' values_or_query_spec returning : {insert, '$3', {cols, '$5'}, '$7', '$8'}.

values_or_query_spec -> VALUES '(' insert_atom_commalist ')' : {values, '$3'}.
values_or_query_spec ->     query_spec                       : '$1'.
values_or_query_spec -> '(' query_spec ')'                   : '$2'.

insert_atom_commalist ->                           insert_atom :         ['$1'].
insert_atom_commalist -> insert_atom_commalist ',' insert_atom : '$1' ++ ['$3'].

insert_atom -> scalar_opt_as_exp : '$1'.

open_statement -> OPEN cursor : {open, '$2'}.

rollback_statement -> ROLLBACK      : 'rollback'.
rollback_statement -> ROLLBACK WORK : 'rollback work'.

select_statement -> query_exp : '$1'.

hint -> HINT : {hints, unwrap_bin('$1')}.

all_distinct -> ALL      : {opt, <<"all">>}.
all_distinct -> DISTINCT : {opt, <<"distinct">>}.

truncate_cluster -> TRUNCATE CLUSTER cluster_name         : {'truncate cluster', '$3', {}}.
truncate_cluster -> TRUNCATE CLUSTER cluster_name storage : {'truncate cluster', '$3', '$4'}.

truncate_table -> TRUNCATE TABLE table                              : {'truncate table', '$3', {},   {}}.
truncate_table -> TRUNCATE TABLE table                      CASCADE : {'truncate table', '$3', {},   {},   cascade}.
truncate_table -> TRUNCATE TABLE table              storage         : {'truncate table', '$3', {},   '$4'}.
truncate_table -> TRUNCATE TABLE table              storage CASCADE : {'truncate table', '$3', {},   '$4', cascade}.
truncate_table -> TRUNCATE TABLE table materialized                 : {'truncate table', '$3', '$4', {}}.
truncate_table -> TRUNCATE TABLE table materialized         CASCADE : {'truncate table', '$3', '$4', {},   cascade}.
truncate_table -> TRUNCATE TABLE table materialized storage         : {'truncate table', '$3', '$4', '$5'}.
truncate_table -> TRUNCATE TABLE table materialized storage CASCADE : {'truncate table', '$3', '$4', '$5', cascade}.

materialized -> PRESERVE MATERIALIZED VIEW LOG : {'materialized view log', preserve}.
materialized -> PURGE    MATERIALIZED VIEW LOG : {'materialized view log', purge}.

storage ->  DROP      STORAGE : {storage, drop}.
storage ->  DROP  ALL STORAGE : {storage, 'drop all'}.
storage ->  REUSE     STORAGE : {storage, reuse}.

update_statement_positioned -> UPDATE table_dblink SET assignment_commalist WHERE CURRENT OF cursor           : {update, '$2', {set, '$4'}, {where_current_of, '$8'}, {returning, {}}}.
update_statement_positioned -> UPDATE table_dblink SET assignment_commalist WHERE CURRENT OF cursor returning : {update, '$2', {set, '$4'}, {where_current_of, '$8'}, '$9'}.

assignment_commalist ->                          assignment :         ['$1'].
assignment_commalist -> assignment_commalist ',' assignment : '$1' ++ ['$3'].

assignment -> column '=' scalar_opt_as_exp : {'=', '$1', '$3'}.

update_statement_searched -> UPDATE table_dblink SET assignment_commalist                        : {update, '$2', {set, '$4'}, [],   {returning, {}}}.
update_statement_searched -> UPDATE table_dblink SET assignment_commalist              returning : {update, '$2', {set, '$4'}, [],   '$5'}.
update_statement_searched -> UPDATE table_dblink SET assignment_commalist where_clause           : {update, '$2', {set, '$4'}, '$5', {returning, {}}}.
update_statement_searched -> UPDATE table_dblink SET assignment_commalist where_clause returning : {update, '$2', {set, '$4'}, '$5', '$6'}.

target_commalist ->                      target :         ['$1'].
target_commalist -> target_commalist ',' target : '$1' ++ ['$3'].

target -> identifier    : '$1'.
target -> parameter_ref : '$1'.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query expressions
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

query_exp ->                     query_term : '$1'.
query_exp -> query_exp UNION     query_term : {union,       '$1', '$3'}.
query_exp -> query_exp UNION ALL query_term : {'union all', '$1', '$4'}.
query_exp -> query_exp INTERSECT query_term : {intersect,   '$1', '$3'}.
query_exp -> query_exp MINUS     query_term : {minus,       '$1', '$3'}.

returning -> RETURNING selection INTO selection : {returning, '$2', '$4'}.
returning -> RETURN    selection INTO selection : {return,    '$2', '$4'}.

query_term ->     query_spec          : '$1'.
query_term -> '(' query_exp  ')'      : '$2'.
query_term -> '(' query_exp  ')' JSON : {'$2', jpparse(list_to_binary([unwrap('$4')])), '('}.

%%%%% foreach
query_spec -> FOREACH selection table_exp
              : {foreach,[{fields, '$2'}] ++ '$3'}.
query_spec -> FOREACH selection do_clause table_exp
              : {foreach, [{fields, '$2'}, '$3'] ++ '$4'}.
query_spec -> FOREACH selection do_clause incase_clause table_exp
              : {foreach, [{fields, '$2'}, '$3', '$4'] ++'$5'}.
query_spec -> FOREACH selection incase_clause table_exp
              : {foreach, [{fields, '$2'}, '$3'] ++'$4'}.
query_spec -> SELECT                   selection      table_exp : {select,
                                                                   [{fields, '$2'}] ++
                                                                   '$3'}.
query_spec -> SELECT                   selection into table_exp : {select,
                                                                   [{fields, '$2'}] ++
                                                                   if '$3' == {} -> []; true -> [{into, '$3'}] end ++
                                                                   '$4'}.
query_spec -> SELECT      all_distinct selection      table_exp : {select,
                                                                   if '$2' == {} -> []; true -> ['$2'] end ++
                                                                   [{fields, '$3'}] ++
                                                                   '$4'}.
query_spec -> SELECT      all_distinct selection into table_exp : {select,
                                                                   if '$2' == {} -> []; true -> ['$2'] end ++
                                                                   [{fields, '$3'}] ++
                                                                   if '$4' == {} -> []; true -> [{into, '$4'}] end ++
                                                                   '$5'}.
query_spec -> SELECT hint              selection      table_exp : {select,
                                                                   if '$2' == {} -> []; true -> ['$2'] end ++
                                                                   [{fields, '$3'}] ++
                                                                   '$4'}.
query_spec -> SELECT hint              selection into table_exp : {select,
                                                                   if '$2' == {} -> []; true -> ['$2'] end ++
                                                                   [{fields, '$3'}] ++
                                                                   if '$4' == {} -> []; true -> [{into, '$4'}] end ++
                                                                   '$5'}.
query_spec -> SELECT hint all_distinct selection      table_exp : {select,
                                                                   if '$2' == {} -> []; true -> ['$2'] end ++
                                                                   if '$3' == {} -> []; true -> ['$3'] end ++
                                                                   [{fields, '$4'}] ++
                                                                   '$5'}.
query_spec -> SELECT hint all_distinct selection into table_exp : {select,
                                                                   if '$2' == {} -> []; true -> ['$2'] end ++
                                                                   if '$3' == {} -> []; true -> ['$3'] end ++
                                                                   [{fields, '$4'}] ++
                                                                   if '$5' == {} -> []; true -> [{into, '$5'}] end ++
                                                                   '$6'}.
do_clause -> DO selection: {'do', '$2'}.

into -> INTO target_commalist : '$2'.

selection -> select_field_commalist : '$1'.

select_field -> case_when_opt_as_exp : ['$1'].
select_field -> scalar_opt_as_exp    : ['$1'].
select_field -> '*'                  : [<<"*">>].

select_field_commalist ->                            select_field :         '$1'.
select_field_commalist -> select_field_commalist ',' select_field : '$1' ++ '$3'.

case_when_opt_as_exp -> case_when_exp         : '$1'.
case_when_opt_as_exp -> case_when_exp    NAME : {as, '$1', unwrap_bin('$2')}.
case_when_opt_as_exp -> case_when_exp AS NAME : {as, '$1', unwrap_bin('$3')}.

case_when_exp -> CASE                   case_when_then_list      END : {'case', <<>>, '$2', {}}.
case_when_exp -> CASE                   case_when_then_list else END : {'case', <<>>, '$2', '$3'}.
case_when_exp -> CASE scalar_opt_as_exp case_when_then_list      END : {'case', '$2', '$3', {}}.
case_when_exp -> CASE scalar_opt_as_exp case_when_then_list else END : {'case', '$2', '$3', '$4'}.
case_when_exp -> '(' case_when_exp ')'                               : '$2'.

case_when_then_list -> case_when_then                     : ['$1'].
case_when_then_list -> case_when_then case_when_then_list : ['$1'|'$2'].

case_when_then -> WHEN search_condition THEN scalar_opt_as_exp : {'$2', '$4'}.

else -> ELSE scalar_opt_as_exp : '$2'.

table_exp -> from_clause                                                                                      : ['$1', {where, {}}, {'hierarchical query', {}}, {'group by', []},  {having, {}}, {'order by', []}].
table_exp -> from_clause                                                                      order_by_clause : ['$1', {where, {}}, {'hierarchical query', {}}, {'group by', []},  {having, {}}, '$2'].
table_exp -> from_clause                                                        having_clause                 : ['$1', {where, {}}, {'hierarchical query', {}}, {'group by', []},  '$2',         {'order by', []}].
table_exp -> from_clause                                                        having_clause order_by_clause : ['$1', {where, {}}, {'hierarchical query', {}}, {'group by', []},  '$2',         '$3'].
table_exp -> from_clause                                        group_by_clause                               : ['$1', {where, {}}, {'hierarchical query', {}}, '$2',              {having, {}}, {'order by', []}].
table_exp -> from_clause                                        group_by_clause               order_by_clause : ['$1', {where, {}}, {'hierarchical query', {}}, '$2',              {having, {}}, '$3'].
table_exp -> from_clause                                        group_by_clause having_clause                 : ['$1', {where, {}}, {'hierarchical query', {}}, '$2',              '$3',         {'order by', []}].
table_exp -> from_clause                                        group_by_clause having_clause order_by_clause : ['$1', {where, {}}, {'hierarchical query', {}}, '$2',              '$3',         '$4'].
table_exp -> from_clause              hierarchical_query_clause                                               : ['$1', {where, {}}, '$2',                       {'group by', []},  {having, {}}, {'order by', []}].
table_exp -> from_clause              hierarchical_query_clause                               order_by_clause : ['$1', {where, {}}, '$2',                       {'group by', []},  {having, {}}, '$3'].
table_exp -> from_clause              hierarchical_query_clause                 having_clause                 : ['$1', {where, {}}, '$2',                       {'group by', []},  '$3',         {'order by', []}].
table_exp -> from_clause              hierarchical_query_clause                 having_clause order_by_clause : ['$1', {where, {}}, '$2',                       {'group by', []},  '$3',         '$4'].
table_exp -> from_clause              hierarchical_query_clause group_by_clause                               : ['$1', {where, {}}, '$2',                       '$3',              {having, {}}, {'order by', []}].
table_exp -> from_clause              hierarchical_query_clause group_by_clause               order_by_clause : ['$1', {where, {}}, '$2',                       '$3',              {having, {}}, '$4'].
table_exp -> from_clause              hierarchical_query_clause group_by_clause having_clause                 : ['$1', {where, {}}, '$2',                       '$3',              '$4',         {'order by', []}].
table_exp -> from_clause              hierarchical_query_clause group_by_clause having_clause order_by_clause : ['$1', {where, {}}, '$2',                       '$3',              '$4',         '$5'].
table_exp -> from_clause where_clause                                                                         : ['$1', '$2',        {'hierarchical query', {}}, {'group by', []},  {having, {}}, {'order by', []}].
table_exp -> from_clause where_clause                                                         order_by_clause : ['$1', '$2',        {'hierarchical query', {}}, {'group by', []},  {having, {}}, '$3'].
table_exp -> from_clause where_clause                                           having_clause                 : ['$1', '$2',        {'hierarchical query', {}}, {'group by', []},  '$3', {'order by', []}].
table_exp -> from_clause where_clause                                           having_clause order_by_clause : ['$1', '$2',        {'hierarchical query', {}}, {'group by', []},  '$3',         '$4'].
table_exp -> from_clause where_clause                           group_by_clause                               : ['$1', '$2',        {'hierarchical query', {}}, '$3',              {having, {}}, {'order by', []}].
table_exp -> from_clause where_clause                           group_by_clause               order_by_clause : ['$1', '$2',        {'hierarchical query', {}}, '$3',              {having, {}}, '$4'].
table_exp -> from_clause where_clause                           group_by_clause having_clause                 : ['$1', '$2',        {'hierarchical query', {}}, '$3',              '$4',         {'order by', []}].
table_exp -> from_clause where_clause                           group_by_clause having_clause order_by_clause : ['$1', '$2',        {'hierarchical query', {}}, '$3',              '$4',         '$5'].
table_exp -> from_clause where_clause hierarchical_query_clause                                               : ['$1', '$2',        '$3',                       {'group by', []},  {having, {}}, {'order by', []}].
table_exp -> from_clause where_clause hierarchical_query_clause                               order_by_clause : ['$1', '$2',        '$3',                       {'group by', []},  {having, {}}, '$4'].
table_exp -> from_clause where_clause hierarchical_query_clause                 having_clause                 : ['$1', '$2',        '$3',                       {'group by', []},  '$4', {'order by', []}].
table_exp -> from_clause where_clause hierarchical_query_clause                 having_clause order_by_clause : ['$1', '$2',        '$3',                       {'group by', []},  '$4',         '$5'].
table_exp -> from_clause where_clause hierarchical_query_clause group_by_clause                               : ['$1', '$2',        '$3',                       '$4',              {having, {}}, {'order by', []}].
table_exp -> from_clause where_clause hierarchical_query_clause group_by_clause               order_by_clause : ['$1', '$2',        '$3',                       '$4',              {having, {}}, '$5'].
table_exp -> from_clause where_clause hierarchical_query_clause group_by_clause having_clause                 : ['$1', '$2',        '$3',                       '$4',              '$5',         {'order by', []}].
table_exp -> from_clause where_clause hierarchical_query_clause group_by_clause having_clause order_by_clause : ['$1', '$2',        '$3',                       '$4',              '$5',         '$6'].

from_clause -> FROM from_column_commalist : {from, '$2'}.

from_column -> table_ref           : ['$1'].
from_column -> '(' join_clause ')' : ['$2'].
from_column ->     join_clause     : ['$1'].

from_column_commalist ->                           from_column :        '$1'.
from_column_commalist -> from_column_commalist ',' from_column : '$1'++ '$3'.

join_clause -> table_ref join_list : {'$1', '$2'}.

join -> inner_cross_join : '$1'.
join -> outer_join       : '$1'.

join_list ->           join :        ['$1'].
join_list -> join_list join : '$1'++ ['$2'].

inner_cross_join ->               JOIN join_ref join_on_or_using_clause : {join,               '$2', '$3'}.
inner_cross_join -> CROSS         JOIN join_ref                         : {cross_join,         '$3'}.
inner_cross_join -> INNER         JOIN join_ref join_on_or_using_clause : {join_inner,         '$3', '$4'}.
inner_cross_join -> NATURAL       JOIN join_ref                         : {natural_join,       '$3'}.
inner_cross_join -> NATURAL INNER JOIN join_ref                         : {natural_inner_join, '$4'}.

join_on_or_using_clause -> ON search_condition                  : {on,    '$2'}.
join_on_or_using_clause -> USING '(' select_field_commalist ')' : {using, '$3'}.

% ----------------------------------------------------------------------------------------------- {{join_type, partition, opt_natural} ... }
outer_join ->                                outer_join_type JOIN join_ref                        join_on_or_using_clause : {{'$1', {},   {}},      '$3', {},   '$4'}.
outer_join ->                                outer_join_type JOIN join_ref                                                : {{'$1', {},   {}},      '$3', {},   {}}.
outer_join ->                                outer_join_type JOIN join_ref query_partition_clause                         : {{'$1', {},   {}},      '$3', '$4', {}}.
outer_join ->                                outer_join_type JOIN join_ref query_partition_clause join_on_or_using_clause : {{'$1', {},   {}},      '$3', '$4', '$5'}.
outer_join -> NATURAL                        outer_join_type JOIN join_ref                        join_on_or_using_clause : {{'$2', {},   natural}, '$4', {},   '$5'}.
outer_join -> NATURAL                        outer_join_type JOIN join_ref                                                : {{'$2', {},   natural}, '$4', {},   {}}.
outer_join -> NATURAL                        outer_join_type JOIN join_ref query_partition_clause                         : {{'$2', {},   natural}, '$4', '$5', {}}.
outer_join -> NATURAL                        outer_join_type JOIN join_ref query_partition_clause join_on_or_using_clause : {{'$2', {},   natural}, '$4', '$5', '$6'}.
outer_join -> query_partition_clause         outer_join_type JOIN join_ref                        join_on_or_using_clause : {{'$2', '$1', {}},      '$4', {},   '$5'}.
outer_join -> query_partition_clause         outer_join_type JOIN join_ref                                                : {{'$2', '$1', {}},      '$4', {},   {}}.
outer_join -> query_partition_clause         outer_join_type JOIN join_ref query_partition_clause                         : {{'$2', '$1', {}},      '$4', '$5', {}}.
outer_join -> query_partition_clause         outer_join_type JOIN join_ref query_partition_clause join_on_or_using_clause : {{'$2', '$1', {}},      '$4', '$5', '$6'}.
outer_join -> query_partition_clause NATURAL outer_join_type JOIN join_ref                        join_on_or_using_clause : {{'$3', '$1', natural}, '$5', {},   '$6'}.
outer_join -> query_partition_clause NATURAL outer_join_type JOIN join_ref                                                : {{'$3', '$1', natural}, '$5', {},   {}}.
outer_join -> query_partition_clause NATURAL outer_join_type JOIN join_ref query_partition_clause                         : {{'$3', '$1', natural}, '$5', '$6', {}}.
outer_join -> query_partition_clause NATURAL outer_join_type JOIN join_ref query_partition_clause join_on_or_using_clause : {{'$3', '$1', natural}, '$5', '$6', '$7'}.
% -----------------------------------------------------------------------------------------------

query_partition_clause -> PARTITION BY     scalar_exp_commalist     : {partition_by, '$3'} .
query_partition_clause -> PARTITION BY '(' scalar_exp_commalist ')' : {partition_by, '$4'}.

outer_join_type -> FULL        : full.
outer_join_type -> FULL  OUTER : full_outer.
outer_join_type -> LEFT        : left.
outer_join_type -> LEFT  OUTER : left_outer.
outer_join_type -> RIGHT       : right.
outer_join_type -> RIGHT OUTER : right_outer.

table_ref -> table_dblink      : '$1'.
table_ref -> query_term        : '$1'.
table_ref -> query_term   NAME : {as, '$1', unwrap_bin('$2')}.

join_ref -> table_dblink      : '$1'.
join_ref -> query_term        : '$1'.
join_ref -> query_term   NAME : {as, '$1', unwrap_bin('$2')}.

hierarchical_query_clause -> START WITH         search_condition CONNECT BY         search_condition : {'hierarchical query', {{'start with', '$3'},       {'connect by', <<>>, '$6'}}}.
hierarchical_query_clause -> START WITH         search_condition CONNECT BY nocycle search_condition : {'hierarchical query', {{'start with', '$3'},       {'connect by', '$6', '$7'}}}.
hierarchical_query_clause -> CONNECT BY         search_condition START WITH         search_condition : {'hierarchical query', {{'connect by', <<>>, '$3'}, {'start with', '$6'}}}.
hierarchical_query_clause -> CONNECT BY nocycle search_condition START WITH         search_condition : {'hierarchical query', {{'connect by', '$3', '$4'}, {'start with', '$7'}}}.

nocycle -> NOCYCLE : <<"nocycle">>.

incase_clause -> INCASE search_condition : {incase, '$2'}.
where_clause -> WHERE search_condition : {where, '$2'}.

group_by_clause -> GROUP BY column_ref_commalist : {'group by', '$3'}.

column_ref_commalist ->                          column_ref   :         ['$1'].
column_ref_commalist ->                          function_ref :         ['$1'].
column_ref_commalist -> column_ref_commalist ',' column_ref   : '$1' ++ ['$3'].
column_ref_commalist -> column_ref_commalist ',' function_ref : '$1' ++ ['$3'].

having_clause -> HAVING search_condition : {having, '$2'}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% search conditions
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

search_condition -> search_condition OR  search_condition : {'or',  '$1', '$3'}.
search_condition -> search_condition AND search_condition : {'and', '$1', '$3'}.
search_condition -> NOT search_condition                  : {'not', '$2'}.
search_condition -> '(' search_condition ')'              : '$2'.
search_condition -> predicate                             : '$1'.

predicate -> comparison_predicate : '$1'.
predicate -> between_predicate    : '$1'.
predicate -> like_predicate       : '$1'.
predicate -> test_for_null        : '$1'.
predicate -> in_predicate         : '$1'.
predicate -> all_or_any_predicate : '$1'.
predicate -> existence_test       : '$1'.

comparison_predicate -> scalar_opt_as_exp                            : '$1'.
comparison_predicate ->       scalar_exp '='        PRIOR scalar_exp : {'=',          '$1',          {prior, '$4'}}.
comparison_predicate ->       scalar_exp COMPARISON PRIOR scalar_exp : {unwrap('$2'), '$1',          {prior, '$4'}}.
comparison_predicate -> PRIOR scalar_exp '='              scalar_exp : {'=',          {prior, '$2'}, '$4'}.
comparison_predicate -> PRIOR scalar_exp COMPARISON       scalar_exp : {unwrap('$3'), {prior, '$2'}, '$4'}.

between_predicate -> scalar_exp     BETWEEN scalar_exp AND scalar_exp:         {between, '$1', '$3', '$5'}.
between_predicate -> scalar_exp not_between scalar_exp AND scalar_exp: {'not', {between, '$1', '$3', '$5'}}.

not_between -> NOT BETWEEN : 'not between'.

like_predicate -> scalar_exp     LIKE scalar_exp        :         {like, '$1', '$3', <<>>}.
like_predicate -> scalar_exp     LIKE scalar_exp escape :         {like, '$1', '$3', '$4'}.
like_predicate -> scalar_exp not_like scalar_exp        : {'not', {like, '$1', '$3', <<>>}}.
like_predicate -> scalar_exp not_like scalar_exp escape : {'not', {like, '$1', '$3', '$4'}}.

not_like -> NOT LIKE  : 'not like'.

escape -> ESCAPE atom : '$2'.

test_for_null -> scalar_exp is_null     :         {'is', '$1', <<"null">>}.
test_for_null -> scalar_exp is_not_null : {'not', {'is', '$1', <<"null">>}}.

is_not_null -> IS NOT NULLX : 'is not'.

is_null -> IS NULLX : is.

in_predicate -> scalar_exp     IN '(' scalar_exp_commalist ')' :         {in, '$1', {list, '$4'}}.
in_predicate -> scalar_exp     IN '(' subquery ')'             :         {in, '$1', '$4'}.
in_predicate -> scalar_exp not_in '(' scalar_exp_commalist ')' : {'not', {in, '$1', {list, '$4'}}}.
in_predicate -> scalar_exp not_in '(' subquery ')'             : {'not', {in, '$1', '$4'}}.

not_in -> NOT IN : 'not in'.

all_or_any_predicate -> scalar_exp '='        any_all_some subquery : {'=',          '$1', {'$3', ['$4']}}.
all_or_any_predicate -> scalar_exp COMPARISON any_all_some subquery : {unwrap('$2'), '$1', {'$3', ['$4']}}.

any_all_some -> ANY  : any.
any_all_some -> ALL  : all.
any_all_some -> SOME : some.

existence_test -> EXISTS subquery : {exists, '$2'}.

% Optional plus (+) is not supported in table_collection_expression:
%
% The optional plus (+) is relevant if you are joining the TABLE collection expression with
% the parent table. The + creates an outer join of the two, so that the query returns rows
% from the outer table even if the collection expression is null.

table_coll_expr -> TABLE '(' column_ref   ')' : {table_coll_expr, '$3'}.
table_coll_expr -> TABLE '(' function_ref ')' : {table_coll_expr, '$3'}.
table_coll_expr -> TABLE '(' subquery     ')' : {table_coll_expr, '$3'}.

subquery -> query_exp : '$1'.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% scalar expressions
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scalar_opt_as_exp_1 -> scalar_exp                       : '$1'.
scalar_opt_as_exp_1 -> scalar_exp '='        scalar_exp : {'=',          '$1', '$3'}.
scalar_opt_as_exp_1 -> scalar_exp COMPARISON scalar_exp : {unwrap('$2'), '$1', '$3'}.

scalar_opt_as_exp_2 -> scalar_exp    NAME : {as, '$1', unwrap_bin('$2')}.
scalar_opt_as_exp_2 -> scalar_exp AS NAME : {as, '$1', unwrap_bin('$3')}.

scalar_opt_as_exp -> scalar_opt_as_exp_1 : '$1'.
scalar_opt_as_exp -> scalar_opt_as_exp_2 : '$1'.

scalar_exp -> scalar_sub_exp '||' scalar_exp : {'||','$1','$3'}.
scalar_exp -> scalar_sub_exp                 : '$1'.

scalar_sub_exp -> scalar_sub_exp '+'    scalar_sub_exp : {'+','$1','$3'}.
scalar_sub_exp -> scalar_sub_exp '-'    scalar_sub_exp : {'-','$1','$3'}.
scalar_sub_exp -> scalar_sub_exp '*'    scalar_sub_exp : {'*','$1','$3'}.
scalar_sub_exp -> scalar_sub_exp '/'    scalar_sub_exp : {'/','$1','$3'}.
scalar_sub_exp -> scalar_sub_exp 'div'  scalar_sub_exp : {'div','$1','$3'}.
scalar_sub_exp -> unary_add_or_subtract scalar_sub_exp : {'$1','$2'}.
scalar_sub_exp -> NULLX                                : <<"NULL">>.
scalar_sub_exp -> atom                                 : '$1'.
scalar_sub_exp -> subquery                             : '$1'.
scalar_sub_exp -> column_ref                           : '$1'.
scalar_sub_exp -> function_ref                         : '$1'.
scalar_sub_exp -> '(' scalar_sub_exp ')'               : '$2'.
scalar_sub_exp -> '(' scalar_sub_exp ')' JSON          : {'$2', jpparse(list_to_binary([unwrap('$4')])), '('}.

unary_add_or_subtract -> '+' : '+'.
unary_add_or_subtract -> '-' : '-'.

scalar_exp_commalist ->                          scalar_opt_as_exp :         ['$1'].
scalar_exp_commalist -> scalar_exp_commalist ',' scalar_opt_as_exp : '$1' ++ ['$3'].

atom -> parameter_ref : '$1'.
atom -> literal       : '$1'.

parameter_ref -> parameter                     : '$1'.
parameter_ref -> parameter           parameter : {'$1', '$2'}.
parameter_ref -> parameter INDICATOR parameter : {indicator, '$1', '$3'}.

function_ref -> function_ref JSON                                               : {'$1', jpparse(list_to_binary([unwrap('$2')])), []}.
function_ref -> FUNS                                                            : {'fun', unwrap_bin('$1'), []}.
function_ref -> FUNS '('                     ')'                                : {'fun', unwrap_bin('$1'), []}.
function_ref -> FUNS '(' '*'                 ')'                                : {'fun', unwrap_bin('$1'), [<<"*">>]}.
function_ref -> FUNS '(' ALL      scalar_exp ')'                                : {'fun', unwrap_bin('$1'), [{all,      '$4'}]}.
function_ref -> FUNS '(' DISTINCT column_ref ')'                                : {'fun', unwrap_bin('$1'), [{distinct, '$4'}]}.
function_ref -> FUNS '(' fun_args            ')'                                : {'fun', unwrap_bin('$1'), make_list('$3')}.
function_ref -> FUNS '(' fun_args_named      ')'                                : {'fun', unwrap_bin('$1'), make_list('$3')}.
function_ref -> identifier                               '('                ')' : {'fun', '$1', []}.
function_ref -> identifier                               '(' fun_args       ')' : {'fun', '$1', make_list('$3')}.
function_ref -> identifier                               '(' fun_args_named ')' : {'fun', '$1', make_list('$3')}.
function_ref -> identifier '.' identifier                '('                ')' : {'fun', list_to_binary(['$1', ".", '$3']), []}.
function_ref -> identifier '.' identifier                '(' fun_args       ')' : {'fun', list_to_binary(['$1', ".", '$3']), make_list('$5')}.
function_ref -> identifier '.' identifier                '(' fun_args_named ')' : {'fun', list_to_binary(['$1', ".", '$3']), make_list('$5')}.
function_ref -> identifier '.' identifier '.' identifier '('                ')' : {'fun', list_to_binary(['$1', ".", '$3', ".", '$5']), []}.
function_ref -> identifier '.' identifier '.' identifier '(' fun_args       ')' : {'fun', list_to_binary(['$1', ".", '$3', ".", '$5']), make_list('$7')}.
function_ref -> identifier '.' identifier '.' identifier '(' fun_args_named ')' : {'fun', list_to_binary(['$1', ".", '$3', ".", '$5']), make_list('$7')}.

fun_args -> fun_arg              : ['$1'].
fun_args -> fun_arg ',' fun_args : ['$1' | '$3'].

fun_arg -> '(' fun_arg ')'               : '$2'.
fun_arg -> atom                          : '$1'.
fun_arg -> case_when_exp                 : '$1'.
fun_arg -> column_ref                    : '$1'.
fun_arg -> fun_arg       NAME            : {as,   '$1',unwrap_bin('$2')}.
fun_arg -> fun_arg '*'   fun_arg         : {'*',  '$1','$3'}.
fun_arg -> fun_arg '+'   fun_arg         : {'+',  '$1','$3'}.
fun_arg -> fun_arg '-'   fun_arg         : {'-',  '$1','$3'}.
fun_arg -> fun_arg '/'   fun_arg         : {'/',  '$1','$3'}.
fun_arg -> fun_arg '='   fun_arg         : {'=',  '$1','$3'}.
fun_arg -> fun_arg 'div' fun_arg         : {'div','$1','$3'}.
fun_arg -> fun_arg '||'  fun_arg         : {'||', '$1','$3'}.
fun_arg -> fun_arg AS    NAME            : {explicit_as,  '$1', unwrap_bin('$3')}.
fun_arg -> fun_arg COMPARISON fun_arg    : {unwrap('$2'), '$1', '$3'}.
fun_arg -> function_ref                  : '$1'.
fun_arg -> NULLX                         : <<"NULL">>.
fun_arg -> subquery                      : '$1'.
fun_arg -> unary_add_or_subtract fun_arg : {'$1', '$2'}.

fun_args_named -> fun_arg_named                    : ['$1'].
fun_args_named -> fun_arg_named ',' fun_args_named : ['$1' | '$3'].

fun_arg_named -> identifier '=>' identifier : {'=>', '$1', '$3'}.
fun_arg_named -> identifier '=>' literal    : {'=>', '$1', '$3'}.
fun_arg_named -> identifier '=>' parameter  : {'=>', '$1', '$3'}.

literal -> STRING    : unwrap_const('$1').
literal -> INTNUM    : unwrap_const('$1').
literal -> APPROXNUM : unwrap_const('$1').

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% miscellaneous
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

table -> identifier                : '$1'.
table -> identifier '.' identifier : list_to_binary(['$1',".",'$3']).
table -> parameter                 : '$1'.

table_alias -> identifier                NAME : {as, '$1',                            unwrap_bin('$2')}.
table_alias -> identifier '.' identifier NAME : {as, list_to_binary(['$1',".",'$3']), unwrap_bin('$4')}.
table_alias -> parameter                 NAME : {as, '$1',                            unwrap_bin('$2')}.
table_alias -> table                          : '$1'.

table_dblink -> identifier          DBLINK                 : {    '$1',                                              {dblink, unwrap_bin('$2')}}.
table_dblink -> identifier          DBLINK            NAME : {as, '$1',                            unwrap_bin('$3'), {dblink, unwrap_bin('$2')}}.
table_dblink -> identifier      '.' identifier DBLINK      : {    list_to_binary(['$1',".",'$3']),                   {dblink, unwrap_bin('$4')}}.
table_dblink -> identifier      '.' identifier DBLINK NAME : {as, list_to_binary(['$1',".",'$3']), unwrap_bin('$5'), {dblink, unwrap_bin('$4')}}.
table_dblink -> parameter           DBLINK                 : {    '$1',                                              {dblink, unwrap_bin('$2')}}.
table_dblink -> parameter           DBLINK            NAME : {as, '$1',                            unwrap_bin('$3'), {dblink, unwrap_bin('$2')}}.
table_dblink -> table_alias                                : '$1'.
table_dblink -> table_coll_expr                            : '$1'.

column_ref ->                               identifier             : '$1'.
column_ref ->                               identifier '(' '+' ')' : list_to_binary(['$1',"(+)"]).
column_ref ->                               identifier JSON        : jpparse(list_to_binary(['$1',unwrap('$2')])).
column_ref ->                               identifier '.' '*'     : list_to_binary(['$1',".*"]).
column_ref ->                identifier '.' identifier             : list_to_binary(['$1',".",'$3']).
column_ref ->                identifier '.' identifier '(' '+' ')' : list_to_binary(['$1',".",'$3',"(+)"]).
column_ref ->                identifier '.' identifier JSON        : jpparse(list_to_binary(['$1',".",'$3',unwrap('$4')])).
column_ref ->                identifier '.' identifier '.' '*'     : list_to_binary(['$1',".",'$3',".*"]).
column_ref -> identifier '.' identifier '.' identifier             : list_to_binary(['$1',".",'$3',".",'$5']).
column_ref -> identifier '.' identifier '.' identifier '(' '+' ')' : list_to_binary(['$1',".",'$3',".",'$5',"(+)"]).
column_ref -> identifier '.' identifier '.' identifier JSON        : jpparse(list_to_binary(['$1',".",'$3',".",'$5',unwrap('$6')])).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% data types
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_type -> BFILE                                 : unwrap_bin('$1').
data_type -> BINARY_DOUBLE                         : unwrap_bin('$1').
data_type -> BINARY_FLOAT                          : unwrap_bin('$1').
data_type -> BLOB                                  : unwrap_bin('$1').
data_type -> CHAR                                  : unwrap_bin('$1').
data_type -> CLOB                                  : unwrap_bin('$1').
data_type -> DATE                                  : unwrap_bin('$1').
data_type -> LONG                                  : unwrap_bin('$1').
data_type -> LONG RAW                              : list_to_binary([unwrap_bin('$1')," ",unwrap_bin('$2')]).
data_type -> NAME                                  : unwrap_bin('$1').
data_type -> identifier '.' NAME                   : list_to_binary(['$1',".",unwrap_bin('$3')]).
data_type -> NCLOB                                 : unwrap_bin('$1').
data_type -> NUMBER                                : unwrap_bin('$1').
data_type -> RAW                                   : unwrap_bin('$1').
data_type -> ROWID                                 : unwrap_bin('$1').
data_type -> TIMESTAMP                             : unwrap_bin('$1').
data_type -> UROWID                                : unwrap_bin('$1').
data_type -> VARCHAR2                              : unwrap_bin('$1').
data_type -> XMLTYPE                               : unwrap_bin('$1').
data_type -> CHAR      '(' sgn_num ')'             : {unwrap_bin('$1'), '$3'}.
data_type -> NAME      '(' sgn_num ')'             : {unwrap_bin('$1'), '$3'}.
data_type -> NCHAR     '(' sgn_num ')'             : {unwrap_bin('$1'), '$3'}.
data_type -> NUMBER    '(' sgn_num ')'             : {unwrap_bin('$1'), '$3'}.
data_type -> NVARCHAR2 '(' sgn_num ')'             : {unwrap_bin('$1'), '$3'}.
data_type -> RAW       '(' sgn_num ')'             : {unwrap_bin('$1'), '$3'}.
data_type -> TIMESTAMP '(' sgn_num ')'             : {unwrap_bin('$1'), '$3'}.
data_type -> UROWID    '(' sgn_num ')'             : {unwrap_bin('$1'), '$3'}.
data_type -> VARCHAR2  '(' sgn_num ')'             : {unwrap_bin('$1'), '$3'}.
data_type -> NAME      '(' sgn_num ',' sgn_num ')' : {unwrap_bin('$1'), '$3', '$5'}.
data_type -> NUMBER    '(' sgn_num ',' sgn_num ')' : {unwrap_bin('$1'), '$3', '$5'}.

sgn_num ->     INTNUM : unwrap_bin('$1').
sgn_num -> '-' INTNUM : list_to_binary(["-",unwrap_bin('$2')]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the various things you can name
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

column -> identifier : '$1'.

cursor -> identifier : {cur, binary_to_list('$1')}.

parameter -> PARAMETER : {param, unwrap_bin('$1')}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% identifier
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

identifier -> NAME            : unwrap_bin('$1').

identifier -> ADMIN           : unwrap_bin('$1').
% identifier -> ALL             : unwrap_bin('$1').    Oracle reserved
% identifier -> ALTER           : unwrap_bin('$1').    Oracle reserved
% identifier -> AND             : unwrap_bin('$1').    Oracle reserved
% identifier -> ANY             : unwrap_bin('$1').    Oracle reserved
% identifier -> APPROXNUM       : unwrap_bin('$1').    reduce/reduce problem
% identifier -> AS              : unwrap_bin('$1').    Oracle reserved
% identifier -> ASC             : unwrap_bin('$1').    Oracle reserved
identifier -> AUTHENTICATION  : unwrap_bin('$1').
identifier -> AUTHORIZATION   : unwrap_bin('$1').
identifier -> BAG             : unwrap_bin('$1').
identifier -> BEGIN           : unwrap_bin('$1').
% identifier -> BETWEEN         : unwrap_bin('$1').    Oracle reserved
identifier -> BFILE           : unwrap_bin('$1').
identifier -> BINARY_DOUBLE   : unwrap_bin('$1').
identifier -> BINARY_FLOAT    : unwrap_bin('$1').
identifier -> BITMAP          : unwrap_bin('$1').
identifier -> BLOB            : unwrap_bin('$1').
identifier -> BODY            : unwrap_bin('$1').
% identifier -> BY              : unwrap_bin('$1').    Oracle reserved
identifier -> CALL            : unwrap_bin('$1').
identifier -> CASCADE         : unwrap_bin('$1').
% identifier -> CASE            : unwrap_bin('$1').    syntax problem
% identifier -> CHAR            : unwrap_bin('$1').    Oracle reserved
% identifier -> CHECK           : unwrap_bin('$1').    Oracle reserved
identifier -> CLOB            : unwrap_bin('$1').
identifier -> CLOSE           : unwrap_bin('$1').
% identifier -> CLUSTER         : unwrap_bin('$1').    Oracle reserved
identifier -> COMMIT          : unwrap_bin('$1').
% identifier -> CONNECT         : unwrap_bin('$1').    Oracle reserved
identifier -> COMPARISON      : unwrap_bin('$1').
% identifier -> CONSTRAINT      : unwrap_bin('$1').    syntax problem
identifier -> CONSTRAINTS     : unwrap_bin('$1').
identifier -> CONTENTS        : unwrap_bin('$1').
identifier -> CONTEXT         : unwrap_bin('$1').
identifier -> CONTINUE        : unwrap_bin('$1').
% identifier -> CREATE          : unwrap_bin('$1').    Oracle reserved
identifier -> CROSS           : unwrap_bin('$1').
identifier -> CURRENT         : unwrap_bin('$1').
identifier -> CURSOR          : unwrap_bin('$1').
identifier -> DATABASE        : unwrap_bin('$1').
identifier -> DATAFILES       : unwrap_bin('$1').
% identifier -> DATE            : unwrap_bin('$1').    Oracle reserved
identifier -> DBLINK          : unwrap_bin('$1').
% identifier -> DEFAULT         : unwrap_bin('$1').    Oracle reserved
identifier -> DEFERRED        : unwrap_bin('$1').
identifier -> DELEGATE        : unwrap_bin('$1').
% identifier -> DELETE          : unwrap_bin('$1').    Oracle reserved
% identifier -> DESC            : unwrap_bin('$1').    Oracle reserved
identifier -> DIRECTORY       : unwrap_bin('$1').
% identifier -> DISTINCT        : unwrap_bin('$1').    Oracle reserved
% identifier -> DROP            : unwrap_bin('$1').    Oracle reserved
% identifier -> ELSE            : unwrap_bin('$1').    Oracle reserved
identifier -> END             : unwrap_bin('$1').
identifier -> ENTERPRISE      : unwrap_bin('$1').
identifier -> ESCAPE          : unwrap_bin('$1').
identifier -> EXCEPT          : unwrap_bin('$1').
identifier -> EXECUTE         : unwrap_bin('$1').
% identifier -> EXISTS          : unwrap_bin('$1').    Oracle reserved
identifier -> EXTERNALLY      : unwrap_bin('$1').
identifier -> FETCH           : unwrap_bin('$1').
identifier -> FILTER_WITH     : unwrap_bin('$1').
% identifier -> FLOAT           : unwrap_bin('$1').    Oracle reserved
identifier -> FORCE           : unwrap_bin('$1').
identifier -> FOREIGN         : unwrap_bin('$1').
identifier -> FOUND           : unwrap_bin('$1').
% identifier -> FROM            : unwrap_bin('$1').    Oracle reserved
identifier -> FULL            : unwrap_bin('$1').
identifier -> FUNCTION        : unwrap_bin('$1').
% identifier -> FUNS            : unwrap_bin('$1').    reduce/reduce problem
identifier -> GLOBALLY        : unwrap_bin('$1').
identifier -> GOTO            : unwrap_bin('$1').
% identifier -> GRANT           : unwrap_bin('$1').    Oracle reserved
% identifier -> GROUP           : unwrap_bin('$1').    Oracle reserved
identifier -> HASHMAP         : unwrap_bin('$1').
% identifier -> HAVING          : unwrap_bin('$1').    Oracle reserved
identifier -> HIERARCHY       : unwrap_bin('$1').
% identifier -> HINT            : unwrap_bin('$1').    reduce/reduce problem
% identifier -> IDENTIFIED      : unwrap_bin('$1').    Oracle reserved
identifier -> IF              : unwrap_bin('$1').
identifier -> IMMEDIATE       : unwrap_bin('$1').
% identifier -> IN              : unwrap_bin('$1').    Oracle reserved
identifier -> INCLUDING       : unwrap_bin('$1').
% identifier -> INDEX           : unwrap_bin('$1').    Oracle reserved
identifier -> INDICATOR       : unwrap_bin('$1').
identifier -> INNER           : unwrap_bin('$1').
% identifier -> INSERT          : unwrap_bin('$1').    Oracle reserved
% identifier -> INTERSECT       : unwrap_bin('$1').    Oracle reserved
% identifier -> INTNUM          : unwrap_bin('$1').    reduce/reduce problem
% identifier -> INTO            : unwrap_bin('$1').    Oracle reserved
identifier -> INVALIDATION    : unwrap_bin('$1').
% identifier -> IS              : unwrap_bin('$1').    Oracle reserved
identifier -> JOIN            : unwrap_bin('$1').
identifier -> JSON            : unwrap_bin('$1').
identifier -> KEEP            : unwrap_bin('$1').
identifier -> KEY             : unwrap_bin('$1').
identifier -> KEYLIST         : unwrap_bin('$1').
identifier -> LEFT            : unwrap_bin('$1').
% identifier -> LIKE            : unwrap_bin('$1').    Oracle reserved
identifier -> LINK            : unwrap_bin('$1').
identifier -> LOCAL           : unwrap_bin('$1').
identifier -> LOG             : unwrap_bin('$1').
% identifier -> LONG            : unwrap_bin('$1').    Oracle reserved
identifier -> MATERIALIZED    : unwrap_bin('$1').
% identifier -> MINUS           : unwrap_bin('$1').    Oracle reserved
identifier -> NATURAL         : unwrap_bin('$1').
identifier -> NCHAR           : unwrap_bin('$1').
identifier -> NCLOB           : unwrap_bin('$1').
identifier -> NO              : unwrap_bin('$1').
% identifier -> NOCYCLE         : unwrap_bin('$1').    reduce/reduce problem
identifier -> NONE            : unwrap_bin('$1').
identifier -> NORM_WITH       : unwrap_bin('$1').
% identifier -> NOT             : unwrap_bin('$1').    Oracle reserved
% identifier -> NULLX           : unwrap_bin('$1').    reduce/reduce problem
% identifier -> NUMBER          : unwrap_bin('$1').    Oracle reserved
identifier -> NVARCHAR2       : unwrap_bin('$1').
% identifier -> OF              : unwrap_bin('$1').    Oracle reserved
% identifier -> ON              : unwrap_bin('$1').    Oracle reserved
identifier -> ONLINE          : unwrap_bin('$1').
identifier -> OPEN            : unwrap_bin('$1').
% identifier -> OPTION          : unwrap_bin('$1').    Oracle reserved
% identifier -> OR              : unwrap_bin('$1').    Oracle reserved
% identifier -> ORDER           : unwrap_bin('$1').    Oracle reserved
identifier -> ORDERED_SET     : unwrap_bin('$1').
identifier -> OUTER           : unwrap_bin('$1').
identifier -> PACKAGE         : unwrap_bin('$1').
% identifier -> PARAMETER       : unwrap_bin('$1').    reduce/reduce problem
identifier -> PARTITION       : unwrap_bin('$1').
identifier -> PRESERVE        : unwrap_bin('$1').
identifier -> PRIMARY         : unwrap_bin('$1').
% identifier -> PRIO            : unwrap_bin('$1').    Oracle reserved
identifier -> PRIVILEGES      : unwrap_bin('$1').
identifier -> PROCEDURE       : unwrap_bin('$1').
identifier -> PROFILE         : unwrap_bin('$1').
% identifier -> PUBLIC          : unwrap_bin('$1').    Oracle reserved
identifier -> PURGE           : unwrap_bin('$1').
% identifier -> RAW             : unwrap_bin('$1').    Oracle reserved
identifier -> ROWID           : unwrap_bin('$1').
identifier -> QUOTA           : unwrap_bin('$1').
identifier -> REFERENCES      : unwrap_bin('$1').
identifier -> REQUIRED        : unwrap_bin('$1').
identifier -> RETURN          : unwrap_bin('$1').
identifier -> RETURNING       : unwrap_bin('$1').
identifier -> REUSE           : unwrap_bin('$1').
% identifier -> REVOKE          : unwrap_bin('$1').    Oracle reserved
identifier -> RIGHT           : unwrap_bin('$1').
identifier -> ROLE            : unwrap_bin('$1').
identifier -> ROLES           : unwrap_bin('$1').
identifier -> ROLLBACK        : unwrap_bin('$1').
identifier -> SCHEMA          : unwrap_bin('$1').
% identifier -> SELECT          : unwrap_bin('$1').    Oracle reserved
identifier -> SEQUENCE        : unwrap_bin('$1').
% identifier -> SET             : unwrap_bin('$1').    Oracle reserved
% identifier -> SOME            : unwrap_bin('$1').    reduce/reduce problem
identifier -> SQLERROR        : unwrap_bin('$1').
% identifier -> START           : unwrap_bin('$1').    Oracle reserved
identifier -> STORAGE         : unwrap_bin('$1').
% identifier -> STRING          : unwrap_bin('$1').    reduce/reduce problem
% identifier -> SYNONYM         : unwrap_bin('$1').    Oracle reserved
% identifier -> TABLE           : unwrap_bin('$1').    Oracle reserved
% identifier -> TABLES          : unwrap_bin('$1').    reduce/reduce problem
% identifier -> TABLESPACE      : unwrap_bin('$1').    reduce/reduce problem
% identifier -> TEMPORARY       : unwrap_bin('$1').    reduce/reduce problem
% identifier -> THEN            : unwrap_bin('$1').    Oracle reserved
% identifier -> THROUGH         : unwrap_bin('$1').    reduce/reduce problem
identifier -> TIMESTAMP       : unwrap_bin('$1').
% identifier -> TO              : unwrap_bin('$1').    Oracle reserved
% identifier -> TRIGGER         : unwrap_bin('$1').    Oracle reserved
% identifier -> TRUNCATE        : unwrap_bin('$1').    reduce/reduce problem
identifier -> TYPE            : unwrap_bin('$1').
% identifier -> UNION           : unwrap_bin('$1').    Oracle reserved
% identifier -> UNIQUE          : unwrap_bin('$1').    Oracle reserved
identifier -> UNLIMITED       : unwrap_bin('$1').
% identifier -> UPDATE          : unwrap_bin('$1').    Oracle reserved
identifier -> UROWID          : unwrap_bin('$1').
identifier -> USING           : unwrap_bin('$1').
identifier -> VALIDATE        : unwrap_bin('$1').
% identifier -> VALUES          : unwrap_bin('$1').    Oracle reserved
% identifier -> VARCHAR2        : unwrap_bin('$1').    Oracle reserved
% identifier -> VIEW            : unwrap_bin('$1').    Oracle reserved
% identifier -> WHEN            : unwrap_bin('$1').    syntax problem
identifier -> WHENEVER        : unwrap_bin('$1').
% identifier -> WHERE           : unwrap_bin('$1').    Oracle reserved
% identifier -> WITH            : unwrap_bin('$1').    Oracle reserved
identifier -> WORK            : unwrap_bin('$1').
identifier -> XMLTYPE         : unwrap_bin('$1').

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% embedded condition things
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

when_action -> GOTO identifier : {goto, binary_to_list('$2')}.
when_action -> CONTINUE        : 'continue'.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Erlang code.

%% -----------------------------------------------------------------------------
%%
%% sqlparse.erl: SQL - parser.
%%
%% Copyright (c) 2012-18 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

% parser and compiler interface
-export([
    is_reserved/1,
    parsetree/1,
    parsetree_with_tokens/1
]).

-define(NODEBUG, true).

-include("sqlparse.hrl").

%%-----------------------------------------------------------------------------
%%                          parser helper functions
%%-----------------------------------------------------------------------------

jpparse(X) ->
    {ok, Pt} = jpparse:parsetree(X),
    Pt.

unwrap({X, _}) when is_atom(X) -> atom_to_list(X);
unwrap({_, _, X}) -> X;
unwrap(X) -> X.

unwrap_bin({X, _}) when is_atom(X) -> atom_to_binary(X, unicode);
unwrap_bin({_, _, X}) when is_list(X) -> list_to_binary([X]);
unwrap_bin({_, _, X}) when is_atom(X) -> atom_to_binary(X, unicode).

unwrap_const({'STRING', _, X}) when is_list(X) ->
    const(list_to_binary(string:trim(X, both, "'")));
unwrap_const({'INTNUM', _, X}) when is_list(X) ->
    const(list_to_integer(X));
unwrap_const({'APPROXNUM', _, X}) when is_list(X) ->
    const(list_to_float(X)).

const(V) -> {const, V}.

strl2atom(Strs) ->
    list_to_atom(lists:flatten(
        string:join([string:to_lower(unwrap(S)) || S <- Strs], " "))).

make_list(L) when is_list(L) -> L;
make_list(L) -> [L].

%%-----------------------------------------------------------------------------
%%                                  PARSER
%%-----------------------------------------------------------------------------
-spec parsetree(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, [tuple()]}.
parsetree(Sql) ->
    ?D("Start~n Sql: ~p~n", [Sql]),
    case parsetree_with_tokens(Sql) of
        {ok, {ParseTree, _Tokens}} ->
            ?D("~n ParseTree: ~p~n Tokens: ~p~n", [ParseTree, _Tokens]),
            {ok, ParseTree};
        Error -> Error
    end.

-spec parsetree_with_tokens(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, {[tuple()], list()}}.
parsetree_with_tokens([]) -> {parse_error, invalid_string};
parsetree_with_tokens(<<>>) -> {parse_error, invalid_string};
parsetree_with_tokens(Sql0) ->
    Sql = re:replace(Sql0, "(^[ \r\n]+)|([ \r\n]+$)", "",
        [global, {return, list}]),
    ?D("Start~n Sql: ~p~n", [Sql]),
    SqlClean =
        unicode:characters_to_list(string:replace(Sql, "\n/", "", all)),
    [C | _] = lists:reverse(SqlClean),
    NSql = if C =:= $; -> SqlClean; true -> string:trim(SqlClean) ++ ";" end,
    case sql_lex:string(NSql) of
        {ok, Toks, _} ->
            case parse(Toks) of
                {ok, PTree} ->
                    ?D("~n ParseTree: ~p~n Tokens: ~p~n", [PTree, Toks]),
                    {ok, {PTree, Toks}};
                {error, {N, ?MODULE, ErrorTerms}} ->
                    {parse_error, {lists:flatten(
                        [integer_to_list(N), ": ", ErrorTerms]), Toks}};
                {error, Error} -> {parse_error, {Error, Toks}}
            end;
        {error, Error, _} -> {lex_error, Error}
    end.

-spec is_reserved(binary() | atom() | list()) -> true | false.
is_reserved(Word) when is_binary(Word) ->
    is_reserved(erlang:binary_to_list(Word));
is_reserved(Word) when is_atom(Word) ->
    is_reserved(erlang:atom_to_list(Word));
is_reserved(Word) when is_list(Word) ->
    lists:member(erlang:list_to_atom(string:to_upper(Word)),
        sql_lex:reserved_keywords()).
