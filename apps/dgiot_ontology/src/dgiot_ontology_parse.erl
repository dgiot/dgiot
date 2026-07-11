%%% dgiot_ontology_parse — Parse Server 本体关系与约束映射
%%%
%%% 本体三层 → Parse 三层:
%%%   定身份 (Class)     → Parse Class (表)
%%%   定关系 (Relation)  → Parse Relation/Pointer (列类型)
%%%   定约束 (SWRL Rule) → Parse Object + JSON (规则的属性)
%%%
%%% Parse Relation 表述本体关系的优势:
%%%   - 多对多: Equipment ←[Relation:monitors]→ Quality
%%%   - 方向性: 从A到B的关系有明确的语义标签
%%%   - 可查询: Parse query.include() 可级联查询关系链

-module(dgiot_ontology_parse).
-export([create_ontology_schemas/0, create_entity/2,
         add_relation/3, add_rule/3, query_related/2,
         validate_constraint/2]).

%% ═══ 创建本体 Schema (物模型+关系+约束) ═══
create_ontology_schemas() ->
    %% 本体类 — 对应 OWL Class (定身份)
    dgiot_parse:create_schemas(#{<<"className">> => <<"OntologyClass">>,
        <<"fields">> => #{
            <<"class_name">> => #{<<"type">> => <<"String">>},
            <<"subclass_of">> => #{<<"type">> => <<"String">>},
            <<"properties">> => #{<<"type">> => <<"Object">>},
            <<"description">> => #{<<"type">> => <<"String">>}
        }}),

    %% 实体实例 — 对应 OWL Individual (设备/工艺/质量的具体实例)
    dgiot_parse:create_schemas(#{<<"className">> => <<"OntologyEntity">>,
        <<"fields">> => #{
            <<"entity_id">> => #{<<"type">> => <<"String">>},
            <<"onto_class">> => #{<<"type">> => <<"String">>},
            <<"name">> => #{<<"type">> => <<"String">>},
            <<"properties">> => #{<<"type">> => <<"Object">>},
            <<"state">> => #{<<"type">> => <<"String">>},
            %% Parse Pointer: 指向关联实体
            <<"parent">> => #{<<"type">> => <<"Pointer">>, <<"targetClass">> => <<"OntologyEntity">>}
        }}),

    %% 关系定义 — 对应 OWL ObjectProperty (定关系)
    %% 核心: 用 Parse Relation 表述多对多语义关系
    dgiot_parse:create_schemas(#{<<"className">> => <<"OntologyRelation">>,
        <<"fields">> => #{
            <<"relation_name">> => #{<<"type">> => <<"String">>},
            <<"domain_class">> => #{<<"type">> => <<"String">>},
            <<"range_class">> => #{<<"type">> => <<"String">>},
            <<"direction">> => #{<<"type">> => <<"String">>},
            <<"cardinality">> => #{<<"type">> => <<"String">>},
            <<"semantic_label">> => #{<<"type">> => <<"String">>}
        }}),

    %% 约束规则 — 对应 SWRL Rule (定动作)
    dgiot_parse:create_schemas(#{<<"className">> => <<"OntologyConstraint">>,
        <<"fields">> => #{
            <<"rule_id">> => #{<<"type">> => <<"String">>},
            <<"rule_name">> => #{<<"type">> => <<"String">>},
            <<"severity">> => #{<<"type">> => <<"String">>},
            <<"target_class">> => #{<<"type">> => <<"String">>},
            <<"condition_property">> => #{<<"type">> => <<"String">>},
            <<"condition_op">> => #{<<"type">> => <<"String">>},
            <<"condition_value">> => #{<<"type">> => <<"Number">>},
            <<"action_state">> => #{<<"type">> => <<"String">>},
            <<"action_name">> => #{<<"type">> => <<"String">>},
            <<"action_params">> => #{<<"type">> => <<"Object">>}
        }}),
    ok.

%% ═══ CRUD ═══

%% 创建实体实例
create_entity(Class, #{<<"entity_id">> := Id} = Props) ->
    dgiot_parse:create_object(<<"OntologyEntity">>, Props#{
        <<"onto_class">> => atom_to_binary(Class),
        <<"state">> => <<"active">>
    }).

%% 添加关系: Source --[Relation]--> Target
add_relation(SourceId, Relation, TargetId) ->
    %% 使用 Parse Relation — 双向记录
    dgiot_parse:create_object(<<"OntologyRelation">>, #{
        <<"relation_name">> => atom_to_binary(Relation),
        <<"source_id">> => SourceId,
        <<"target_id">> => TargetId
    }).

%% 添加约束规则
add_rule(TargetClass, RuleId, #{
    <<"severity">> := Sev, <<"when">> := When, <<"then">> := Then}) ->
    dgiot_parse:create_object(<<"OntologyConstraint">>, #{
        <<"rule_id">> => RuleId,
        <<"severity">> => Sev,
        <<"target_class">> => atom_to_binary(TargetClass),
        <<"condition_property">> => maps:get(<<"property">>, When),
        <<"condition_op">> => maps:get(<<"op">>, When),
        <<"condition_value">> => maps:get(<<"value">>, When),
        <<"action_state">> => maps:get(<<"state">>, Then),
        <<"action_name">> => maps:get(<<"action">>, Then)
    }).

%% 查询相关实体 (沿关系链追溯)
query_related(EntityId, Relation) ->
    dgiot_parse:query_object(<<"OntologyRelation">>, #{
        <<"where">> => #{
            <<"source_id">> => EntityId,
            <<"relation_name">> => atom_to_binary(Relation)
        }
    }).

%% 验证约束 (检查实体属性是否违反规则)
validate_constraint(#{<<"entity_id">> := Id, <<"properties">> := Props}, Rules) ->
    [R || R <- Rules, check_rule(R, Props)].

check_rule(#{<<"condition_property">> := Prop,
             <<"condition_op">> := Op,
             <<"condition_value">> := Val}, Props) ->
    case maps:find(Prop, Props) of
        {ok, V} when is_number(V) -> compare(Op, V, Val);
        _ -> false
    end.

compare(<<"<">>, A, B)  -> A < B;
compare(<<">">>, A, B)  -> A > B;
compare(<<"==">>, A, B) -> A == B;
compare(<<"<=">>, A, B) -> A =< B;
compare(<<">=">>, A, B) -> A >= B;
compare(_, _, _) -> false.
