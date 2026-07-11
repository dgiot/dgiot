%%% DGIOT 本体论 — 头文件
%%%
%%% 核心概念:
%%%   物模型 (Thing Model): 定义设备有什么属性 → dgiot_device 负责
%%%   本体模型 (Ontology Model): 定义实体是谁/和谁有关系/触发什么规则 → dgiot_ontology 负责
%%%
%%% 对应关系:
%%%   OWL Class     → Erlang Record (类型定义)
%%%   OWL DataProperty → Record Field (数据属性)
%%%   OWL ObjectProperty → Message Type (关系 = 消息路由标签)
%%%   SWRL Rule     → pattern match clause (规则 = 状态迁移条件)

%% ─── 本体实体基础记录 ───
-record(ontology_entity, {
    id          :: binary(),       % 唯一标识
    class       :: atom(),         % OWL Class (equipment/process/material/quality...)
    sub_class   :: atom(),         % OWL SubClass
    properties  :: map(),          % DataProperties
    relations   :: #{atom() => [binary()]},  % ObjectProperties: #{monitors => [TargetId]}
    rules       :: [map()],        % SWRL Rules
    state       :: atom(),         % 当前状态
    pid         :: pid()           % 关联进程 (如果有)
}).

%% ─── 模型定义 (类似物模型但包含关系和规则) ───
-record(ontology_model, {
    model_id    :: binary(),       % 模型ID (如 <<"CigaretteMaker-v1">>)
    class       :: atom(),         % 所属 OWL 类
    sub_class   :: atom(),         % 子类
    properties  :: [map()],        % 属性列表
    relations   :: [map()],        % 关系列表
    rules       :: [map()],        % 规则列表
    events      :: [map()]         % 事件列表
}).

%% ─── 关系类型 (OWL ObjectProperty 语义标签) ───
-define(REL_MONITORS,       monitors).        % Quality → Equipment/Process
-define(REL_MONITORED_BY,   monitored_by).    % Equipment/Process → Quality
-define(REL_EXECUTES,       executes).        % Equipment → Process
-define(REL_PRODUCES,       produces).        % Process → Product
-define(REL_CONSUMED_BY,    consumed_by).     % Material → Process
-define(REL_INSPECTS,       inspects).        % Quality → Product
-define(REL_TRIGGERS,       triggers).        % Defect → Action
-define(REL_CHARACTERIZES,  characterizes).   % Quality → Material
-define(REL_SUPPORTS,       supports).        % Knowledge → Quality

%% ─── 严重度 ───
-define(SEV_L1, l1).  % 严重 — 立即自动
-define(SEV_L2, l2).  % 重要 — 人机协同
-define(SEV_L3, l3).  % 一般 — 人工主导
