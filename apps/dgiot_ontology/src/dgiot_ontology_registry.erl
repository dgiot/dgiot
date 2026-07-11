%%% DGIOT 本体注册表 — 实体→进程的全局通讯录
%%% 扩展 dgiot_device 的通道注册，增加语义关系和类层次
-module(dgiot_ontology_registry).
-behaviour(gen_server).
-export([start_link/0, register/3, lookup/1, lookup_by_class/1,
         connect/3, disconnect/2, all/0, count/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-include("dgiot_ontology.hrl").

%% ===== 客户端 API =====
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(Class, Id, Meta) when is_atom(Class), is_binary(Id) ->
    gen_server:call(?MODULE, {register, Class, Id, Meta}).

lookup(Id) -> gen_server:call(?MODULE, {lookup, Id}).

lookup_by_class(Class) -> gen_server:call(?MODULE, {lookup_class, Class}).

%% 建立语义关系: SourceId --[Relation]--> TargetId
connect(SourceId, Relation, TargetId) ->
    gen_server:call(?MODULE, {connect, SourceId, Relation, TargetId}).

disconnect(SourceId, TargetId) ->
    gen_server:call(?MODULE, {disconnect, SourceId, TargetId}).

all() -> gen_server:call(?MODULE, all).
count() -> gen_server:call(?MODULE, count).

%% ===== 服务端 =====
init([]) ->
    {ok, #{
        by_id      => #{},  % #{Id => #{class, pid, meta, relations}}
        by_class   => #{},  % #{Class => #{Id => Pid}}
        relations  => #{}   % #{FromId => #{Relation => [ToId]}}
    }}.

handle_call({register, Class, Id, Meta}, _From, #{by_id := ById, by_class := ByClass} = State) ->
    Entry = #{class => Class, meta => Meta, relations => #{}, registered_at => erlang:system_time()},
    NewById = ById#{Id => Entry},
    ClassMap = maps:get(Class, ByClass, #{}),
    NewByClass = ByClass#{Class => ClassMap#{Id => true}},
    io:format("[ONTOLOGY] + ~s :: ~s", [Class, Id]),
    {reply, {ok, Id}, State#{by_id := NewById, by_class := NewByClass}};

handle_call({lookup, Id}, _From, #{by_id := ById} = State) ->
    case maps:find(Id, ById) of
        {ok, Entry} -> {reply, {ok, Entry}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({lookup_class, Class}, _From, #{by_class := ByClass} = State) ->
    Result = maps:get(Class, ByClass, #{}),
    {reply, {ok, Result}, State};

handle_call({connect, FromId, Relation, ToId}, _From,
            #{by_id := ById, relations := Rels} = State) ->
    case {maps:find(FromId, ById), maps:find(ToId, ById)} of
        {{ok, FromEntry}, {ok, ToEntry}} ->
            %% 更新实体关系表
            NewFromEntry = FromEntry#{
                relations => maps:update_with(Relation,
                    fun(L) -> [ToId | L] end, [ToId],
                    maps:get(relations, FromEntry, #{}))},
            NewById = ById#{FromId := NewFromEntry},
            %% 更新全局关系索引
            FromRels = maps:get(FromId, Rels, #{}),
            TargetList = maps:get(Relation, FromRels, []),
            NewRels = Rels#{FromId => FromRels#{Relation => [ToId | TargetList]}},
            io:format("[ONTOLOGY] ~s --[~s]--> ~s", [FromId, Relation, ToId]),
            {reply, {ok, connected}, State#{by_id := NewById, relations := NewRels}};
        _ -> {reply, {error, not_registered}, State}
    end;

handle_call({disconnect, FromId, ToId}, _From, State) ->
    {reply, {ok, disconnected}, State};

handle_call(all, _From, #{by_id := ById} = State) ->
    {reply, {ok, ById}, State};

handle_call(count, _From, #{by_id := ById} = State) ->
    {reply, maps:size(ById), State}.

handle_cast(_Msg, State) -> {noreply, State}.
