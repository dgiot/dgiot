%%% @author Fred Hebert <mononcqc@ferd.ca>
%%%  [http://ferd.ca/]
%%% @author Lukas Larsson <lukas@erlang.org>
%%% @doc Functions to deal with
%%% <a href="http://www.erlang.org/doc/man/erts_alloc.html">Erlang's memory
%%% allocators</a>, or particularly, to try to present the allocator data
%%% in a way that makes it simpler to discover possible problems.
%%%
%%% Tweaking Erlang memory allocators and their behaviour is a very tricky
%%% ordeal whenever you have to give up the default settings. This module
%%% (and its documentation) will try and provide helpful pointers to help
%%% in this task.
%%%
%%% This module should mostly be helpful to figure out <em>if</em> there is
%%% a problem, but will offer little help to figure out <em>what</em> is wrong.
%%%
%%% To figure this out, you need to dig deeper into the allocator data
%%% (obtainable with {@link allocators/0}), and/or have some precise knowledge
%%% about the type of load and work done by the VM to be able to assess what
%%% each reaction to individual tweak should be.
%%%
%%% A lot of trial and error might be required to figure out if tweaks have
%%% helped or not, ultimately.
%%%
%%% In order to help do offline debugging of memory allocator problems
%%% recon_alloc also has a few functions that store snapshots of the
%%% memory statistics.
%%% These snapshots can be used to freeze the current allocation values so that
%%% they do not change during analysis while using the regular functionality of
%%% this module, so that the allocator values can be saved, or that
%%% they can be shared, dumped, and reloaded for further analysis using files.
%%% See {@link snapshot_load/1} for a simple use-case.
%%%
%%% Glossary:
%%% <dl>
%%%   <dt>sys_alloc</dt>
%%%   <dd>System allocator, usually just malloc</dd>
%%%
%%%   <dt>mseg_alloc</dt>
%%%   <dd>Used by other allocators, can do mmap. Caches allocations</dd>
%%%
%%%   <dt>temp_alloc</dt>
%%%   <dd>Used for temporary allocations</dd>
%%%
%%%   <dt>eheap_alloc</dt>
%%%   <dd>Heap data (i.e. process heaps) allocator</dd>
%%%
%%%   <dt>binary_alloc</dt>
%%%   <dd>Global binary heap allocator</dd>
%%%
%%%   <dt>ets_alloc</dt>
%%%   <dd>ETS data allocator</dd>
%%%
%%%   <dt>driver_alloc</dt>
%%%   <dd>Driver data allocator</dd>
%%%
%%%   <dt>sl_alloc</dt>
%%%   <dd>Short-lived memory blocks allocator</dd>
%%%
%%%   <dt>ll_alloc</dt>
%%%   <dd>Long-lived data (i.e. Erlang code itself) allocator</dd>
%%%
%%%   <dt>fix_alloc</dt>
%%%   <dd>Frequently used fixed-size data allocator</dd>
%%%
%%%  <dt>std_alloc</dt>
%%%  <dd>Allocator for other memory blocks</dd>
%%%
%%%  <dt>carrier</dt>
%%%  <dd>When a given area of memory is allocated by the OS to the
%%%    VM (through sys_alloc or mseg_alloc), it is put into a 'carrier'. There
%%%    are two kinds of carriers: multiblock and single block. The default
%%%    carriers data is sent to are multiblock carriers, owned by a specific
%%%    allocator (ets_alloc, binary_alloc, etc.). The specific allocator can
%%%    thus do allocation for specific Erlang requirements within bits of
%%%    memory that has been preallocated before. This allows more reuse,
%%%    and we can even measure the cache hit rates {@link cache_hit_rates/0}.
%%%
%%%    There is however a threshold above which an item in memory won't fit
%%%    a multiblock carrier. When that happens, the specific allocator does
%%%    a special allocation to a single block carrier. This is done by the
%%%    allocator basically asking for space directly from sys_alloc or
%%%    mseg_alloc rather than a previously multiblock area already obtained
%%%    before.
%%%
%%%    This leads to various allocation strategies where you decide to
%%%    choose:
%%%    <ol>
%%%      <li>which multiblock carrier you're going to (if at all)</li>
%%%      <li>which block in that carrier you're going to</li>
%%%    </ol>
%%%
%%%    See <a href="http://www.erlang.org/doc/man/erts_alloc.html">the official
%%%    documentation on erts_alloc</a> for more details.
%%%  </dd>
%%%
%%%  <dt>mbcs</dt>
%%%  <dd>Multiblock carriers.</dd>
%%%
%%%  <dt>sbcs</dt>
%%%  <dd>Single block carriers.</dd>
%%%
%%%  <dt>lmbcs</dt>
%%%  <dd>Largest multiblock carrier size</dd>
%%%
%%%  <dt>smbcs</dt>
%%%  <dd>Smallest multiblock carrier size</dd>
%%%
%%%  <dt>sbct</dt>
%%%  <dd>Single block carrier threshold</dd>
%%% </dl>
%%%
%%% By default all sizes returned by this module are in bytes. You can change
%%% this by calling {@link set_unit/1}.
%%%
-module(recon_alloc).
-define(UTIL_ALLOCATORS, [temp_alloc,
                          eheap_alloc,
                          binary_alloc,
                          ets_alloc,
                          driver_alloc,
                          sl_alloc,
                          ll_alloc,
                          fix_alloc,
                          std_alloc
                         ]).

-type allocator() :: temp_alloc | eheap_alloc | binary_alloc | ets_alloc
                   | driver_alloc | sl_alloc | ll_alloc | fix_alloc
                   | std_alloc.
-type instance() :: non_neg_integer().
-type allocdata(T) :: {{allocator(), instance()}, T}.
-type allocdata_types(T) :: {{allocator(), [instance()]}, T}.
-export_type([allocator/0, instance/0, allocdata/1]).

-define(CURRENT_POS, 2). % pos in sizes tuples for current value
-define(MAX_POS, 4). % pos in sizes tuples for max value

-export([memory/1, memory/2, fragmentation/1, cache_hit_rates/0,
         average_block_sizes/1, sbcs_to_mbcs/1, allocators/0,
         allocators/1]).

%% Snapshot handling
-type memory() :: [{atom(),atom()}].
-type snapshot() :: {memory(),[allocdata(term())]}.

-export_type([memory/0, snapshot/0]).

-export([snapshot/0,  snapshot_clear/0,
         snapshot_print/0, snapshot_get/0,
         snapshot_save/1,  snapshot_load/1]).

%% Unit handling
-export([set_unit/1]).

%%%%%%%%%%%%%%
%%% Public %%%
%%%%%%%%%%%%%%


%% @doc Equivalent to `memory(Key, current)'.
-spec memory(used | allocated | unused) -> pos_integer()
      ;     (usage) -> number()
      ;     (allocated_types | allocated_instances) ->
                 [{allocator(), pos_integer()}].
memory(Key) -> memory(Key, current).

%% @doc reports one of multiple possible memory values for the entire
%% node depending on what is to be reported:
%%
%% <ul>
%%   <li>`used' reports the memory that is actively used for allocated
%%       Erlang data;</li>
%%   <li>`allocated' reports the memory that is reserved by the VM. It
%%       includes the memory used, but also the memory yet-to-be-used but still
%%       given by the OS. This is the amount you want if you're dealing with
%%       ulimit and OS-reported values. </li>
%%   <li>`allocated_types' report the memory that is reserved by the
%%       VM grouped into the different util allocators.</li>
%%   <li>`allocated_instances' report the memory that is reserved
%%       by the VM grouped into the different schedulers. Note that
%%       instance id 0 is the global allocator used to allocate data from
%%       non-managed threads, i.e. async and driver threads.</li>
%%   <li>`unused' reports the amount of memory reserved by the VM that
%%       is not being allocated.
%%       Equivalent to `allocated - used'.</li>
%%   <li>`usage' returns a percentage (0.0 .. 1.0) of `used/allocated'
%%       memory ratios.</li>
%% </ul>
%%
%% The memory reported by `allocated' should roughly
%% match what the OS reports. If this amount is different by a large margin,
%% it may be the sign that someone is allocating memory in C directly, outside
%% of Erlang's own allocator -- a big warning sign. There are currently
%% three sources of memory alloction that are not counted towards this value:
%% The cached segments in the mseg allocator, any memory allocated as a
%% super carrier, and small pieces of memory allocated during startup
%% before the memory allocators are initialized.
%%
%% Also note that low memory usages can be the sign of fragmentation in
%% memory, in which case exploring which specific allocator is at fault
%% is recommended (see {@link fragmentation/1})
-spec memory(used | allocated | unused, current | max) -> pos_integer()
    ;       (usage, current | max) -> number()
    ;       (allocated_types|allocated_instances, current | max) ->
                    [{allocator(),pos_integer()}].
memory(used,Keyword) ->
    lists:sum(lists:map(fun({_,Prop}) ->
                                container_size(Prop,Keyword,blocks_size)
                        end,util_alloc()));
memory(allocated,Keyword) ->
    lists:sum(lists:map(fun({_,Prop}) ->
                                container_size(Prop,Keyword,carriers_size)
                        end,util_alloc()));
memory(allocated_types,Keyword) ->
    lists:foldl(fun({{Alloc,_N},Props},Acc) ->
                        CZ = container_size(Props,Keyword,carriers_size),
                        orddict:update_counter(Alloc,CZ,Acc)
                end,orddict:new(),util_alloc());
memory(allocated_instances,Keyword) ->
    lists:foldl(fun({{_Alloc,N},Props},Acc) ->
                        CZ = container_size(Props,Keyword,carriers_size),
                        orddict:update_counter(N,CZ,Acc)
                end,orddict:new(),util_alloc());
memory(unused,Keyword) ->
    memory(allocated,Keyword) - memory(used,Keyword);
memory(usage,Keyword) ->
    memory(used,Keyword) / memory(allocated,Keyword).

%% @doc Compares the block sizes to the carrier sizes, both for
%% single block (`sbcs') and multiblock (`mbcs') carriers.
%%
%% The returned results are sorted by a weight system that is
%% somewhat likely to return the most fragmented allocators first,
%% based on their percentage of use and the total size of the carriers,
%% for both `sbcs' and `mbcs'.
%%
%% The values can both be returned for `current' allocator values, and
%% for `max' allocator values. The current values hold the present allocation
%% numbers, and max values, the values at the peak. Comparing both together
%% can give an idea of whether the node is currently being at its memory peak
%% when possibly leaky, or if it isn't. This information can in turn
%% influence the tuning of allocators to better fit sizes of blocks and/or
%% carriers.
-spec fragmentation(current | max) -> [allocdata([{atom(), term()}])].
fragmentation(Keyword) ->
    WeighedData = [begin
      BlockSbcs = container_value(Props, Keyword, sbcs, blocks_size),
      CarSbcs = container_value(Props, Keyword, sbcs, carriers_size),
      BlockMbcs = container_value(Props, Keyword, mbcs, blocks_size),
      CarMbcs = container_value(Props, Keyword, mbcs, carriers_size),
      {Weight, Vals} = weighed_values({BlockSbcs,CarSbcs},
                                      {BlockMbcs,CarMbcs}),
      {Weight, {Allocator,N}, Vals}
    end || {{Allocator, N}, Props} <- util_alloc()],
    [{Key,Val} || {_W, Key, Val} <- lists:reverse(lists:sort(WeighedData))].

%% @doc looks at the `mseg_alloc' allocator (allocator used by all the
%% allocators in {@link allocator()}) and returns information relative to
%% the cache hit rates. Unless memory has expected spiky behaviour, it should
%% usually be above 0.80 (80%).
%%
%% Cache can be tweaked using three VM flags: `+MMmcs', `+MMrmcbf', and
%% `+MMamcbf'.
%%
%% `+MMmcs' stands for the maximum amount of cached memory segments. Its
%% default value is '10' and can be anything from 0 to 30. Increasing
%% it first and verifying if cache hits get better should be the first
%% step taken.
%%
%% The two other options specify what are the maximal values of a segment
%% to cache, in relative (in percent) and absolute terms (in kilobytes),
%% respectively. Increasing these may allow more segments to be cached, but
%% should also add overheads to memory allocation. An Erlang node that has
%% limited memory and increases these values may make things worse on
%% that point.
%%
%% The values returned by this function are sorted by a weight combining
%% the lower cache hit joined to the largest memory values allocated.
-spec cache_hit_rates() -> [{{instance,instance()}, [{Key,Val}]}] when
    Key :: hit_rate | hits | calls,
    Val :: term().
cache_hit_rates() ->
    WeighedData = [begin
      Mem = proplists:get_value(memkind, Props),
      {_,Hits} = lists:keyfind(cache_hits, 1, proplists:get_value(status,Mem)),
      {_,Giga,Ones} = lists:keyfind(mseg_alloc,1,proplists:get_value(calls,Mem)),
      Calls = 1000000000*Giga + Ones,
      HitRate = usage(Hits,Calls),
      Weight = (1.00 - HitRate)*Calls,
      {Weight, {instance,N}, [{hit_rate,HitRate}, {hits,Hits}, {calls,Calls}]}
    end || {{_, N}, Props} <- alloc([mseg_alloc])],
    [{Key,Val} || {_W,Key,Val} <- lists:reverse(lists:sort(WeighedData))].

%% @doc Checks all allocators in {@link allocator()} and returns the average
%% block sizes being used for `mbcs' and `sbcs'. This value is interesting
%% to use because it will tell us how large most blocks are.
%% This can be related to the VM's largest multiblock carrier size
%% (`lmbcs') and smallest multiblock carrier size (`smbcs') to specify
%% allocation strategies regarding the carrier sizes to be used.
%%
%% This function isn't exceptionally useful unless you know you have some
%% specific problem, say with sbcs/mbcs ratios (see {@link sbcs_to_mbcs/0})
%% or fragmentation for a specific allocator, and want to figure out what
%% values to pick to increase or decrease sizes compared to the currently
%% configured value.
%%
%% Do note that values for `lmbcs' and `smbcs' are going to be rounded up
%% to the next power of two when configuring them.
-spec average_block_sizes(current | max) -> [{allocator(), [{Key,Val}]}] when
    Key :: mbcs | sbcs,
    Val :: number().
average_block_sizes(Keyword) ->
    Dict = lists:foldl(fun({{Instance,_},Props},Dict0) ->
      CarSbcs = container_value(Props, Keyword, sbcs, blocks),
      SizeSbcs = container_value(Props, Keyword, sbcs, blocks_size),
      CarMbcs = container_value(Props, Keyword, mbcs, blocks),
      SizeMbcs = container_value(Props, Keyword, mbcs, blocks_size),
      Dict1 = dict:update_counter({Instance,sbcs,count},CarSbcs,Dict0),
      Dict2 = dict:update_counter({Instance,sbcs,size},SizeSbcs,Dict1),
      Dict3 = dict:update_counter({Instance,mbcs,count},CarMbcs,Dict2),
      Dict4 = dict:update_counter({Instance,mbcs,size},SizeMbcs,Dict3),
      Dict4
    end,
    dict:new(),
    util_alloc()),
    average_group(average_calc(lists:sort(dict:to_list(Dict)))).

%% @doc compares the amount of single block carriers (`sbcs') vs the
%% number of multiblock carriers (`mbcs') for each individual allocator in
%% {@link allocator()}.
%%
%% When a specific piece of data is allocated, it is compared to a threshold,
%% called the 'single block carrier threshold' (`sbct'). When the data is
%% larger than the `sbct', it gets sent to a single block carrier. When the
%% data is smaller than the `sbct', it gets placed into a multiblock carrier.
%%
%% mbcs are to be preferred to sbcs because they basically represent pre-
%% allocated memory, whereas sbcs will map to one call to sys_alloc
%% or mseg_alloc, which is more expensive than redistributing
%% data that was obtained for multiblock carriers. Moreover, the VM is able to
%% do specific work with mbcs that should help reduce fragmentation in ways
%% sys_alloc or mmap usually won't.
%%
%% Ideally, most of the data should fit inside multiblock carriers. If
%% most of the data ends up in `sbcs', you may need to adjust the multiblock
%% carrier sizes, specifically the maximal value (`lmbcs') and the threshold
%% (`sbct'). On 32 bit VMs, `sbct' is limited to 8MBs, but 64 bit VMs can go
%% to pretty much any practical size.
%%
%% Given the value returned is a ratio of sbcs/mbcs, the higher the value,
%% the worst the condition. The list is sorted accordingly.
-spec sbcs_to_mbcs(max | current) -> [allocdata(term())].
sbcs_to_mbcs(Keyword) ->
    WeightedList = [begin
      Sbcs = container_value(Props, Keyword, sbcs, blocks),
      Mbcs = container_value(Props, Keyword, mbcs, blocks),
      Ratio = case {Sbcs, Mbcs} of
        {0,0} -> 0;
        {_,0} -> infinity; % that is bad!
        {_,_} -> Sbcs / Mbcs
      end,
      {Ratio, {Allocator,N}}
     end || {{Allocator, N}, Props} <- util_alloc()],
    [{Alloc,Ratio} || {Ratio,Alloc} <- lists:reverse(lists:sort(WeightedList))].

%% @doc returns a dump of all allocator settings and values
-spec allocators() -> [allocdata(term())].
allocators() ->
    UtilAllocators = erlang:system_info(alloc_util_allocators),
    Allocators = [sys_alloc,mseg_alloc|UtilAllocators],
    %% versions is deleted in order to allow the use of the orddict api,
    %% and never really having come across a case where it was useful to know.
    [{{A,N},lists:sort(proplists:delete(versions,Props))} ||
        A <- Allocators,
        Allocs <- [erlang:system_info({allocator,A})],
        Allocs =/= false,
        {_,N,Props} <- Allocs].

%% @doc returns a dump of all allocator settings and values modified
%%      depending on the argument.
%% <ul>
%%   <li>`types' report the settings and accumulated values for each
%%       allocator type. This is useful when looking for anomalies
%%       in the system as a whole and not specific instances.</li>
%% </ul>
-spec allocators(types) -> [allocdata_types(term())].
allocators(types) ->
    allocators_types(alloc(), []).

allocators_types([{{Type,No},Vs}|T], As) ->
    case lists:keytake(Type, 1, As) of
        false ->
            allocators_types(T,[{Type,[No],sort_values(Type, Vs)}|As]);
        {value,{Type,Nos,OVs},NAs} ->
            MergedValues = merge_values(sort_values(Type, Vs),OVs),
            allocators_types(T,[{Type,[No|Nos],MergedValues}|NAs])
    end;
allocators_types([], As) ->
    [{{Type,Nos},Vs} || {Type, Nos, Vs} <- As].

merge_values([{Key,Vs}|T1], [{Key,OVs}|T2]) when Key =:= memkind ->
    [{Key, merge_values(Vs, OVs)} | merge_values(T1, T2)];
merge_values([{Key,Vs}|T1], [{Key,OVs}|T2]) when Key =:= calls;
                                                 Key =:= fix_types;
                                                 Key =:= sbmbcs;
                                                 Key =:= mbcs;
                                                 Key =:= mbcs_pool;
                                                 Key =:= sbcs;
                                                 Key =:= status ->
    [{Key,lists:map(
           fun({{K,MV1,V1}, {K,MV2,V2}}) ->
                   %% Merge the MegaVs + Vs into one
                   V = MV1 * 1000000 + V1 + MV2 * 1000000 + V2,
                   {K, V div 1000000, V rem 1000000};
              ({{K,V1}, {K,V2}}) when K =:= segments_watermark ->
                   %% We take the maximum watermark as that is
                   %% a value that we can use somewhat. Ideally
                   %% maybe the average should be used, but the
                   %% value is very rarely important so leave it
                   %% like this for now.
                   {K, lists:max([V1,V2])};
              ({{K,V1}, {K,V2}}) when K =:= foreign_blocks ->
                   %% foreign blocks are just merged as a bigger list.
                   {K, V1++V2};
              ({{K,V1}, {K,V2}}) ->
                   {K, V1 + V2};
              ({{K,C1,L1,M1}, {K,C2,L2,M2}}) ->
                   %% Merge the Curr, Last, Max into one
                   {K, C1+C2, L1+L2, M1+M2}
           end, lists:zip(Vs,OVs))} | merge_values(T1,T2)];
merge_values([{Type,_Vs}=E|T1], T2) when Type =:= mbcs_pool ->
    %% For values never showing up in instance 0 but in all other
    [E|merge_values(T1,T2)];
merge_values(T1, [{Type,_Vs}=E|T2]) when Type =:= fix_types ->
    %% For values only showing up in instance 0
    [E|merge_values(T1,T2)];
merge_values([E|T1], [E|T2]) ->
    %% For values that are constant
    [E|merge_values(T1,T2)];
merge_values([{options,_Vs1}|T1], [{options,_Vs2} = E|T2]) ->
    %% Options change a but in between instance 0 and the other,
    %% We show the others as they are the most interesting.
    [E|merge_values(T1,T2)];
merge_values([],[]) ->
    [].

sort_values(mseg_alloc, Vs) ->
    {value, {memkind, MemKindVs}, OVs} = lists:keytake(memkind, 1, Vs),
    lists:sort([{memkind, lists:sort(MemKindVs)} | OVs]);
sort_values(_Type, Vs) ->
    lists:sort(Vs).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Snapshot handling %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Take a new snapshot of the current memory allocator statistics.
%% The snapshot is stored in the process dictionary of the calling process,
%% with all the limitations that it implies (i.e. no garbage-collection).
%% To unsert the snapshot, see {@link snapshot_clear/1}.
-spec snapshot() -> snapshot() | undefined.
snapshot() ->
    put(recon_alloc_snapshot, snapshot_int()).

%% @doc clear the current snapshot in the process dictionary, if present,
%% and return the value it had before being unset.
%% @end
%% Maybe we should use erlang:delete(Key) here instead?
-spec snapshot_clear() -> snapshot() | undefined.
snapshot_clear() ->
    put(recon_alloc_snapshot, undefined).

%% @doc print a dump of the current snapshot stored by {@link snapshot/0}
%% Prints `undefined' if no snapshot has been taken.
-spec snapshot_print() -> ok.
snapshot_print() ->
    io:format("~p.~n",[snapshot_get()]).

%% @doc returns the current snapshot stored by {@link snapshot/0}.
%% Returns `undefined' if no snapshot has been taken.
-spec snapshot_get() -> snapshot() | undefined.
snapshot_get() ->
    get(recon_alloc_snapshot).

%% @doc save the current snapshot taken by {@link snapshot/0} to a file.
%% If there is no current snapshot, a snaphot of the current allocator
%% statistics will be written to the file.
-spec snapshot_save(Filename) -> ok when
      Filename :: file:name().
snapshot_save(Filename) ->
    Snapshot = case snapshot_get() of
                   undefined ->
                       snapshot_int();
                   Snap ->
                       Snap
               end,
    case file:write_file(Filename,io_lib:format("~p.~n",[Snapshot])) of
        ok -> ok;
        {error,Reason} ->
            erlang:error(Reason,[Filename])
    end.


%% @doc load a snapshot from a given file. The format of the data in the
%% file can be either the same as output by {@link snapshot_save()},
%% or the output obtained by calling
%%  `{erlang:memory(),[{A,erlang:system_info({allocator,A})} || A <- erlang:system_info(alloc_util_allocators)++[sys_alloc,mseg_alloc]]}.'
%% and storing it in a file.
%% If the latter option is taken, please remember to add a full stop at the end
%% of the resulting Erlang term, as this function uses `file:consult/1' to load
%% the file.
%%
%% Example usage:
%%
%%```On target machine:
%%     1> recon_alloc:snapshot().
%%     undefined
%%     2> recon_alloc:memory(used).
%%     18411064
%%     3> recon_alloc:snapshot_save("recon_snapshot.terms").
%%     ok
%%
%%   On other machine:
%%     1> recon_alloc:snapshot_load("recon_snapshot.terms").
%%     undefined
%%     2> recon_alloc:memory(used).
%%     18411064'''
%%
-spec snapshot_load(Filename) -> snapshot() | undefined when
      Filename :: file:name().
snapshot_load(Filename) ->
    {ok,[Terms]} = file:consult(Filename),
    Snapshot =
        case Terms of
            %% We handle someone using
            %% {erlang:memory(),
            %%  [{A,erlang:system_info({allocator,A})} ||
            %%     A <- erlang:system_info(alloc_util_allocators)++[sys_alloc,mseg_alloc]]}
            %% to dump data.
            {M,[{Alloc,_D}|_] = Allocs} when is_atom(Alloc) ->
                {M,[{{A,N},lists:sort(proplists:delete(versions,Props))} ||
                       {A,Instances = [_|_]} <- Allocs,
                       {_, N, Props} <- Instances]};
            %% We assume someone used recon_alloc:snapshot() to store this one
            {M,Allocs} ->
                {M,[{AN,lists:sort(proplists:delete(versions,Props))} ||
                       {AN, Props} <- Allocs]}
        end,
    put(recon_alloc_snapshot,Snapshot).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handling of units %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc set the current unit to be used by recon_alloc. This effects all
%% functions that return bytes.
%%
%% Eg.
%% ```1> recon_alloc:memory(used,current).
%%    17548752
%%    2> recon_alloc:set_unit(kilobyte).
%%    undefined
%%    3> recon_alloc:memory(used,current).
%%    17576.90625'''
%%
-spec set_unit(byte | kilobyte | megabyte | gigabyte) -> ok.
set_unit(byte) ->
    put(recon_alloc_unit,undefined);
set_unit(kilobyte) ->
    put(recon_alloc_unit,1024);
set_unit(megabyte) ->
    put(recon_alloc_unit,1024*1024);
set_unit(gigabyte) ->
    put(recon_alloc_unit,1024*1024*1024).

conv({Mem,Allocs} = D) ->
    case get(recon_alloc_unit) of
        undefined ->
            D;
        Factor ->
            {conv_mem(Mem,Factor),conv_alloc(Allocs,Factor)}
    end.

conv_mem(Mem,Factor) ->
    [{T,M / Factor} || {T,M} <- Mem].

conv_alloc([{{sys_alloc,_I},_Props} = Alloc|R], Factor) ->
    [Alloc|conv_alloc(R,Factor)];
conv_alloc([{{mseg_alloc,_I} = AI,Props}|R], Factor) ->
    MemKind = orddict:fetch(memkind,Props),
    Status = orddict:fetch(status,MemKind),
    {segments_size,Curr,Last,Max} = lists:keyfind(segments_size,1,Status),
    NewSegSize = {segments_size,Curr/Factor,Last/Factor,Max/Factor},
    NewStatus = lists:keyreplace(segments_size,1,Status,NewSegSize),
    NewProps = orddict:store(memkind,orddict:store(status,NewStatus,MemKind),
                             Props),
    [{AI,NewProps}|conv_alloc(R,Factor)];
conv_alloc([{AI,Props}|R], Factor) ->
    FactorFun = fun({T,Curr}) when
                          T =:= blocks_size; T =:= carriers_size ->
                        {T,Curr/Factor};
                    ({T,Curr,Last,Max}) when
                          T =:= blocks_size; T =:= carriers_size;
                          T =:= mseg_alloc_carriers_size;
                          T =:= sys_alloc_carriers_size ->
                        {T,Curr/Factor,Last/Factor,Max/Factor};
                   (T) ->
                        T
                end,
    NewMbcsProp = [FactorFun(Prop) || Prop <- orddict:fetch(mbcs,Props)],
    NewSbcsProp = [FactorFun(Prop) || Prop <- orddict:fetch(sbcs,Props)],
    NewProps = orddict:store(sbcs,NewSbcsProp,
                  orddict:store(mbcs,NewMbcsProp,Props)),
    case orddict:find(mbcs_pool,Props) of
        error ->
            [{AI,NewProps}|conv_alloc(R,Factor)];
        {ok,MbcsPoolProps} ->
            NewMbcsPoolProp = [FactorFun(Prop) || Prop <- MbcsPoolProps],
            NewPoolProps = orddict:store(mbcs_pool,NewMbcsPoolProp,NewProps),
            [{AI,NewPoolProps}|conv_alloc(R,Factor)]
    end;
conv_alloc([],_Factor) ->
    [].

%%%%%%%%%%%%%%%
%%% Private %%%
%%%%%%%%%%%%%%%

%% Sort on small usage vs large size.
%% The weight cares about both the sbcs and mbcs values, and also
%% returns a proplist of possibly interesting values.
weighed_values({SbcsBlockSize, SbcsCarrierSize},
               {MbcsBlockSize, MbcsCarrierSize}) ->
    SbcsUsage = usage(SbcsBlockSize, SbcsCarrierSize),
    MbcsUsage = usage(MbcsBlockSize, MbcsCarrierSize),
    SbcsWeight = (1.00 - SbcsUsage)*SbcsCarrierSize,
    MbcsWeight = (1.00 - MbcsUsage)*MbcsCarrierSize,
    Weight = SbcsWeight + MbcsWeight,
    {Weight, [{sbcs_usage, SbcsUsage},
              {mbcs_usage, MbcsUsage},
              {sbcs_block_size, SbcsBlockSize},
              {sbcs_carriers_size, SbcsCarrierSize},
              {mbcs_block_size, MbcsBlockSize},
              {mbcs_carriers_size, MbcsCarrierSize}]}.

%% Returns the `BlockSize/CarrierSize' as a 0.0 -> 1.0 percentage,
%% but also takes 0/0 to be 100% to make working with sorting and
%% weights simpler.
usage(0,0) -> 1.00;
usage(0.0,0.0) -> 1.00;
%usage(N,0) -> ???;
usage(Block,Carrier) -> Block/Carrier.

%% Calculation for the average of blocks being used.
average_calc([]) ->
    [];
average_calc([{{Instance,Type,count},Ct},{{Instance,Type,size},Size}|Rest]) ->
    case {Size,Ct} of
        {_,0} when Size == 0 -> [{Instance, Type, 0} | average_calc(Rest)];
        _ -> [{Instance,Type,Size/Ct} | average_calc(Rest)]
    end.

%% Regrouping/merging values together in proplists
average_group([]) -> [];
average_group([{Instance,Type1,N},{Instance,Type2,M} | Rest]) ->
    [{Instance,[{Type1,N},{Type2,M}]} | average_group(Rest)].

%% Get the total carrier size
container_size(Props, Keyword, Container) ->
    Sbcs = container_value(Props, Keyword, sbcs, Container),
    Mbcs = container_value(Props, Keyword, mbcs, Container),
    Sbcs+Mbcs.

container_value(Props, Keyword, Type, Container)
  when is_atom(Keyword) ->
    container_value(Props, key2pos(Keyword), Type, Container);
container_value(Props, Pos, mbcs = Type, Container)
  when Pos == ?CURRENT_POS,
       ((Container =:= blocks) or (Container =:= blocks_size)
        or (Container =:= carriers) or (Container =:= carriers_size))->
    %% We include the mbcs_pool into the value for mbcs.
    %% The mbcs_pool contains carriers that have been abandoned
    %% by the specific allocator instance and can therefore be
    %% grabbed by another instance of the same type.
    %% The pool was added in R16B02 and enabled by default in 17.0.
    %% See erts/emulator/internal_docs/CarrierMigration.md in
    %% Erlang/OTP repo for more details.
    Pool = case proplists:get_value(mbcs_pool, Props) of
               PoolProps when PoolProps =/= undefined ->
                   element(Pos,lists:keyfind(Container, 1, PoolProps));
               _ -> 0
           end,
    TypeProps = proplists:get_value(Type, Props),
    Pool + element(Pos,lists:keyfind(Container, 1, TypeProps));
container_value(Props, Pos, Type, Container)
  when Type =:= sbcs; Type =:= mbcs ->
    TypeProps = proplists:get_value(Type, Props),
    element(Pos,lists:keyfind(Container, 1, TypeProps)).

%% Create a new snapshot
snapshot_int() ->
    {erlang:memory(),allocators()}.

%% If no snapshot has been taken/loaded then we use current values
snapshot_get_int() ->
    case snapshot_get() of
        undefined ->
            conv(snapshot_int());
        Snapshot ->
            conv(Snapshot)
    end.

%% Get the alloc part of a snapshot
alloc() ->
    {_Mem,Allocs} = snapshot_get_int(),
    Allocs.
alloc(Type) ->
    [{{T,Instance},Props} || {{T,Instance},Props} <- alloc(),
                             lists:member(T,Type)].

%% Get only alloc_util allocs
util_alloc() ->
    alloc(?UTIL_ALLOCATORS).

key2pos(current) ->
    ?CURRENT_POS;
key2pos(max) ->
    ?MAX_POS.
