recon
=====

Recon wants to be a set of tools usable in production to diagnose Erlang problems or inspect production environment safely.

To build the library:

    rebar3 compile

Documentation for the library can be obtained at http://ferd.github.io/recon/

It is recommended that you use tags if you do not want bleeding edge and development content for this library.

Current Status
--------------

[![Build Status](https://travis-ci.org/ferd/recon.png)](https://travis-ci.org/ferd/recon)

Versions supported: OTP-17 and up. Support of R16B03-1 down to R15B02 is best effort. Builds with Rebar3 require OTP-17.1 and up because that's what the tool supports.

Changelog
---------

Branches are organized by version. `master` contains the bleeding edge, `2.x`
contains all stable changes up to the latest release of v2, and `1.x` contains
all stable changes of the first version of Recon.

*2.x*

- 2.5.0
  - Optional formatting of records in traces (thanks to @bartekgorny)
  - Basic support for OTP-22 in `recon_alloc` (base handling of `foreign_blocks` type)
- 2.4.0
  - Optional formatting of records in traces (thanks to @bartekgorny)
- 2.3.6
  - Adapting for OTP-21. Includes the 'deprecation' of `recon:files/0`
    since OTP-21 no longer supports listing all file descriptors, and
    removing `error_logger_queue_len` from node stats since a new
    logging mechanism was introduced in-process instead.
- 2.3.5
  - fixing timefold's first iteration to prevent errors at call-site
    by sleeping before sampling
- 2.3.4
  - fixing edoc tag that broke some downstream packaging attempts
- 2.3.3
  - fixing `bin_leak` arith errors
  - fixes to `recon_alloc:allocators/1` (incl. R16 compatibility)
  - fix errors in scheduler wall time calculations
  - `term_to_pid` supports binaries
- 2.3.2
  - Allow the `return_to` option in `recon_trace`
  - More efficient sorting function for procs and ports attributes
    (thanks to @zhongwencool and @pichi)
  - Allow the usage of `return_trace` in `recon_trace:calls/2-3` instead
    of `fun(_) -> return_trace() end`.
- 2.3.1
  - Updated `app_deps` script to run with rebar3 dependencies
  - Minor docs update
- 2.3.0
  - Doc made clearer around semantics of `recon:proc_count` and
    `recon:proc_window`.
  - Fix doc typos
  - Fix potential race condition on waiting for death of tracing process
  - Add an option which allows sending tracing output somewhere other than
    group_leader() (thanks @djnym)
  - Add ability to pass custom formatter function when tracing (thanks @iilyak)
- 2.2.1
  - Fixing type specs for `recon:port_types/0` and `recon_lib:count/1`,
    thanks to @lucafavatella
  - Minor documentation fixes.
- 2.2.0:
  - Adding scheduler info metrics to get a more accurate picture than what
    top and CPU gives.
  - Broadening `recon_trace:calls/2` interface to allow multiple match specs,
    which was currently only allowed for `calls/3`.
  - Support for `mbcs_pool` data in `erts_alloc`, and some internal refactoring,
    thanks to Lukas Larsson.
- 2.1.2:
  - Fixing tests for R15B02 and up
  - Fixing a backwards compatibility for R15B03 on `recon_alloc` operations
    with dumps on disk
- 2.1.1:
  - Renaming `recon_trace:mfa()` type to `recon_trace:tspec()` to avoid
    issues in older Erlang versions regarding redefining an existing type
    (Thanks Roberto Aloi)
- 2.1.0:
  - Adding `recon_trace` script to allow safe tracing of function calls
    on production nodes.
  - Adding `queue_fun.awk` script to inspect running functions of processes
    with large mailboxes in a crash dump.
- 2.0.2:
  - Preventing crashes in `recon_alloc` when certain expected allocators
    do not return results (Thanks to Michal Ptaszek)
- 2.0.1:
  - Add support for R16B03 in `recon_alloc`.
- 2.0.0:
  - Test suite added
  - Major rewrite of `recon_alloc`, thanks to Lukas Larsson. Things that changed include:
    - `average_sizes/0` is renamed `average_block_sizes/1` and now takes
      the keywords `current` and `max`.
    - Documentation updates.
    - `memory/1` has new options in `allocated_types` and `allocated_instances`.
    - `memory/2` has been added, which allows to choose between `current` and
      `max` values. `memory(Term)` is made equivalent to `memory(Term, current)`.
    - Allow `sbcs_to_mbcs/0` to take the arguments `current` and `max`.
    - Added unit conversion function `set_unit/1`, which allows to get the
      `recon_alloc` results in bytes (default), kilobytes, megabytes, and
      gigabytes, to help with readability.
  - Updated the internal rebar version, if anybody were to use it.
  - `recon:port_info/1` no longer includes the `parallelism` option by default
    within the `meta` category as this would hurt backwards compatibility on
    some installations.
  - `recon:get_state/2` is added in order to specify timeouts.
    `recon:get_state/1` keeps its 5000 milliseconds timeout.
  - Addition of a fake attribute called `binary_memory`, which is callable in
    `recon:info/2,4`, `recon:proc_count/2`, and `recon:proc_window/3`. This
    attribute allows to fetch the amount of memory used by refc binaries for
    a process, and to sort by that value for counts and windows.


*1.x*

- 1.2.0:
  - add `recon_alloc:snapshot*` functions, which allow memory allocation
    snapshots to be taken, saved on disk, reloaded, and analyzed on-demand.
    Thanks to Lukas Larsson for this functionality.
  - remove `parallelism` data from `port_info` for better OTP backwards
    compatibility with little loss of information.
- 1.1.0:
  - add `recon_lib:term_to_port` to convert a string back to a
    usable port.
  - add `recon:port_info/1` and `recon:port_info/2`
  - add `recon_alloc` module
- 1.0.0: add `info/2` and `info/4`. The `memory` info type thus gets renamed
  to `memory_used`, in order to avoid conflicts when picking between a type
  and a specific process attribute. Types exported by the module also get
  updated.
- 0.4.2: extended `app_deps.erl` to read apps/ directories for releases
- 0.4.1: fixed bug where nodes with lots of processes could see the GC call
  fail if said processes failed between long calls within the `bin_leak`
  function call.
- 0.4.0: fixed bug where nodes with lots of processes or ports could see their
  count or window functions fail because a process or socket closed between the
  time the function started and before it finished. This ends up changing the
  API in `recon_lib` for the window and count functions that take a specific
  pid as an argument.
- 0.3.1: factored out some logic from `recon:info/1` into `recon_lib:term_to_pid`
  and allowed arbitrary terms to be used for pids in `recon:get_state/1`.
