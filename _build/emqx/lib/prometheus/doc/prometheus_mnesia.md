

# Module prometheus_mnesia #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Mnesia instrumentation helpers.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#table_disk_size-1">table_disk_size/1</a></td><td>Equivalent to <a href="#table_disk_size-2"><tt>table_disk_size(mnesia:system_info(directory), Table)</tt></a>.</td></tr><tr><td valign="top"><a href="#table_disk_size-2">table_disk_size/2</a></td><td>
Returns sum of all mnesia files for the given <code>Table</code> in bytes.</td></tr><tr><td valign="top"><a href="#tm_info-0">tm_info/0</a></td><td>
Returns {PCount, CCount} tuple, where
PCount is a number of participant transactions and
CCount is a number of coordinator transactions.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="table_disk_size-1"></a>

### table_disk_size/1 ###

<pre><code>
table_disk_size(Table) -&gt; Size
</code></pre>

<ul class="definitions"><li><code>Table = <a href="file.md#type-name_all">file:name_all()</a></code></li><li><code>Size = non_neg_integer()</code></li></ul>

Equivalent to [`table_disk_size(mnesia:system_info(directory), Table)`](#table_disk_size-2).

<a name="table_disk_size-2"></a>

### table_disk_size/2 ###

<pre><code>
table_disk_size(Dir, Table) -&gt; Size
</code></pre>

<ul class="definitions"><li><code>Dir = <a href="file.md#type-name_all">file:name_all()</a></code></li><li><code>Table = <a href="file.md#type-name_all">file:name_all()</a></code></li><li><code>Size = non_neg_integer()</code></li></ul>

Returns sum of all mnesia files for the given `Table` in bytes.
Mnesia can create different files for each table:
- .DAT - DETS files
- .TMP - temp files
- .DMP - dumped ets tables
- .DCD - disc copies data
- .DCL - disc copies log
- .LOGTMP - disc copies log

More on Mnesia files can be found in
[
Mnesia System Information chapter
](http://erlang.org/doc/apps/mnesia/Mnesia_chap7.md) of Mnesia User's Guide

<a name="tm_info-0"></a>

### tm_info/0 ###

<pre><code>
tm_info() -&gt; {Ps, Cs} | {undefined, undefined}
</code></pre>

<ul class="definitions"><li><code>Ps = non_neg_integer()</code></li><li><code>Cs = non_neg_integer()</code></li></ul>

Returns {PCount, CCount} tuple, where
PCount is a number of participant transactions and
CCount is a number of coordinator transactions.
Can return {undefined, undefined} occasionally.

