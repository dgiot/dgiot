# rfc3339

parse and format rfc3339 strings in elixir and erlang

## Installing as a rebar depedancy (erlang)

You install it as a rebar dependency by adding the following in the deps section of rebar.config

```erlang
    {rfc3339,  {git, "git://github.com/talentdeficit/rfc3339.git", {branch, master}}}
```
## building

```bash
rebar3 compile
```

## running

```bash
rebar3 shell
```


## erlang usage

###### parse_to_local_datetime

```erlang
> rfc3339:parse_to_local_datetime(<<"1996-12-19T16:39:57-08:00">>).
{{1996,12,19},{10,39,57}}
```

###### parse
```erlang
> rfc3339:parse(<<"1937-01-01T12:00:27.87+00:20">>).
{ok,{{1937,1,1},{12,0,27},870000,20}}
```

###### format
```erlang
> rfc3339:format({date(), time()}).
{ok,<<"2016-07-02T14:04:39Z">>}
```



## todo

-[ ] everything
