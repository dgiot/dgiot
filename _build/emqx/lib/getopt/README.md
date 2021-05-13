Getopt for Erlang
=================

Command-line parsing module that uses a syntax similar to that of GNU *getopt*.


Requirements
------------

You should only need a somewhat recent version of Erlang/OTP. The module has
been tested with all versions of Erlang starting with R13B and ending with 20.

You also need a recent version of [rebar3](http://www.rebar3.org/) in
the system path.

Installation
------------

To compile the module you simply run `rebar3 compile`.

To run the unit tests run `rebar3 eunit`.

To build the (very) limited documentation run `rebar edoc`.

To use getopt in your project you can just add it as a dependency in your
`rebar.config` file in the following way:
```sh
{deps,
 [
  {getopt, "1.0.1"}
 ]
}
```


Usage
-----

The `getopt` module provides four functions:

```erlang
parse([{Name, Short, Long, ArgSpec, Help}], Args :: string() | [string()]) ->
    {ok, {Options, NonOptionArgs}} | {error, {Reason, Data}}

tokenize(CmdLine :: string()) -> [string()]

usage([{Name, Short, Long, ArgSpec, Help}], ProgramName :: string()) -> ok

usage([{Name, Short, Long, ArgSpec, Help}], ProgramName :: string(),
      CmdLineTail :: string()) -> ok

usage([{Name, Short, Long, ArgSpec, Help}], ProgramName :: string(),
      CmdLineTail :: string(), OptionsTail :: [{string(), string}]) -> ok
```

The `parse/2` function receives a list of tuples with the command line option
specifications. The type specification for the tuple is:

```erlang
-type arg_type() :: 'atom' | 'binary' | 'boolean' | 'float' | 'integer' | 'string'.

-type arg_value() :: atom() | binary() | boolean() | float() | integer() | string().

-type arg_spec() :: arg_type() | {arg_type(), arg_value()} | undefined.

-type option_spec() :: {
                   Name    :: atom(),
                   Short   :: char() | undefined,
                   Long    :: string() | undefined,
                   ArgSpec :: arg_spec(),
                   Help    :: string() | undefined
                  }.
```

The elements of the tuple are:

  - `Name`: name of the option.
  - `Short`: character for the short option (e.g. $i for -i).
  - `Long`: string for the long option (e.g. "info" for --info).
  - `ArgSpec`: data type and optional default value the argument will be converted to.
  - `Help`: help message that is shown for the option when `usage/2` is called.

e.g.

```erlang
{port, $p, "port", {integer, 5432}, "Database server port"}
```

The second parameter receives the list of arguments as passed to the `main/1`
function in escripts or the unparsed command line as a string.

If the function is successful parsing the command line arguments it will return
a tuple containing the parsed options and the non-option arguments. The options
will be represented by a list of key-value pairs with the `Name` of the
option as *key* and the argument from the command line as *value*. If the option
doesn't have an argument, only the atom corresponding to its `Name` will be
added to the list of options. For the example given above we could get something
like `{port, 5432}`. The non-option arguments are just a list of strings with
all the arguments that did not have corresponding options.

e.g. Given the following option specifications:

```erlang
OptSpecList =
    [
     {host,    $h,        "host",    {string, "localhost"}, "Database server host"},
     {port,    $p,        "port",    integer,               "Database server port"},
     {dbname,  undefined, "dbname",  {string, "users"},     "Database name"},
     {xml,     $x,        undefined, undefined,             "Output data in XML"},
     {verbose, $v,        "verbose", integer,               "Verbosity level"},
     {file,    undefined, undefined, string,                "Output file"}
    ].
```

And this command line:

```erlang
Args = "-h myhost --port=1000 -x myfile.txt -vvv dummy1 dummy2"
```

Which could also be passed in the format the `main/1` function receives the arguments in escripts:

```erlang
Args = ["-h", "myhost", "--port=1000", "-x", "file.txt", "-vvv", "dummy1", "dummy2"].
```

The call to `getopt:parse/2`:

```erlang
getopt:parse(OptSpecList, Args).
```

Will return:

```erlang
{ok,{[{host,"myhost"},
      {port,1000},
      xml,
      {file,"file.txt"},
      {dbname,"users"},
      {verbose,3}],
     ["dummy1","dummy2"]}}
```

The `tokenize/1` function will separate a command line string into
tokens, taking into account whether an argument is single or double
quoted, a character is escaped or if there are environment variables to
be expanded. e.g.:

```erlang
getopt:tokenize("  --name John\\ Smith --path \"John's Files\" -u ${USER}").
```

Will return something like:

```erlang
["--name","John Smith","--path","John's Files","-u","jsmith"]
```

The other functions exported by the `getopt` module (`usage/2`, `usage/3`
and `usage/4`) are used to show the command line syntax for the program.
For example, given the above-mentioned option specifications, the call to
`getopt:usage/2`:

```erlang
getopt:usage(OptSpecList, "ex1").
```

Will show (on *standard_error*):

    Usage: ex1 [-h <host>] [-p <port>] [--dbname <dbname>] [-x] [-v] <file>

      -h, --host                    Database server host
      -p, --port                    Database server port
      --dbname                      Database name
      -x                            Output data in XML
      -v                            Verbosity level
      <file>                        Output file

This call to `getopt:usage/3` will add a string after the usage command line:

```erlang
getopt:usage(OptSpecList, "ex1", "[var=value ...] [command ...]").
```

Will show (on *standard_error*):

    Usage: ex1 [-h <host>] [-p <port>] [--dbname <dbname>] [-x] [-v <verbose>] <file> [var=value ...] [command ...]

      -h, --host            Database server host
      -p, --port            Database server port
      --dbname              Database name
      -x                    Output data in XML
      -v, --verbose         Verbosity level
      <file>                Output file

Whereas this call to `getopt:usage/3` will also add some lines to the options
help text:

```erlang
getopt:usage(OptSpecList, "ex1", "[var=value ...] [command ...]",
             [{"var=value", "Variables that will affect the execution (e.g. debug=1)"},
              {"command",   "Commands that will be executed (e.g. count)"}]).
```

Will show (on *standard_error*):

    Usage: ex1 [-h <host>] [-p <port>] [--dbname <dbname>] [-x] [-v <verbose>] <file> [var=value ...] [command ...]

      -h, --host            Database server host
      -p, --port            Database server port
      --dbname              Database name
      -x                    Output data in XML
      -v, --verbose         Verbosity level
      <file>                Output file
      var=value             Variables that will affect the execution (e.g. debug=1)
      command               Commands that will be executed (e.g. count)


Command-line Syntax
-------------------

The syntax supported by the `getopt` module is very similar to that followed
by GNU programs, which is described [here](http://www.gnu.org/s/libc/manual/html_node/Argument-Syntax.html).

Options can have both short (single character) and long (string) option names.

A short option can have the following syntax:

    -a         Single option 'a', no argument or implicit boolean argument
    -a foo     Single option 'a', argument "foo"
    -afoo      Single option 'a', argument "foo"
    -abc       Multiple options: 'a'; 'b'; 'c'
    -bcafoo    Multiple options: 'b'; 'c'; 'a' with argument "foo"
    -aaa       Multiple repetitions of option 'a'

A long option can have the following syntax:

    --foo      Single option 'foo', no argument
    --foo=bar  Single option 'foo', argument "bar"
    --foo bar  Single option 'foo', argument "bar"


Argument Types
--------------

The arguments allowed for options are: *atom*; *binary*; *boolean*; *float*; *integer*; *string*.
The `getopt` module checks every argument to see if it can be converted to its
correct type.

In the case of boolean arguments, the following values (in lower or
upper case) are considered `true`: *true*; *t*; *yes*; *y*; *on*; *enabled*; *1*.
These ones are considered `false`: *false*; *f*; *no*; *n*; *off*; *disabled*; *0*.

Numeric arguments can only be negative when passed as part of an assignment expression.

e.g. `--increment=-100` is a valid expression; whereas `--increment -100` is invalid


Implicit Arguments
------------------

The arguments for options with the *boolean* and *integer* data types can sometimes
be omitted. In those cases the value assigned to the option is *true* for *boolean*
arguments and *1* for integer arguments.


Multiple Repetitions
--------------------

An option can be repeated several times, in which case there will be multiple
appearances of the option in the resulting list. The only exceptions are short
options with integer arguments. In that particular case, each appearance of
the short option within a single command line argument will increment the
number that will be returned for that specific option.

e.g. Given an option specification list with the following format:

```erlang
OptSpecList =
    [
     {define,  $D, "define",  string,  "Define a variable"},
     {verbose, $v, "verbose", integer, "Verbosity level"}
    ].
```

The following invocation:

```erlang
getopt:parse(OptSpecList, "-DFOO -DVAR1=VAL1 -DBAR --verbose --verbose=3 -v -vvvv dummy").
```

would return:

```erlang
{ok,{[{define,"FOO"}, {define,"VAR1=VAL1"}, {define,"BAR"},
      {verbose,1}, {verbose,3}, {verbose,1}, {verbose,4}],
     ["dummy"]}}
```


Positional Options
------------------

We can also have options with neither short nor long option names. In this case,
the options will be taken according to their position in the option specification
list passed to `getopt:/parse2`.

For example, with the following option specifications:

```erlang
OptSpecList =
    [
     {xml,         $x,        "xml",     undefined, "Output data as XML"},
     {dbname,      undefined, undefined, string,    "Database name"},
     {output_file, undefined, undefined, string,    "File where the data will be saved to"}
    ].
```

This call to `getopt:parse/2`:

```erlang
getopt:parse(OptSpecList, "-x mydb file.out dummy dummy").
```

Will return:

```erlang
{ok,{[xml,{dbname,"mydb"},{output_file,"file.out"}],
     ["dummy","dummy"]}}
```


Option Terminators
------------------

The string `--` is considered an option terminator. This means that all the
command-line arguments after it are considered non-option arguments and will be
returned without being evaluated even if they follow the *getopt* syntax.

e.g. This invocation using the first option specification list in the document:

```erlang
getopt:parse(OptSpecList, "-h myhost -p 1000 -- --dbname mydb dummy").
```

will return:

```erlang
{ok,{[{host,"myhost"}, {port,1000},{dbname,"users"}],
     ["--dbname","mydb","dummy"]}}
```

Notice that the *dbname* option was assigned the value `users` instead of `mydb`.
This happens because the option terminator prevented *getopt* from evaluating it
and the default value was assigned to it.


Non-option Arguments
--------------------

The single `-` character is always considered as a non-option argument.

e.g. This invocation using the specification list from the previous example:

```erlang
getopt:parse(OptSpecList, "-h myhost -p 1000 - --dbname mydb dummy").
```

will return:

```erlang
{ok,{[{host,"myhost"}, {port,1000}, {dbname,"mydb"}],
     ["-","dummy"]}}
```


Arguments with embedded whitespace
----------------------------------

Arguments that have embedded whitespace have to be quoted with either
single or double quotes to be considered as a single
argument.


e.g. Given an option specification list with the following format:

```erlang
OptSpecList =
    [
     {define,  $D, "define",  string,  "Define a variable"},
     {user,    $u, "user",    string,  "User name"}
    ].
```

The following invocation:

```erlang
getopt:parse(OptSpecList,
             "-D'FOO=VAR 123' --define \"VAR WITH SPACES\" -u\"my user name\"").
```

would return:

```erlang
{ok,{[{define,"FOO=VAR 123"},
      {define,"VAR WITH SPACES"},
      {user,"my user name"}],
     []}}
```

When parsing a command line with unclosed quotes the last argument
will be a single string starting at the position where the last quote
was entered.

e.g. The following invocation:

```erlang
getopt:parse(OptSpecList, "--user ' my user ' \"argument with unclosed quotes").
```

would return:

```erlang
{ok,{[{user," my user "}],
     ["argument with unclosed quotes"]}}
```


Environment variable expansion
------------------------------

`getopt:parse/2` will expand environment variables when used with a command
line that is passed as a single string. The formats that are supported
for environment variable expansion are:

  - $VAR (simple Unix/bash format)
  - ${VAR} (full Unix/bash format)
  - %VAR% (Windows format)

If a variable is not present in the environment it will not be
expanded. Variables can be expanded within double-quoted and free
arguments. *getopt* will not expand environment variables within
single-quoted arguments.

e.g. Given the following option specification list:

```erlang
OptSpecList =
    [
     {path,    $p, "path",    string,  "File path"}
    ].
```

The following invocation:

```erlang
getopt:parse(OptSpecList, "--path ${PATH} $NONEXISTENT_DUMMY_VAR").
```

would return (depending on the value of your PATH variable) something like:

```erlang
{ok,{[{path, "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"}],
     ["$NONEXISTENT_DUMMY_VAR"]}}
```

Currently, *getopt* does not perform wildcard expansion of file paths.


Escaping arguments
==================

Any character can be escaped by prepending the \ (backslash) character
to it.

e.g.

```erlang
getopt:parse(OptSpecList, "--path /john\\'s\\ files dummy").
```

Will return:

```erlang
{ok,{[{path,"/john's files"}],["dummy"]}}
```
