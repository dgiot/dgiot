# JPParse 

[![Build Status](https://travis-ci.org/K2InformaticsGmbH/jpparse.svg?branch=rebar3)](https://travis-ci.org/K2InformaticsGmbH/jpparse) [![Coverage Status](https://coveralls.io/repos/github/K2InformaticsGmbH/jpparse/badge.svg?branch=master)](https://coveralls.io/github/K2InformaticsGmbH/jpparse?branch=master)

## Introduction:

Json Path Parser parses a Json path into an abstract syntax tree, ready to be interpreted for matching them to json data objects.

### Still missing:

## Glossary:
### For the path expression:

<dl>
<dt>Property</dt>
    <dd>key/attribute of a json object</dd>
<dt>Value</dt>
   <dd>Whatever data is associated with a property in a json object</dd>
<dt>':'</dt>
    <dd>Columns are used to access properties of objects (if an object has multiple identical properties, only the last one will match).</dd>
<dt>'[ ]'</dt>
    <dd>Square brackets signify you expect the data to be a list, and you wish to traverse it.<br>
    They can contain:
<ul>       
 <li>no value: meaning you wish to traverse the whole list.</li>
      <li>  a number: meaning you wish to only access the data on that index of the list</li>
       <li> a range (ex. 1-45): meaning you wish to traverse a sublist</li>
        <li>multiple numbers (ex 1,2,5): meaning you wish to access the data only on those index numbers</li></ul></dd>

<dt>'{ }'</dt>
    <dd>Curly braces signify you expect the data to be an object, and you wish to somehow traverse it<br>
    They can contain:
       <ul><li> no value: meaning you wish to traverse every value of the whole object</li>
        <li>a property: meaning you wish to access the value of that property (same as using a column). Only the last corresponding property will match.</li>
       <li> multiple properties: meaning you wish to access the values of those properties</li></ul></dd>

<dt>'$keys$'</dt>
   <dd> Signifies you wish to work on (or return) the object keys as if they were a list (containing the object's keys). Is understood as being a property.</dd>
   
<dt>'$values$'</dt>
   <dd> Signifies you wish to work on (or return) the object values as if they were a list (containing the object's values). Is understood as being a property.</dd>
   
<dt>'$firstChild$'</dt>
    <dd>Signifies you wish to work on the first value of the object, whatever the key. Is understood as being a property.</dd>
</dl>

#### "single value / list of values as result" operator correspondence table
From the moment a path expression contains an operator who returns a list, result will be a list, even if only a single object matches.

input | output
--- | ---
`a:b` when `b` is list | List 
`a:b` when `b` is object | Single (object) 
`a:b` when `b` is neither list or object | Single (value) 
`a[]` | List 
`a[1]` | List 
`a[1,2]` | List 
`b{}` | List 
`b{prop}` | List 
`b{$keys$}` | Single (object)

### For the parsed form expression:
<dl>
<dt>'_'</dt>
<dd>    When used as second parameter in '{ }' or '[ ]' functions, signifies 'match all'
    When used as first parameter, signifies 'work on anonymous root data object' (used when the path expression starts with brackets or curly braces, or to access the root object).</dd>

<dt>'{}'</dt>
    <dd>Binary function. First parameter is the object to work on, second the attribute(s).
    It's the default operator: `{':', [a]}` is equal in meaning to `{':', [{'{}', '_', a}]}`, and should internally be treated as such.</dd>

<dt>'[]'</dt>
    <dd>Binary function: First parameter is the list to work on, second the index(es).</dd>

<dt>'-'</dt>
    <dd>Binary function: First parameter is the position to start from, second the position to stop at.</dd>

<dt>':'</dt>
    <dd>Unary function: Parameter list contains the path elements to traverse.</dd>

<dt>'$anything$</dt>
    <dd>Nullary function: translates to whatever the inclosed name may mean (currently supported: `firstChild`, `keys`, `values`)</dd>
       

## jpparse grammar/syntax examples:

path expression | parsed form
--- | ---
`<<"a{}[]">>` | `{'[]',{'{}',<<"a">>,[][]}`
`<<"a[1,2,3]">>` | `{'[]',<<"a">>,[1,2,3]}`
`<<"a{x,y,z}">>` | `{'{}',<<"a">>,[<<"x">>,<<"y">>,<<"z">>]}`
`<<"a:b[]:c">>` | `{':',<<"c">>,{'[]',{':',<<"b">>,<<"a">>[]}}`
`<<"a:b[1,2,3]">>` | `{'[]',{':',<<"b">>,<<"a">>[1,2,3]}`
`<<"a:b{x,y,z}">>` | `{'{}',{':',<<"b">>,<<"a">>[<<"x">>,<<"y">>,<<"z">>]}`
`<<"a[]{}:b{}[]">>` | `{'[]',{'{}',{':',<<"b">>,{'{}',{'[]',<<"a">>,[][]}[][]}`
`<<"a{$tok$}">>` | `{'{}',<<"a">>,[{'$',<<"tok">>}]}`
`<<"a:b{$tok$}[1,2]">>` | `{'[]',{'{}',{':',<<"b">>,<<"a">>[{'$',<<"tok">>}][1,2]}`
`<<"a{f(x)}[f(y)]">>` | `{'[]',{'{}',<<"a">>,[{'fun',<<"f">>,[<<"x">>]}][{'fun',<<"f">>,[<<"y">>]}]}`
`<<"a:b[f()]:c">>` | `{':',<<"c">>,{'[]',{':',<<"b">>,<<"a">>},[{'fun',<<"f">>,[]}]}}`
`<<"a:b{f()}:c">>` | `{':',<<"c">>,{'{}',{':',<<"b">>,<<"a">>},[{'fun',<<"f">>,[]}]}}`
`<<"a:f(y(p,q),x):c">>` | `{':',<<"c">>,{':',{'fun',<<"f">>,[{'fun',<<"y">>,[<<"p">>,<<"q">>]<<"x">>]},<<"a">>}}`
`<<"a:b[]{f(x:y)}:c">>` | `{':',<<"c">>,{'{}',{'[]',{':',<<"b">>,<<"a">>},[]},[{'fun',<<"f">>,[{':',<<"y">>,<<"x">>}]}]}}`

[more examples](https://github.com/k2informatics/jpparse/blob/master/test/test.txt)

### Example Usage

```erlang
1> jpparse:parsetree_with_tokens("a:b").
{ok,{{':',<<"b">>,<<"a">>},
     [{'STRING',1,"a"},{':',1},{'STRING',1,"b"}]}}
2> jpparse:parsetree("a:b{1-2"). 
{parse_error,{1,"syntax error before: '-'",
              [{'STRING',1,"a"},
               {':',1},
               {'STRING',1,"b"},
               {'{',1},
               {'STRING',1,"1"},
               {'-',1},
               {'STRING',1,"2"}]}}
3> jpparse:parsetree("a:b[1-2").
{parse_error,{1,"syntax error before: '-'",
              [{'STRING',1,"a"},
               {':',1},
               {'STRING',1,"b"},
               {'[',1},
               {'STRING',1,"1"},
               {'-',1},
               {'STRING',1,"2"}]}}
4> jpparse:parsetree_with_tokens("a:b[1,2]").
{ok,{{'[]',{':',<<"b">>,<<"a">>},[1,2]},
     [{'STRING',1,"a"},
      {':',1},
      {'STRING',1,"b"},
      {'[',1},
      {'STRING',1,"1"},
      {',',1},
      {'STRING',1,"2"},
      {']',1}]}}
5> jpparse:parsetree_with_tokens("a:b{x,y}").
{ok,{{'{}',{':',<<"b">>,<<"a">>},[<<"x">>,<<"y">>]},
     [{'STRING',1,"a"},
      {':',1},
      {'STRING',1,"b"},
      {'{',1},
      {'STRING',1,"x"},
      {',',1},
      {'STRING',1,"y"},
      {'}',1}]}}
```
