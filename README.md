# Euneus

An incredibly flexible and performant JSON parser, generator and formatter in pure Erlang.

Euneus is built on the top of the new [OTP json module](https://erlang.org/documentation/doc-15.0-rc3/lib/stdlib-6.0/doc/html/json.html).

## Disclaimer ⚠️

Work in progress.

## Installation

```erlang
% rebar.config
{deps, [
    {json_polyfill, "0.1.1"}, % Required only for OTP < 27
    {euneus, {git, "https://github.com/williamthome/euneus.git", {branch, "dev/v2"}}}
]}.
```

## Basic usage

```erlang
1> euneus:encode(#{age => 68, name => <<"Joe Armstrong">>, nationality => <<"British">>}).
<<"{\"name\":\"Joe Armstrong\",\"age\":68,\"nationality\":\"British\"}">>
2> euneus:decode(v(1)).
#{<<"age">> => 68,<<"name">> => <<"Joe Armstrong">>,<<"nationality">> => <<"British">>}
```

## Encode

The functions `euneus:encode/1,2` encode an Erlang term into a binary JSON.
The second argument of `euneus:encode/2` are options, and this is the spec:
```erlang
-type options() :: #{
    nulls => [term()],
    drop_nulls => boolean(),
    escape => default | fun((binary()) -> iodata()),
    integer => default | encode(integer()),
    float => default | encode(float()),
    atom => default | encode(atom()),
    list => default | encode(list()),
    proplist => boolean() | {true, is_proplist()},
    map => default | encode(map()),
    sort_keys => boolean(),
    tuple => default
           | encode(tuple())
           | [ datetime
             | timestamp
             | ipv4
             | ipv6
             | {record, #{Name :: atom() => {Fields :: [atom()], Size :: pos_integer()}}
                      | [{Name :: atom(), Fields :: [atom()]}]}
             | fun((tuple()) -> next | {halt, term()})],
    pid => default | encode(pid()),
    port => default | encode(port()),
    reference => default | encode(reference())
}.
```

### Encode example

```erlang
1> Term = #{
..   id => 1,
..   date => {{1970,1,1},{0,0,0}},
..   ip => {0,0,0,0}
.. }.
#{id => 1,date => {{1970,1,1},{0,0,0}},ip => {0,0,0,0}}

2> Opts = #{tuple => [datetime, ipv4]}.
#{tuple => [datetime,ipv4]}

3> euneus:encode(Term, Opts).
<<"{\"id\":1,\"date\":\"1970-01-01T00:00:00Z\",\"ip\":\"0.0.0.0\"}">>
```

## Decode

The functions `euneus:decode/1,2` decode a binary JSON into an Erlang term.
The second argument of `euneus:decode/2` are options, and this is the spec:
```erlang
-type options() :: #{
    codecs => [ copy
              | timestamp
              | datetime
              | ipv4
              | ipv6
              | pid
              | port
              | reference
              | fun((binary()) -> next | {halt, term()})],
    array_start => json:array_start_fun(),
    array_push => json:array_push_fun(),
    array_finish => json:array_finish_fun(),
    object_start => json:object_start_fun(),
    object_keys => binary
                 | copy
                 | atom
                 | existing_atom
                 | json:from_binary_fun(),
    object_push => json:object_push_fun(),
    object_finish => json:object_finish_fun(),
    float => json:from_binary_fun(),
    integer => json:from_binary_fun(),
    null => term()
}.
```

### Decode example

```erlang
1> JSON = ~"""
.. {
..    "id": 1,
..    "date": "1970-01-01T00:00:00Z",
..    "ip": "0.0.0.0"
.. }
.. """.
<<"{\n   \"id\": 1,\n   \"date\": \"1970-01-01T00:00:00Z\",\n   \"ip\": \"0.0.0.0\"\n}">>

2> Opts = #{
..   codecs => [datetime, ipv4],
..   object_keys => atom
.. }.
#{codecs => [datetime,ipv4],object_keys => atom}

3> euneus:decode(JSON, Opts).
#{id => 1,date => {{1970,1,1},{0,0,0}},ip => {0,0,0,0}}

```

