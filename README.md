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

### Decode

The functions `euneus:decode/1,2` decode a binary JSON. The second argument
of `euneus:decode/2` are options, and, currently, this is the spec:
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

#### Decode example

```erlang
1> JSON = ~"""
.. {
..    "id": 1,
..    "date": "1970-01-01T00:00:00Z",
..    "ip": "0.0.0.0"
.. }
.. """.
<<"{\n   \"id\": 1,\n   \"date\": \"1970-01-01T00:00:00Z\",\n   \"ip\": \"0.0.0.0\"\n}">>
2> euneus:decode(JSON, #{
..   codecs => [datetime, ipv4],
..   object_keys => atom
.. }).
#{id => 1,date => {{1970,1,1},{0,0,0}},ip => {0,0,0,0}}

```

