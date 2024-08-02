# Euneus

An incredibly flexible and performant JSON parser, generator and formatter in pure Erlang.

Euneus is built on the top of the new [OTP json module](https://erlang.org/documentation/doc-15.0-rc3/lib/stdlib-6.0/doc/html/json.html).

## ⚠️ Disclaimer

This is a work-in-progress branch aiming to release a v2.0 of Euneus.

For v1.0, please look at the [v1.2.2](https://github.com/williamthome/euneus/tree/v1.2.2) tag.

## Installation

```erlang
% rebar.config
{deps, [
    {json_polyfill, "0.1.1"}, % Required only for OTP < 27
    {euneus, {git, "https://github.com/williamthome/euneus.git", {branch, "main"}}}
]}.
```

## Basic usage

```erlang
1> euneus:encode(#{age => 68, name => <<"Joe Armstrong">>, nationality => <<"British">>}).
<<"{\"name\":\"Joe Armstrong\",\"age\":68,\"nationality\":\"British\"}">>
2> euneus:decode(v(1)).
#{<<"age">> => 68,<<"name">> => <<"Joe Armstrong">>,<<"nationality">> => <<"British">>}
```

Please see the [documentation](https://hexdocs.pm/euneus/readme.html) for more examples.

## Encode

The functions `euneus:encode/1,2` encode an Erlang term into a binary JSON.
The second argument of `euneus:encode/2` are options.

### Encode options

```erlang
-type options() :: #{
    codecs => [codec()],
    nulls => [term()],
    skip_values => [term()],
    key_to_binary => fun((term()) -> binary()),
    sort_keys => boolean(),
    proplists => boolean() | {true, is_proplist()},
    escape => fun((binary()) -> iodata()),
    encode_integer => encode(integer()),
    encode_float => encode(float()),
    encode_atom => encode(atom()),
    encode_list => encode(list()),
    encode_map => encode(map()),
    encode_tuple => encode(tuple()),
    encode_pid => encode(pid()),
    encode_port => encode(port()),
    encode_reference => encode(reference())
}.

-type codec() ::
    datetime
    | timestamp
    | ipv4
    | ipv6
    % {records, #{foo => {record_info(fields, foo), record_info(size, foo)}}}
    | {records, #{Name :: atom() := {Fields :: [atom()], Size :: pos_integer()}}}
    | codec_callback().

-type codec_callback() :: fun((tuple()) -> next | {halt, term()}).

-type is_proplist() :: fun((list()) -> boolean()).

-type encode(Type) :: fun((Type, json:encoder(), state()) -> iodata()).
```

#### `codecs`

Transforms tuples into any other Erlang term that will be encoded again.
By returning `next`, the next codec will be called, or by returning
`{halt, Term :: term()}`, the Term will be encoded again.

You can use the built-in codecs or your own.
Please see the `t:euneus_encoder:codec/0` type for details.

Example:

```erlang
1> Term = #{
..   id => 1,
..   date => {{1970,1,1},{0,0,0}},
..   ip => {0,0,0,0}
.. }.
#{id => 1,date => {{1970,1,1},{0,0,0}},ip => {0,0,0,0}}
2> euneus:encode(Term, #{codecs => [datetime, ipv4]}).
<<"{\"id\":1,\"date\":\"1970-01-01T00:00:00Z\",\"ip\":\"0.0.0.0\"}">>
```

> [!IMPORTANT]
> The codecs list is traversed for every tuple.

## Decode

The functions `euneus:decode/1,2` decode a binary JSON into an Erlang term.
The second argument of `euneus:decode/2` are options.

### Options

```erlang
-type options() :: #{
    codecs => [codec()],
    null => term(),
    binary_to_float => json:from_binary_fun(),
    binary_to_integer => json:from_binary_fun(),
    array_start => json:array_start_fun(),
    array_push => json:array_push_fun(),
    array_finish =>
        ordered
        | reversed
        | json:array_finish_fun(),
    object_start => json:object_start_fun(),
    object_keys =>
        copy
        | atom
        | existing_atom
        | json:from_binary_fun(),
    object_push => json:object_push_fun(),
    object_finish =>
        map
        | proplist
        | reversed_proplist
        | json:object_finish_fun()
}.

-type codec() ::
    copy
    | timestamp
    | datetime
    | ipv4
    | ipv6
    | pid
    | port
    | reference
    | codec_callback().

-type codec_callback() :: fun((binary()) -> next | {halt, term()}).
```

#### `codecs`

Transforms binary values into any other Erlang term.
By returning `next`, the next codec will be called, or by returning
`{halt, Term :: term()}`, the Term is returned as the value.

You can use the built-in codecs or your own.
Please see the `t:euneus_decoder:codec/0` type for details.

Example:

```erlang
1> JSON = ~"""
.. {
..    "id": 1,
..    "date": "1970-01-01T00:00:00Z",
..    "ip": "0.0.0.0"
.. }
.. """.
<<"{\n   \"id\": 1,\n   \"date\": \"1970-01-01T00:00:00Z\",\n   \"ip\": \"0.0.0.0\"\n}">>
2> euneus:decode(JSON, #{codecs => [datetime, ipv4]}).
#{<<"id">> => 1,<<"date">> => {{1970,1,1},{0,0,0}},<<"ip">> => {0,0,0,0}}
```

## Benchmark

The benchmarks are implemented very simply, but they are a good start foroptimizing
Euneus since no optimizations have been made. You will find the benchmark commands
in `euneus_benchmarker`, and data and tests under the test folder.

> [!IMPORTANT]
> For the first benchmark run, bootstrapping `erlperf` is required:
>
> ```console
> $ rebar3 as benchmark shell
>
> 1> euneus_benchmarker:bootstrap().
> ===> Verifying dependencies...
> ===> Analyzing applications...
> ===> Compiling erlperf
> ===> Building escript for erlperf...
> ok
> ```

### Setup

- OS : Linux
- CPU: 12th Gen Intel(R) Core(TM) i9-12900HX
- VM : Erlang/OTP 27 [erts-15.0.1] [source] [64-bit] [smp:24:24] [ds:24:24:10] [async-threads:1] [jit:ns]

### Results

```console
$ rebar3 as benchmark shell

1> euneus_benchmarker:encode_benchmark().
Code      ||   Samples       Avg   StdDev    Median      P99  Iteration    Rel
jiffy      1         3        37    2.70%        37       38   27036 us   100%
euneus     1         3        24    6.28%        24       26   41110 us    66%
thoas      1         3        14    4.22%        14       14   73195 us    37%

2> euneus_benchmarker:decode_benchmark().
Code       ||   Samples       Avg   StdDev    Median      P99  Iteration    Rel
euneus      1         3        24    2.44%        24       24   42268 us   100%
jiffy       1         3        19    3.09%        19       19   53589 us    79%
thoas       1         3        14    0.00%        14       14   71452 us    59%
```

> [!NOTE]
> Since `erlperf` currently does not accept labels, `Code` returns something like:
> - euneus_benchmarker:encode_benchmark/0:
>   - #Fun<euneus_benchmarker.0.129271664> = euneus
>   - #Fun<euneus_benchmarker.1.129271664> = jiffy
>   - #Fun<euneus_benchmarker.2.129271664> = thoas
> - euneus_benchmarker:decode_benchmark/0:
>   - #Fun<euneus_benchmarker.3.129271664> = euneus
>   - #Fun<euneus_benchmarker.4.129271664> = jiffy
>   - #Fun<euneus_benchmarker.5.129271664> = thoas

## Sponsors

If you like this tool, please consider [sponsoring me](https://github.com/sponsors/williamthome).
I'm thankful for your never-ending support :heart:

I also accept coffees :coffee:

[!["Buy Me A Coffee"](https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png)](https://www.buymeacoffee.com/williamthome)

## License

Copyright (c) 2024 [William Fank Thomé](https://github.com/williamthome)

Euneus is 100% open-source and community-driven. All components are
available under the Apache 2 License on [GitHub](https://github.com/williamthome/euneus).

See [LICENSE.md](LICENSE.md) for more information.
