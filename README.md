# Euneus

[![Github Actions](https://github.com/williamthome/euneus/workflows/CI/badge.svg)](https://github.com/williamthome/euneus/actions)
[![Coverage](https://raw.githubusercontent.com/cicirello/jacoco-badge-generator/main/tests/100.svg)](https://github.com/williamthome/euneus/actions/workflows/ci.yml)
[![Erlang Versions](https://img.shields.io/badge/Erlang%2FOTP-24%2B-green?style=flat-square)](http://www.erlang.org)
[![Latest Release](https://img.shields.io/github/release/williamthome/euneus.svg?style=flat-square)](https://github.com/williamthome/euneus/releases/latest)
[![Hex Version](https://img.shields.io/hexpm/v/euneus.svg)](https://hex.pm/packages/euneus)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/euneus/)
[![Total Download](https://img.shields.io/hexpm/dt/euneus.svg)](https://hex.pm/packages/euneus)
[![License](https://img.shields.io/hexpm/l/euneus.svg)](https://github.com/williamthome/euneus/blob/main/LICENSE)
[![Last Updated](https://img.shields.io/github/last-commit/williamthome/euneus.svg)](https://github.com/williamthome/euneus/commits/main)

An incredibly flexible and performant JSON parser, generator and formatter in pure Erlang.

Euneus is built on the top of the new [OTP json module](https://erlang.org/documentation/doc-15.0-rc3/lib/stdlib-6.0/doc/html/json.html).

Both encoder and decoder fully conform to [RFC 8259](https://datatracker.ietf.org/doc/html/rfc8259)
and [ECMA 404](https://ecma-international.org/publications-and-standards/standards/ecma-404/) standards
and are tested using [JSONTestSuite](https://github.com/nst/JSONTestSuite).

Detailed examples and further explanation can be found at [hexdocs](https://hexdocs.pm/euneus).

## Requirements

OTP >= 24.

## Why should you use Euneus over the OTP json module?

The new OTP `json` module is incredible and blazing fast!

Unfortunately, it is only available for OTP >= 27. Euneus is available from OTP >= 24.

Also, Euneus simplifies a lot of overheads with the new OTP `json` module without
losing any option provided by the json module and keeping its performance.

A simple example comparing the OTP `json` module with Euneus decoding object keys:

```erlang
> json:decode(<<"{\"foo\":\"bar\"}">>, [], #{object_push => fun(K, V, Acc) -> [{binary_to_atom(K), V} | Acc] end}).
{#{foo => <<"bar">>},[],<<>>}
> euneus:decode(<<"{\"foo\":\"bar\"}">>, #{object_keys => atom}).
#{foo => <<"bar">>}
```

### Encode Features

Some reasons to use Euneus for JSON encoding:

- Possibility to skip values
- Encoding proplists (proplists are not encoded by the OTP json module)
- Sort object keys
- Simple custom encoding via codecs:

  ```erlang
  -type codec() ::
      timestamp
      | datetime
      | ipv4
      | ipv6
      | {records, #{Name :: atom() := {Fields :: [atom()], Size :: pos_integer()}}}
      | codec_fun()
      | custom_codec().
  ```

#### Encode Codecs

##### Encode timestamp

```erlang
> euneus:encode({0, 0, 0}, #{codecs => [timestamp]}).
<<"\"1970-01-01T00:00:00.000Z\"">>
```

##### Encode datetime

```erlang
> euneus:encode({{1970, 01, 01}, {00, 00, 00}}, #{codecs => [datetime]}).
<<"\"1970-01-01T00:00:00Z\"">>
```

##### Encode ipv4

```erlang
> euneus:encode({0, 0, 0, 0}, #{codecs => [ipv4]}).
<<"\"0.0.0.0\"">>
```

##### Encode ipv6

```erlang
> euneus:encode({16#fe80, 0, 0, 0, 16#204, 16#acff, 16#fe17, 16#bf38}, #{codecs => [ipv6]}).
<<"\"fe80::204:acff:fe17:bf38\"">>
```

##### Encode record

```erlang
% -record(foo, {foo, bar}).
> euneus:encode(#foo{foo = 1, bar = 2}, #{
    codecs => [
      {records, #{
        foo => {record_info(fields, foo), record_info(size, foo)}
      }}
    ]
  }).
<<"{\"foo\":1,\"bar\":2}">>
```

### Decode Features

Some reasons to use Euneus for JSON decoding:

- Faster decoding than the OTP `json` module via some options:

  ```erlang
  #{
    array_finish => reversed,
    object_finish => reversed_proplist % or proplist
  }
  ```

- The overhead of transforming binary keys to, e.g., atoms
- Simple custom decoding via codecs:

  ```erlang
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
  ```

#### Decode Object keys

##### Keys to binary (default)

```erlang
> euneus:decode(<<"{\"foo\":\"bar\"}">>).
#{<<"foo">> => <<"bar">>}
```

##### Keys copy

Just do a binary copy of the key.

```erlang
> euneus:decode(<<"{\"foo\":\"bar\"}">>, #{object_keys => copy}).
#{<<"foo">> => <<"bar">>}
```

##### Keys to atom

```erlang
> euneus:decode(<<"{\"foo\":\"bar\"}">>, #{object_keys => atom}).
#{foo => <<"bar">>}
```

##### Keys to existing atom

```erlang
> euneus:decode(<<"{\"foo\":\"bar\"}">>, #{object_keys => existing_atom}).
#{foo => <<"bar">>}
```

#### Decode Codecs

##### Decode copy

Just do a binary copy of the value.

```erlang
> euneus:decode(<<"\"foo\"">>, #{codecs => [copy]}).
<<"foo">>
```

##### Decode timestamp

```erlang
> euneus:decode(<<"\"1970-01-01T00:00:00.000Z\"">>, #{codecs => [timestamp]}).
{0,0,0}
```

##### Decode datetime

```erlang
> euneus:decode(<<"\"1970-01-01T00:00:00Z\"">>, #{codecs => [datetime]}).
{{1970,1,1},{0,0,0}}
```

##### Decode ipv4

```erlang
> euneus:decode(<<"\"0.0.0.0\"">>, #{codecs => [ipv4]}).
{0,0,0,0}
```

##### Decode ipv6

```erlang
> euneus:decode(<<"\"0.0.0.0\"">>, #{codecs => [ipv4]}).
{0,0,0,0}
> euneus:decode(<<"\"::\"">>, #{codecs => [ipv6]}).
{0,0,0,0,0,0,0,0}
> euneus:decode(<<"\"::1\"">>, #{codecs => [ipv6]}).
{0,0,0,0,0,0,0,1}
> euneus:decode(<<"\"::192.168.42.2\"">>, #{codecs => [ipv6]}).
{0,0,0,0,0,0,49320,10754}
> euneus:decode(<<"\"fe80::204:acff:fe17:bf38\"">>, #{codecs => [ipv6]}).
{65152,0,0,0,516,44287,65047,48952}
```

##### Decode pid

```erlang
> euneus:decode(<<"\"<0.92.0>\"">>, #{codecs => [pid]}).
<0.92.0>
```

##### Decode port

```erlang
> euneus:decode(<<"\"#Port<0.1>\"">>, #{codecs => [port]}).
#Port<0.1>
```

##### Decode reference

```erlang
> euneus:decode(<<"\"#Ref<0.314572725.1088159747.110918>\"">>, #{codecs => [reference]}).
#Ref<0.314572725.1088159747.110918>
```

## Installation

### Erlang

```erlang
% rebar.config
{deps, [
    {json_polyfill, "~> 0.2"}, % Required only for OTP < 27
    {euneus, "~> 2.4"}
]}.
```

### Elixir

```elixir
# mix.exs
defp deps do
  [
    {:json_polyfill, "~> 0.2"}, # Required only for OTP < 27
    {:euneus, "~> 2.4"}
  ]
end
```

## Basic usage

```erlang
1> euneus:encode(#{age => 68, name => <<"Joe Armstrong">>, nationality => <<"British">>}).
<<"{\"name\":\"Joe Armstrong\",\"age\":68,\"nationality\":\"British\"}">>
2> euneus:decode(v(1)).
#{<<"age">> => 68,<<"name">> => <<"Joe Armstrong">>,<<"nationality">> => <<"British">>}
```

## Encode

The functions `euneus:encode/1` `euneus:encode/2` encodes an Erlang term into a binary JSON.
The second argument of `euneus:encode/2` are options.

Please see the `m:euneus_encoder` [documentation](https://hexdocs.pm/euneus/euneus_encoder.html)
for more examples and detailed explanation.

The data mapping and error reasons can be found in the OTP json encode function [documentation](https://erlang.org/documentation/doc-15.0-rc3/lib/stdlib-6.0/doc/html/json.html#encode/1).

## Decode

The functions `euneus:decode/1` and `euneus:decode/2` decodes a binary JSON into an Erlang term.
The second argument of `euneus:decode/2` are options.

Please see the `m:euneus_decoder` [documentation](https://hexdocs.pm/euneus/euneus_decoder.html)
for more examples and detailed explanation.

The data mapping and error reasons can be found in the OTP json decode function [documentation](https://erlang.org/documentation/doc-15.0-rc3/lib/stdlib-6.0/doc/html/json.html#decode/1).

## Stream

Three functions provide JSON decode streaming:

- `euneus:decode_stream_start/1` - Equivalent to `euneus:decode_stream_start(JSON, #{})`;
- `euneus:decode_stream_start/2` - Begin parsing a stream of bytes of a JSON value;
- `euneus:decode_stream_continue/2` - Continue parsing a stream of bytes of a JSON value.

Please see the `m:euneus_decoder` [documentation](https://hexdocs.pm/euneus/euneus_decoder.html)
for more examples and detailed explanation.

## Format

Two functions provide JSON formatting:

- `euneus:minify/1` - Removes any extra spaces and new line characters;
- `euneus:format/2` - Formats the JSON by passing custom options.

Please see the `m:euneus_formatter` [documentation](https://hexdocs.pm/euneus/euneus_formatter.html)
for more examples and detailed explanation.

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

### Results

> Setup:
>
> - OS : Linux
> - CPU: 12th Gen Intel(R) Core(TM) i9-12900HX
> - VM : Erlang/OTP 27 [erts-15.0.1] [source] [64-bit] [smp:24:24] [ds:24:24:10] [async-threads:1] [jit:ns]

```console
$ rebar3 as benchmark shell

1> euneus_benchmarker:encode_benchmark().
Code      ||   Samples       Avg   StdDev    Median      P99  Iteration    Rel
jiffy      1         3        26   36.69%        25       36   38474 us   100%
euneus     1         3        20   38.20%        18       29   49197 us    78%
thoas      1         3        10   36.06%         9       14     100 ms    38%

2> euneus_benchmarker:decode_benchmark().
Code       ||   Samples       Avg   StdDev    Median      P99  Iteration    Rel
euneus      1         3        24    2.44%        24       24   42268 us   100%
jiffy       1         3        19    3.09%        19       19   53589 us    79%
thoas       1         3        14    0.00%        14       14   71452 us    59%
```

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
