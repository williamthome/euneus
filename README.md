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

## Installation

### Erlang

```erlang
% rebar.config
{deps, [
    json_polyfill, % Required only for OTP < 27
    {euneus, "2.4.0"}
]}.
```

### Elixir

```elixir
# mix.exs
defp deps do
  [
    {:json_polyfill, "~> 0.1"}, # Required only for OTP < 27
    {:euneus, "~> 2.4"}
  ]
end
```

Or consider using [Exneus](https://github.com/williamthome/exneus):

```elixir
# mix.exs
defp deps do
  [
    {:json_polyfill, "~> 0.1"}, # Required only for OTP < 27
    {:exneus, "~> 0.1"}
  ]
end
```

> Exneus is a wrapper of Euneus for Elixir.

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
