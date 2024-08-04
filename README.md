# Euneus

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
    {json_polyfill, "0.1.1"}, % Required only for OTP < 27
    {euneus, {git, "https://github.com/williamthome/euneus.git", {branch, "main"}}}
]}.
```

### Elixir

```elixir
# mix.exs
defp deps do
  [
    {:json_polyfill, "~> 0.1.1"}, # Required only for OTP < 27
    {:euneus, git: "https://github.com/williamthome/euneus.git", branch: "main"}
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

Please see the `m:euneus_encoder` [documentation](https://hexdocs.pm/euneus/doc/euneus_encoder.html)
for more examples and detailed explanation.

The data mapping and error reasons can be found in the OTP json encode function [documentation](https://erlang.org/documentation/doc-15.0-rc3/lib/stdlib-6.0/doc/html/json.html#encode/1).

## Decode

The functions `euneus:decode/1` and `euneus:decode/2` decodes a binary JSON into an Erlang term.
The second argument of `euneus:decode/2` are options.

Please see the `m:euneus_decoder` [documentation](https://hexdocs.pm/euneus/doc/euneus_decoder.html)
for more examples and detailed explanation.

The data mapping and error reasons can be found in the OTP json decode function [documentation](https://erlang.org/documentation/doc-15.0-rc3/lib/stdlib-6.0/doc/html/json.html#decode/1).

## Format

Two functions provide JSON formatting:

- `euneus:minify/1` - Removes any extra spaces and new line characters;
- `euneus:format/2` - Formats the JSON by passing custom options.

Please see the `m:euneus_formatter` [documentation](https://hexdocs.pm/euneus/doc/euneus_formatter.html)
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
>
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
