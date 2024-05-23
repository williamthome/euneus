# Euneus

An incredibly flexible and performant JSON parser, generator and formatter in pure Erlang built on top of the OTP `json` module.

Both encoder and decoder fully conform to [RFC 8259](https://tools.ietf.org/html/rfc8259) and [ECMA 404](https://ecma-international.org/publications-and-standards/standards/ecma-404/) standards. The decoder is tested using [JSONTestSuite](https://github.com/nst/JSONTestSuite).

## Installation

### Erlang

```erlang
% rebar.config
{deps, [
    {euneus, "2.0.0"},
    {json_polyfill, "0.1.0"} % required if OTP < 27
]}
```

### Elixir

```elixir
# mix.exs
def deps do
  [
    {:euneus, "~> 2.0"},
    {:json_polyfill, "~> 0.1"} # required if OTP < 27
  ]
end
```

## Basic Usage

```erlang
1> JSON = euneus:encode(#{title => <<"The Movie">>, year => 1990, starring => ['Mike', <<"Joe">>, "Robert"]}).
<<"{\"title\":\"The Movie\",\"year\":1990,\"starring\":[\"Mike\",\"Joe\",[82,111,98,101,114,116]]}">>
2> euneus:decode(JSON).
#{<<"starring">> => [<<"Mike">>,<<"Joe">>,"Robert"],
  <<"title">> => <<"The Movie">>,<<"year">> => 1990}
```

## Encode

### Data Mapping

| **Erlang**             | **JSON** |
|------------------------|----------|
| `integer() \| float()` | Number   |
| `true \| false `       | Boolean  |
| `null`                 | Null     |
| `binary()`             | String   |
| `atom()`               | String   |
| `list()`               | Array    |
| `#{binary() => _}`     | Object   |
| `#{atom() => _}`       | Object   |
| `#{integer() => _}`    | Object   |

## Decode

### Data Mapping

| **JSON** | **Erlang**             |
|----------|------------------------|
| Number   | `integer() \| float()` |
| Boolean  | `true \| false`        |
| Null     | `null`                 |
| String   | `binary()`             |
| Object   | `#{binary() => _}`     |

## TODO

- [ ] Encoder
  - [ ] Add and improve tests
  - [ ] Specs
  - [ ] Docs
  - [ ] Codecs
    - [ ] term_codec
  - [ ] Escape
    - [ ] HTML
    - [ ] Javascript
    - [ ] Unicode
- [ ] Decoder
  - [ ] Add and improve tests
  - [ ] Specs
  - [ ] Docs
  - [ ] Codecs
    - [ ] term_codec
  - [ ] Possibility to resume when an error occur
- [ ] Formatter
  - [ ] Specs
  - [ ] Docs

## Why the name Euneus?

Euneus is the twin brother of Thoas, Jason's son.

## Sponsors

If you like this tool, please consider [sponsoring me](https://github.com/sponsors/williamthome).
I'm thankful for your never-ending support :heart:

I also accept coffees :coffee:

[!["Buy Me A Coffee"](https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png)](https://www.buymeacoffee.com/williamthome)

## Contributing

### Issues

Feel free to [submit an issue on Github](https://github.com/williamthome/euneus/issues/new).

## License

Copyright (c) 2023-2024 [William Fank Thomé](https://github.com/williamthome)

`euneus` is 100% open source and community-driven. All components are available under the Apache 2 License on [GitHub](https://github.com/williamthome/euneus).

See [LICENSE.md](LICENSE.md) for more information.
