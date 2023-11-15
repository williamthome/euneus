# Euneus

A JSON parser and generator in pure Erlang.

Euneus is a rewrite of [Thoas][thoas].

Like Thoas, both the parser and generator fully conform to
[RFC 8259](https://tools.ietf.org/html/rfc8259) and
[ECMA 404](http://www.ecma-international.org/publications/standards/Ecma-404.htm).

## Table of Contents

- [Installation](#installation)
    - [Erlang](#erlang)
    - [Elixir](#elixir)
- [Basic Usage](#basic-usage)
- [Data Mapping](#data-mapping)
    - [Why not more built-in types?](#why-not-more-built-in-types)
    - [Note about proplists](#note-about-proplists)
- [Differences to Thoas](#differences-to-thoas)
    - [Encode](#encode)
    - [Decode](#decode)
        - [Resuming](#resuming)
    - [Why Euneus over Thoas?](#why-euneus-over-thoas)
- [Benchmarks](#benchmarks)
    - [Encode](#encode-1)
    - [Decode](#decode-1)
- [Tests](#tests)
- [Smart modules](#smart-modules)
- [Credits](#credits)
- [Why the name Euneus?](#why-the-name-euneus)
- [Sponsors](#sponsors)
- [Contributing](#contributing)
    - [Issues](#issues)
    - [Installation](#installation-1)
    - [Commands](#commands)
- [License](#license)

## Installation

### Erlang

```erlang
% rebar.config
{deps, [{euneus, "0.7.0"}]}
```

### Elixir

```elixir
# mix.exs
def deps do
  [{:euneus, "~> 0.7"}]
end
```

## Basic Usage

```erlang
1> {ok, JSON} = euneus:encode_to_binary(#{name => #{english => <<"Charmander">>, japanese => <<"ヒトカゲ"/utf8>>}, caught_at => erlang:timestamp(), type => [fire], profile => #{height => 0.6, weight => 8}, ability => #{0 => <<"Blaze">>, 1 => undefined}}).
{ok, <<"{\"name\":{\"english\":\"Charmander\",\"japanese\":\"ヒトカゲ\"},\"profile\":{\"height\":0.6,\"weight\":8},\"type\":[\"fire\"],\"caught_at\":\"2023-10-24T05:47:04.939Z\",\"ability\":{\"0\":\"Blaze\",\"1\":null}}">>}

2> euneus:decode(JSON).
{ok,#{<<"ability">> =>
          #{<<"0">> => <<"Blaze">>,<<"1">> => undefined},
      <<"caught_at">> => {1698,126333,753000},
      <<"name">> =>
          #{<<"english">> => <<"Charmander">>,
            <<"japanese">> =>
                <<227,131,146,227,131,136,227,130,171,227,130,178>>},
      <<"profile">> => #{<<"height">> => 0.6,<<"weight">> => 8},
      <<"type">> => [<<"fire">>]}}

3> euneus:decode(JSON, #{
    keys => fun
        (<<Char>> = Key, _Opts) when Char >= $0, Char =< $9 ->
            binary_to_integer(Key);
        (Key, _Opts) ->
            binary_to_existing_atom(Key)
    end
}).
{ok,#{name =>
          #{english => <<"Charmander">>,
            japanese =>
                <<227,131,146,227,131,136,227,130,171,227,130,178>>},
      profile => #{height => 0.6,weight => 8},
      type => [<<"fire">>],
      caught_at => {1698,126333,753000},
      ability => #{0 => <<"Blaze">>,1 => undefined}}}
```

## Data Mapping

<!-- Generated via https://www.tablesgenerator.com/markdown_tables -->
<!-- To edit, open "./assets/md-tables/data-mapping.tgn" in the link above. -->
| **Erlang ->**                	| **Encode Options ->**                                                                                                                                     	| **JSON ->**                 	| **Decode Options ->**                                                                                           	| **Erlang**                   	|
|------------------------------	|-----------------------------------------------------------------------------------------------------------------------------------------------------------	|-----------------------------	|-----------------------------------------------------------------------------------------------------------------	|------------------------------	|
| undefined                    	| #{}                                                                                                                                                       	| null                        	| #{}                                                                                                             	| undefined                    	|
| undefined                    	| #{}                                                                                                                                                       	| null                        	| #{null_term => nil}                                                                                             	| nil                          	|
| true                         	| #{}                                                                                                                                                       	| true                        	| #{}                                                                                                             	| true                         	|
| false                        	| #{}                                                                                                                                                       	| false                       	| #{}                                                                                                             	| false                        	|
| abc                          	| #{}                                                                                                                                                       	| "abc"                       	| #{}                                                                                                             	| <<"abc">>                    	|
| "abc"                        	| #{}                                                                                                                                                       	| [97,98,99]                  	| #{}                                                                                                             	| "abc"                        	|
| <<"abc">>                    	| #{}                                                                                                                                                       	| "abc"                       	| #{}                                                                                                             	| <<"abc">>                    	|
| {{1970,1,1},{0,0,0}}         	| #{}                                                                                                                                                       	| "1970-01-01T00:00:00Z"      	| #{}                                                                                                             	| {{1970,1,1},{0,0,0}}         	|
| {0,0,0}                      	| #{}                                                                                                                                                       	| "1970-01-01T00:00:00.000Z"  	| #{}                                                                                                             	| {0,0,0}                      	|
| 123                          	| #{}                                                                                                                                                       	| 123                         	| #{}                                                                                                             	| 123                          	|
| 123.45600                    	| #{}                                                                                                                                                       	| 123.456                     	| #{}                                                                                                             	| 123.456                      	|
| [<<"foo">>,true,0,undefined] 	| #{}                                                                                                                                                       	| ["foo",true,0,null]         	| #{}                                                                                                             	| [<<"foo">>,true,0,undefined] 	|
| #{foo => bar}                	| #{}                                                                                                                                                       	| {"foo":"bar"}               	| #{}                                                                                                             	| #{<<"foo">> => <<"bar">>}    	|
| #{foo => bar}                	| #{}                                                                                                                                                       	| {"foo":"bar"}               	| #{keys => to_existing_atom}                                                                                     	| #{foo => <<"bar">>}          	|
| #{0 => 0}                    	| #{}                                                                                                                                                       	| {"0":0}                     	| #{keys => to_integer}                                                                                           	| #{0 => 0}                    	|
| {myrecord, val}              	| #{unhandled_encoder => fun({myrecord, Val}, Opts) -><br>    Encode = maps:get(list_encoder, Opts),<br>    Encode([myrecord, #{key => Val}], Opts)<br>end}) 	| ["myrecord", {"key":"val"}] 	| #{arrays => fun([<<"myrecord">>, #{<<"key">> := Val}], _Opts) -><br>    {myrecord, binary_to_atom(Val)}<br>end} 	| {myrecord, val}              	|

### Why not more built-in types?

The goal of `Euneus` is to have built-in types that can be encoded and then decoded to the original value. If you have any type that can be encoded and rolled back, feel free to open a [new issue](https://github.com/williamthome/euneus/issues/new) to discuss it.

### Note about proplists

Proplists are not handled by Euneus, you must convert proplists to maps before the encoding or override the `list_encoder` option in the encoder to handle them, for example:

```erlang
1> Options = #{
       list_encoder => fun
           ([{K, _} | _] = Proplist, Opts)
             when is_binary(K); is_atom(K); is_integer(K) ->
               Map = proplists:to_map(Proplist),
               euneus_encoder:encode_map(Map, Opts);
           (List, Opts) ->
               euneus_encoder:encode_list(List, Opts)
       end
   }.

2> Proplist = [{foo, bar}, {bar, [{0, ok}]}].

3> euneus:encode_to_binary(Proplist, Options).
{ok,<<"{\"foo\":\"bar\",\"bar\":{\"0\":\"ok\"}}">>}
```

The reason for that is because it's impossible to know when a list is a proplist and also because a proplist cannot be decoded. Please see the [Why not more built-in types?](#why-not-more-built-in-types) section for more info about this decision.

## Differences to Thoas

Euneus is based on [Thoas][thoas], so let's discuss the differences.

The main difference between `Euneus` to `Thoas` is that Euneus gives more control to encoding or decoding data. All encode functions can be overridden and extended and all decoded data can be overridden and transformed.

### Encode

Available encode options:

```erlang
#{
    %% nulls defines what terms will be replaced with the null literal (default: ['undefined']).
    nulls => nonempty_list(),
    %% binary_encoder allow override the binary() encoding.
    binary_encoder => function((binary(), euneus_encoder:options()) -> iolist()),
    %% atom_encoder allow override the atom() encoding.
    atom_encoder => function((atom(), euneus_encoder:options()) -> iolist()),
    %% integer_encoder allow override the integer() encoding.
    integer_encoder => function((integer(), euneus_encoder:options()) -> iolist()),
    %% float_encoder allow override the float() encoding.
    float_encoder => function((float(), euneus_encoder:options()) -> iolist()),
    %% list_encoder allow override the list() encoding.
    list_encoder => function((list(), euneus_encoder:options()) -> iolist()),
    %% map_encoder allow override the map() encoding.
    map_encoder => function((map(), euneus_encoder:options()) -> iolist()),
    %% datetime_encoder allow override the calendar:datetime() encoding.
    datetime_encoder => function((calendar:datetime(), euneus_encoder:options()) -> iolist()),
    %% timestamp_encoder allow override the erlang:timestamp() encoding.
    timestamp_encoder => function((erlang:timestamp(), euneus_encoder:options()) -> iolist()),
    %% unhandled_encoder allow encode any custom term (default: raise unsupported_type error).
    unhandled_encoder => function((term(), euneus_encoder:options()) -> iolist()),
    %% escaper allow override the binary escaping (default: json)
    escaper => json
             | html
             | javascript
             | unicode
             | function((binary(), euneus_encoder:options()) -> iolist()),
    error_handler => function(( error | exit | throw
                              , term()
                              , erlang:stacktrace() ) -> euneus_encoder:result())
}
```

For example:

```erlang
1> EncodeOpts = #{
       binary_encoder => fun
           (<<"foo">>, Opts) ->
               euneus_encoder:encode_binary(<<"bar">>, Opts);
           (Bin, Opts) ->
               euneus_encoder:encode_binary(Bin, Opts)
       end,
       unhandled_encoder => fun
           ({_, _, _, _} = Ip, Opts) ->
               case inet:ntoa(Ip) of
                   {error, einval} ->
                       throw(invalid_ip);
                   IpStr ->
                       IpBin = list_to_binary(IpStr),
                       euneus_encoder:encode_binary(IpBin, Opts)
               end;
           (Term, Opts) ->
               euneus_encoder:throw_unsupported_type_error(Term, Opts)
       end,
       error_handler => fun
           (throw, invalid_ip, _Stacktrace) ->
               {error, invalid_ip};
           (Class, Reason, Stacktrace) ->
               euneus_encoder:handle_error(Class, Reason, Stacktrace)
       end
   }.

2> Data = #{<<"foo">> => bar, ipv4 => {127,0,0,1}, none => undefined}.

3> euneus:encode_to_binary(Data, EncodeOpts).
{ok, <<"{\"bar\":\"bar\",\"ipv4\":\"127.0.0.1\",\"none\":null}">>}

4> euneus:encode_to_binary({1270,0,0,1}, EncodeOpts).
{error, invalid_ip}
```

### Decode

Available decode options:

```erlang
#{
    %% null_term is the null literal override (default: 'undefined').
    null_term => term(),
    %% arrays allow override any array/list().
    arrays => function((list(), euneus_decoder:options()) -> term()),
    %% objects allow override any object/map().
    objects => function((map(), euneus_decoder:options()) -> term()),
    %% keys allow override the keys from JSON objects.
    keys => copy
          | to_atom
          | to_existing_atom
          | to_integer
          | function((binary(), euneus_decoder:options()) -> term()),
    %% values allow override any other term, like array item or object value.
    values => copy
            | to_atom
            | to_existing_atom
            | to_integer
            | function((binary(), euneus_decoder:options()) -> term())
}
```

For example:

```erlang
1> DecodeOpts = #{
      null_term => nil,
      keys => fun
          (<<"bar">>, _Opts) ->
              foo;
          (Key, _Opts) ->
              binary_to_atom(Key)
      end,
      values => fun
          (<<"127.0.0.1">>, _Opts) ->
              {127, 0, 0, 1};
          (Value, _Opts) ->
              Value
      end
   }.

2> JSON = <<"{\"bar\":\"bar\",\"ipv4\":\"127.0.0.1\",\"none\":null}">>.

3> euneus:decode(JSON, DecodeOpts).
{ok,#{foo => <<"bar">>,
      ipv4 => {127,0,0,1},
      none => nil}}
```

#### Resuming

Euneus permits resuming the decoding when an invalid token is found. Any value can replace the invalid token by overriding the `error_handler` option, e.g.:

```erlang
1> ErrorHandler = fun
      (throw, {{token, Token}, Rest, Opts, Input, Pos, Buffer}, _Stacktrace) ->
          Replacement = foo,
          euneus_decoder:resume(Token, Replacement, Rest, Opts, Input, Pos, Buffer);
      (Class, Reason, Stacktrace) ->
          euneus_decoder:handle_error(Class, Reason, Stacktrace)
   end.

2> Opts = #{error_handler => ErrorHandler}.

3> euneus:decode(<<"[1e999,1e999,{\"foo\": 1e999}]">>, Opts).
{ok,[foo,foo,#{<<"foo">> => foo}]}
```

> **Note**
>
> By using `euneus_decoder:resume/6` the replacement will be the `null_term` option.

### Why Euneus over Thoas?

`Thoas` is incredible, works performant and perfectly fine, but `Euneus` is more flexible, permitting more customizations, and is more performant than Thoas. See the [benchmarks](#benchmarks).

The motivation for Euneus is [this PR](https://github.com/lpil/thoas/pull/28).

## Benchmarks

All the benchmarks compare `Euneus` and `Thoas` via [Benchee](https://github.com/bencheeorg/benchee) to obtain the results.

Use `$ make bench.encode` or `$ make bench.decode` to run the benchmarks. Edit the scripts in the `./euneus_bench/script` folder if needed.

The benchmarks use the smart versions. Please the [Smart modules](#smart-modules) section for more information.

> **Note**
>
> - Results:
>
>   Values in `IPS` (iterations per second), aka how often can the given function be executed within one second (the higher the better - good for graphing), only for run times.
>   - `Bold`: best IPS;
>
> - System info:
>   - Erlang: 26.1
>   - Elixir: 1.16.0-dev
>   - Operating system: Linux
>   - Available memory: 15.54 GB
>   - CPU Information: Intel(R) Core(TM) i7-7700HQ CPU @ 2.80GHz
>   - Number of Available Cores: 8
>
> - Benchmark setup:
>   - warmup: 5 s
>   - time: 30 s
>   - memory time: 1 s
>   - reduction time: 0 ns
>   - parallel: 1

### Encode

> **Note**
>
> `Thoas` does not permit any customization.

<!-- Generated via https://www.tablesgenerator.com/markdown_tables -->
<!-- To edit, open "./assets/md-tables/bench-encode.tgn" in the link above. -->
| **File**                   	|  **Euneus** 	| **Thoas** 	| **Comparison** 	|
|----------------------------	|------------:	|----------:	|---------------:	|
| blockchain.json            	| **12.00 K** 	|    7.92 K 	|          1.52x 	|
| giphy.json                 	|  **1.03 K** 	|    0.86 K 	|          1.20x 	|
| github.json                	|  **3.67 K** 	|    2.57 K 	|          1.43x 	|
| govtrack.json              	|   **13.33** 	|     12.37 	|          1.08x 	|
| issue-90.json              	|   **30.10** 	|     17.56 	|          1.71x 	|
| json-generator-pretty.json 	|  **1.45 K** 	|    1.09 K 	|          1.33x 	|
| json-generator.json        	|  **1.45 K** 	|    1.08 K 	|          1.34x 	|
| pokedex.json               	|  **2.08 K** 	|    1.75 K 	|          1.19x 	|
| utf-8-escaped.json         	| **11.99 K** 	|   10.63 K 	|          1.13x 	|
| utf-8-unescaped.json       	| **11.99 K** 	|   10.89 K 	|          1.10x 	|

> **Note**
>
> `Thoas` does not permit any customization and does not decode `ISO 8601` dates to erlang term, but Euneus decodes out of the box, for example, `"1970-01-01T00:00:00Z"` to `{{1970,01,01},{0,0,0}}` :: [calendar:datetime()](https://www.erlang.org/doc/man/calendar.html#data-types) and `"1970-01-01T00:00:00.000Z"` to `{0,0,0}` :: [erlang:timestamp()](https://www.erlang.org/doc/man/os#timestamp-0).

<!-- Generated via https://www.tablesgenerator.com/markdown_tables -->
<!-- To edit, open "./assets/md-tables/bench-decode.tgn" in the link above. -->
| **File**                   	|  **Euneus** 	| **Thoas** 	| **Comparison** 	|
|----------------------------	|------------:	|----------:	|---------------:	|
| blockchain.json            	|  **7.18 K** 	|    5.78 K 	|          1.24x 	|
| giphy.json                 	|  **589.84** 	|    536.41 	|          1.08x 	|
| github.json                	|  **2.33 K** 	|    2.02 K 	|          1.16x 	|
| govtrack.json              	|   **16.90** 	|     15.89 	|          1.06x 	|
| issue-90.json              	|   **25.35** 	|     17.70 	|          1.43x 	|
| json-generator-pretty.json 	|  **617.33** 	|    542.99 	|          1.14x 	|
| json-generator.json        	|  **800.43** 	|    700.34 	|          1.15x 	|
| pokedex.json               	|  **1.36 K** 	|    1.31 K 	|          1.04x 	|
| utf-8-escaped.json         	|  **2.05 K** 	|    1.67 K 	|          1.23x 	|
| utf-8-unescaped.json       	| **11.27 K** 	|   10.48 K 	|          1.08x 	|

## Tests

There are Eunit tests in [euneus_encoder](/src/euneus_encoder.erl) and [euneus_decoder](/src/euneus_decoder.erl) and tests suites in a specific project under the [euneus_test](/euneus_test/) directory. Euneus has more than 330 tests.

Also, the parser is tested using [JSONTestSuite](https://github.com/nst/JSONTestSuite) and all tests passes:

![JSON Test Suite](/assets/images/json-test-suite-result.png)

See the [Euneus parser](https://github.com/nst/JSONTestSuite/tree/master/parsers/test_erlang_euneus) in JSONTestSuite.

> **Note**
>
> All of the JSONTestSuite tests are embedded in Euneus tests.

## Smart modules

Euneus has modules that permit customizations and others that use the default options. The modules without customizations are called smart. The smart versions are faster because they do not do any option checks.

If you are good to go with the default options, please use the smart versions:
- Encode:
    - `euneus:encode/1` or `euneus_encoder_smart_json:encode/1`;
    - `euneus:encode_js/1` or `euneus_encoder_smart_javascript:encode/1`;
    - `euneus:encode_html/1` or `euneus_encoder_smart_html:encode/1`;
    - `euneus:encode_unicode/1` or `euneus_encoder_smart_unicode:encode/1`;
- Decode:
    - `euneus:decode/1` or `euneus_decoder_smart:decode/1`.

## Credits

Euneus is a rewrite of Thoas, so all credits go to [Michał Muskała](https://github.com/michalmuskala), [Louis Pilfold](https://github.com/lpil), also both [Jason][jason] and [Thoas][thoas] contributors. Thanks for the hard work!

## Why the name Euneus?

Euneus is the twin brother of [Thoas](https://en.wikipedia.org/wiki/Thoas_(son_of_Jason)).

## Sponsors

If you like this tool, please consider [sponsoring me](https://github.com/sponsors/williamthome).
I'm thankful for your never-ending support :heart:

I also accept coffees :coffee:

[!["Buy Me A Coffee"](https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png)](https://www.buymeacoffee.com/williamthome)

## Contributing

### Issues

Feel free to [submit an issue on Github](https://github.com/williamthome/euneus/issues/new).

### Installation

```console
# Clone this repo
git clone git@github.com:williamthome/euneus.git

# Navigate to the project root
cd euneus

# Compile (ensure you have rebar3 installed)
rebar3 compile
```

### Commands

```
# Benchmark euneus:encode/1
$ make bench.encode
```

```
# Benchmark euneus:decode/1
$ make bench.decode
```

```
# Run all tests
$ make test
```

```
# Run all tests and dialyzer
$ make check
```

> **Note**:
>
> Open the [Makefile](Makefile) to see all commands.

## License

Euneus is released under the [Apache License 2.0](LICENSE.md).

Euneus is based off of [Thoas][thoas], which is also Apache 2.0 licensed.

Some elements have their origins in the [Poison library][poison] and were initially licensed under [CC0-1.0](https://creativecommons.org/publicdomain/zero/1.0/).

[jason]: https://github.com/michalmuskala/jason
[thoas]: https://github.com/lpil/thoas
[poison]: https://github.com/devinus/poison
