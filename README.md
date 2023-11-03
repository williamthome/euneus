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
{deps, [euneus]}
```

### Elixir

```elixir
# mix.exs
def deps do
  [{:euneus, "~> 0.4"}]
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
| **Erlang ->**        	| **Encode Options ->**                                                                                                                                    	| **JSON ->**                 	| **Decode Options ->**                                                                                                    	| **Erlang**           	|
|----------------------	|----------------------------------------------------------------------------------------------------------------------------------------------------------	|-----------------------------	|--------------------------------------------------------------------------------------------------------------------------	|----------------------	|
| undefined            	| #{}                                                                                                                                                      	| null                        	| #{}                                                                                                                      	| undefined            	|
| true                 	| #{}                                                                                                                                                      	| true                        	| #{}                                                                                                                      	| true                 	|
| false                	| #{}                                                                                                                                                      	| false                       	| #{}                                                                                                                      	| false                	|
| abc                  	| #{}                                                                                                                                                      	| "abc"                       	| #{}                                                                                                                      	| <<"abc">>            	|
| "abc"                	| #{}                                                                                                                                                      	| [97,98,99]                  	| #{}                                                                                                                      	| "abc"                	|
| <<"abc">>            	| #{}                                                                                                                                                      	| "abc"                       	| #{}                                                                                                                      	| <<"abc">>            	|
| {{1970,1,1},{0,0,0}} 	| #{}                                                                                                                                                      	| "1970-01-01T00:00:00Z"      	| #{}                                                                                                                      	| {{1970,1,1},{0,0,0}} 	|
| {0,0,0}              	| #{}                                                                                                                                                      	| "1970-01-01T00:00:00.000Z"  	| #{}                                                                                                                      	| {0,0,0}              	|
| 123                  	| #{}                                                                                                                                                      	| 123                         	| #{}                                                                                                                      	| 123                  	|
| 123.45600            	| #{}                                                                                                                                                      	| 123.456                     	| #{}                                                                                                                      	| 123.456              	|
| [true,0,undefined]   	| #{}                                                                                                                                                      	| [true,0,null]               	| #{null_term => nil}                                                                                                      	| [true,0,nil]         	|
| #{foo => bar}        	| #{}                                                                                                                                                      	| {"foo":"bar"}               	| #{keys => fun(Key, _Opts) -> binary_to_atom(Key) end}                                                           	| #{foo => <<"bar">>}  	|
| {myrecord, val}      	| #{encode_unhandled => fun({myrecord, Val}, Opts) -><br>    Encode = maps:get(encode_list, Opts),<br>    Encode([myrecord, #{key => Val}], Opts)<br>end}) 	| ["myrecord", {"key":"val"}] 	| #{arrays => fun([<<"myrecord">>, #{<<"key">> := Val}], _Opts) -><br>    {myrecord, binary_to_atom(Val)}<br>end} 	| {myrecord, val}      	|

### Why not more built-in types?

The goal of `Euneus` is to have built-in types that can be encoded and then decoded to the original value. If you have any type that can be encoded and rolled back, feel free to open a [new issue](https://github.com/williamthome/euneus/issues/new) to discuss it.

### Note about proplists

Proplists are not handled by Euneus, you must override the `list_encoder` option in the encoder to handle them, for example:

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
% {ok,<<"{\"foo\":\"bar\",\"bar\":{\"0\":\"ok\"}}">>}
```

Another option is to convert proplists to maps before the encoding. The reason is because it's impossible to know when a list is a proplist and also because a proplist cannot be decoded. See the [Why not more built-in types?](#why-not-more-built-in-types) section.

## Differences to Thoas

Euneus is based on [Thoas][thoas], so let's discuss the differences.

The main difference between `Euneus` to `Thoas` is that Euneus gives more control to encoding or decoding data. All encode functions can be overridden and extended and all decoded data can be overridden and transformed.

### Encode

Available encode options:

```erlang
#{
    %% nulls defines what terms will be replaced with the null literal (default: ['undefined']).
    nulls => nonempty_list(),
    %% encode_binary allow override the binary() encoding.
    binary_encoder => function((binary(), Opts :: map()) -> iolist()),
    %% atom_encoder allow override the atom() encoding.
    atom_encoder => function((atom(), Opts :: map()) -> iolist()),
    %% integer_encoder allow override the integer() encoding.
    integer_encoder => function((integer(), Opts :: map()) -> iolist()),
    %% float_encoder allow override the float() encoding.
    float_encoder => function((float(), Opts :: map()) -> iolist()),
    %% list_encoder allow override the list() encoding.
    list_encoder => function((list(), Opts :: map()) -> iolist()),
    %% map_encoder allow override the map() encoding.
    map_encoder => function((map(), Opts :: map()) -> iolist()),
    %% datetime_encoder allow override the calendar:datetime() encoding.
    datetime_encoder => function((calendar:datetime(), Opts :: map()) -> iolist()),
    %% timestamp_encoder allow override the erlang:timestamp() encoding.
    timestamp_encoder => function((erlang:timestamp(), Opts :: map()) -> iolist()),
    %% unhandled_encoder allow encode any custom term (default: raise unsupported_type error).
    unhandled_encoder => function((term(), Opts :: map()) -> iolist()),
    %% escaper allow override the binary escaping (default: json)
    escaper => json
             | html
             | javascript
             | unicode
             | function((binary(), Opts :: map()) -> iolist())
}
```

For example:

```erlang
EncodeOpts = #{
    binary_encoder => fun
        (<<"foo">>, Opts) ->
            euneus_encoder:escape_binary(<<"bar">>, Opts);
        (Bin, Opts) ->
            euneus_encoder:escape_binary(Bin, Opts)
    end,
    unhandled_encoder => fun
        ({_, _, _, _} = Ip, Opts) ->
            case inet:ntoa(Ip) of
                {error, einval} ->
                    error(invalid_ip);
                IpStr ->
                    IpBin = list_to_binary(IpStr),
                    euneus_encoder:escape_binary(IpBin, Opts)
            end;
        (Term, Opts) ->
            euneus_encoder:throw_unsupported_type_error(Term, Opts)
    end
},
Data = #{<<"foo">> => bar, ipv4 => {127, 0, 0, 1}, none => undefined},
euneus:encode_to_binary(Data, EncodeOpts).
%% {ok, <<"{\"bar\":\"bar\",\"ipv4\":\"127.0.0.1\",\"none\":null}">>}
```

### Decode

Available decode options:

```erlang
#{
    %% null_term is the null literal override (default: 'undefined').
    null_term => term(),
    %% arrays allow override any array/list().
    arrays => function((list(), Opts :: map()) -> term()),
    %% objects allow override any object/map().
    objects => function((map(), Opts :: map()) -> term()),
    %% keys allow override the keys from JSON objects.
    keys => copy
          | to_atom
          | to_existing_atom
          | to_integer
          | function((binary(), Opts :: map()) -> term()),
    %% values allow override any other term, like array item or object value.
    values => copy
            | to_atom
            | to_existing_atom
            | to_integer
            | function((binary(), Opts :: map()) -> term())
}
```

For example:

```erlang
DecodeOpts = #{
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
},
JSON = <<"{\"bar\":\"bar\",\"ipv4\":\"127.0.0.1\",\"none\":null}">>,
euneus:decode(JSON, DecodeOpts).
%% {ok,#{foo"=> <<"bar">>,
%%       ipv4 => {127,0,0,1},
%%       none => nil}}
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
% {ok,[foo,foo,#{<<"foo">> => foo}]}
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
| **File**                   	|  **Euneus** 	|  **Thoas** 	| **Comparison** 	|
|----------------------------	|------------:	|-----------:	|---------------:	|
| blockchain.json            	|  **9.73 K** 	|     7.86 K 	|          1.24x 	|
| giphy.json                 	|  **897.47** 	|     853.31 	|          1.05x 	|
| github.json                	|  **3.14 K** 	|     2.54 K 	|          1.24x 	|
| govtrack.json              	|   **12.72** 	|      12.27 	|          1.04x 	|
| issue-90.json              	|   **28.92** 	|      17.50 	|          1.65x 	|
| json-generator-pretty.json 	|  **1.16 K** 	|     1.08 K 	|          1.08x 	|
| json-generator.json        	|  **1.17 K** 	|     1.08 K 	|          1.08x 	|
| pokedex.json               	|      1.63 K 	| **1.73 K** 	|          1.07x 	|
| utf-8-escaped.json         	| **11.88 K** 	|    10.57 K 	|          1.12x 	|
| utf-8-unescaped.json       	| **12.19 K** 	|    10.83 K 	|          1.13x 	|

### Decode

> **Note**
>
> `Thoas` does not permit any customization and does not decode `ISO 8601` dates to erlang term, but Euneus decodes out of the box, for example, `"1970-01-01T00:00:00Z"` to `{{1970,01,01},{0,0,0}}` :: [calendar:datetime()](https://www.erlang.org/doc/man/calendar.html#data-types) and `"1970-01-01T00:00:00.000Z"` to `{0,0,0}` :: [erlang:timestamp()](https://www.erlang.org/doc/man/os#timestamp-0).

<!-- Generated via https://www.tablesgenerator.com/markdown_tables -->
<!-- To edit, open "./assets/md-tables/bench-decode.tgn" in the link above. -->
| **File**                   	|  **Euneus** 	| **Thoas** 	| **Comparison** 	|
|----------------------------	|------------:	|----------:	|---------------:	|
| blockchain.json            	|  **7.18 K** 	|    5.78 K 	|          1.24x 	|
| giphy.json                 	|  **474.91** 	|    474.75 	|          1.00x 	|
| github.json                	|  **2.33 K** 	|    2.02 K 	|          1.16x 	|
| govtrack.json              	|   **16.35** 	|     15.65 	|          1.04x 	|
| issue-90.json              	|   **25.35** 	|     17.70 	|          1.43x 	|
| json-generator-pretty.json 	|  **617.33** 	|    542.99 	|          1.14x 	|
| json-generator.json        	|  **728.01** 	|    655.15 	|          1.11x 	|
| pokedex.json               	|  **1.37 K** 	|    1.33 K 	|          1.03x 	|
| utf-8-escaped.json         	|  **1.88 K** 	|    1.66 K 	|          1.13x 	|
| utf-8-unescaped.json       	| **10.87 K** 	|   10.47 K 	|          1.04x 	|

## Tests

There are Eunit tests in [euneus_encoder](/src/euneus_encoder.erl) and [euneus_decoder](/src/euneus_decoder.erl) and tests suites in a specific project under the [euneus_test](/euneus_test/) directory. Euneus has more than 330 tests.

Also, the parser is tested using [JSONTestSuite](https://github.com/nst/JSONTestSuite) and all tests are green:

![JSON Test Suite](/assets/images/json-test-suite-result.png)

> **Note**
>
> All of the JSONTestSuite tests are embedded in Euneus tests.

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
