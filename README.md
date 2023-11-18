# Euneus

An incredibly flexible and performant JSON parser and generator.

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
- [Plugins](#plugins)
    - [Usage](#usage)
        - [Encode](#encode)
        - [Decode](#decode)
    - [Built-in Plugins](#built-in-plugins)
        - [datetime](#datetime)
        - [inet](#inet)
        - [pid](#pid)
        - [port](#port)
        - [proplist](#proplist)
        - [reference](#reference)
        - [timestamp](#timestamp)
- [Differences to Thoas](#differences-to-thoas)
    - [Encode](#encode-1)
    - [Decode](#decode-1)
        - [Resuming](#resuming)
    - [Why Euneus over Thoas?](#why-euneus-over-thoas)
- [Benchmarks](#benchmarks)
    - [Encode](#encode-2)
        - [Smart encoding](#smart-encoding)
        - [Encoding with empty map as option](#encoding-with-empty-map-as-option)
        - [Encoding with all built-in plugins](#encoding-with-all-built-in-plugins)
    - [Decode](#decode-2)
        - [Smart decoding](#smart-decoding)
        - [Decoding with empty map as option](#decoding-with-empty-map-as-option)
        - [Decoding with all built-in plugins](#decoding-with-all-built-in-plugins)
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
{deps, [{euneus, "1.0.3"}]}
```

### Elixir

```elixir
# mix.exs
def deps do
  [{:euneus, "~> 1.0"}]
end
```

## Basic Usage

```erlang
1> {ok, JSON} = euneus:encode_to_binary(#{name => #{english => <<"Charmander">>, japanese => <<"ヒトカゲ"/utf8>>}, type => [fire], profile => #{height => 0.6, weight => 8}, ability => #{0 => <<"Blaze">>, 1 => undefined}}).
{ok, <<"{\"name\":{\"english\":\"Charmander\",\"japanese\":\"ヒトカゲ\"},\"profile\":{\"height\":0.6,\"weight\":8},\"type\":[\"fire\"],\"ability\":{\"0\":\"Blaze\",\"1\":null}}">>}

2> euneus:decode(JSON).
{ok,#{<<"ability">> =>
          #{<<"0">> => <<"Blaze">>,<<"1">> => undefined},
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
      ability => #{0 => <<"Blaze">>,1 => undefined}}}
```

## Data Mapping

> [!TIP]
>
> More types can be handled by using custom plugins. Please see the [Plugins](#plugins) section for more info.

<!-- Generated via https://www.tablesgenerator.com/markdown_tables -->
<!-- To edit, open "./assets/md-tables/data-mapping.tgn" in the link above. -->
| **Erlang ->**                                                	| **Encode Options ->**                                                                                                                    	| **JSON ->**                          	| **Decode Options ->**                                                                                           	| **Erlang**                                                   	|
|--------------------------------------------------------------	|------------------------------------------------------------------------------------------------------------------------------------------	|--------------------------------------	|-----------------------------------------------------------------------------------------------------------------	|--------------------------------------------------------------	|
| undefined                                                    	| #{}                                                                                                                                      	| null                                 	| #{}                                                                                                             	| undefined                                                    	|
| undefined                                                    	| #{}                                                                                                                                      	| null                                 	| #{null_term => nil}                                                                                             	| nil                                                          	|
| true                                                         	| #{}                                                                                                                                      	| true                                 	| #{}                                                                                                             	| true                                                         	|
| false                                                        	| #{}                                                                                                                                      	| false                                	| #{}                                                                                                             	| false                                                        	|
| abc                                                          	| #{}                                                                                                                                      	| "abc"                                	| #{}                                                                                                             	| <<"abc">>                                                    	|
| "abc"                                                        	| #{}                                                                                                                                      	| [97,98,99]                           	| #{}                                                                                                             	| "abc"                                                        	|
| <<"abc">>                                                    	| #{}                                                                                                                                      	| "abc"                                	| #{}                                                                                                             	| <<"abc">>                                                    	|
| 123                                                          	| #{}                                                                                                                                      	| 123                                  	| #{}                                                                                                             	| 123                                                          	|
| 123.45600                                                    	| #{}                                                                                                                                      	| 123.456                              	| #{}                                                                                                             	| 123.456                                                      	|
| [<<"foo">>,true,0,undefined]                                 	| #{}                                                                                                                                      	| ["foo",true,0,null]                  	| #{}                                                                                                             	| [<<"foo">>,true,0,undefined]                                 	|
| #{foo => bar}                                                	| #{}                                                                                                                                      	| {"foo":"bar"}                        	| #{}                                                                                                             	| #{<<"foo">> => <<"bar">>}                                    	|
| #{foo => bar}                                                	| #{}                                                                                                                                      	| {"foo":"bar"}                        	| #{keys => to_existing_atom}                                                                                     	| #{foo => <<"bar">>}                                          	|
| #{0 => 0}                                                    	| #{}                                                                                                                                      	| {"0":0}                              	| #{keys => to_integer}                                                                                           	| #{0 => 0}                                                    	|
| {{1970,1,1},{0,0,0}}                                         	| #{plugins => [datetime]}                                                                                                                 	| "1970-01-01T00:00:00Z"               	| #{plugins => [datetime]}                                                                                        	| {{1970,1,1},{0,0,0}}                                         	|
| {127,0,0,1}                                                  	| #{plugins => [inet]}                                                                                                                     	| "127.0.0.1"                          	| #{plugins => [inet]}                                                                                            	| {127,0,0,1}                                                  	|
| {16#3ffe,16#b80,16#1f8d,16#2,16#204,16#acff,16#fe17,16#bf38} 	| #{plugins => [inet]}                                                                                                                     	| "3ffe:b80:1f8d:2:204:acff:fe17:bf38" 	| #{plugins => [inet]}                                                                                            	| {16#3ffe,16#b80,16#1f8d,16#2,16#204,16#acff,16#fe17,16#bf38} 	|
| <0.92.0>                                                     	| #{plugins => [pid]}                                                                                                                      	| "<0.92.0>"                           	| #{plugins => [pid]}                                                                                             	| <0.92.0>                                                     	|
| #Port<0.1>                                                   	| #{plugins => [port]}                                                                                                                     	| "#Port<0.1>"                         	| #{plugins => [port]}                                                                                            	| #Port<0.1>                                                   	|
| [{foo, bar}]                                                 	| #{plugins => [proplist]}                                                                                                                 	| {\"foo\":\"bar\"}                    	| #{plugins => [proplist]}                                                                                        	| #{<<"foo">> => <<"bar">>}                                    	|
| #Ref<0.957048870.857473026.108035>                           	| #{plugins => [reference]}                                                                                                                	| "#Ref<0.957048870.857473026.108035>" 	| #{plugins => [reference]}                                                                                       	| #Ref<0.957048870.857473026.108035>                           	|
| {0,0,0}                                                      	| #{plugins => [timestamp]}                                                                                                                	| "1970-01-01T00:00:00.000Z"           	| #{plugins => [timestamp]}                                                                                       	| {0,0,0}                                                      	|
| {myrecord, val}                                              	| #{unhandled_encoder => fun({myrecord, Val}, Opts) ->    <br>    euneus_encoder:encode_list([myrecord, #{key => Val}], Opts)<br><br>end}) 	| ["myrecord", {"key":"val"}]          	| #{arrays => fun([<<"myrecord">>, #{<<"key">> := Val}], _Opts) -><br>    {myrecord, binary_to_atom(Val)}<br>end} 	| {myrecord, val}                                              	|

### Why not more built-in types?

The goal of `Euneus` is to have built-in types that can be commonly encoded and decoded, but the range of types can be easily extended by using plugins.  Please see the [Plugins](#plugins) section for more info.

### Note about proplists

Proplists are not handled by Euneus by default.

There are three options:
1. Use the built-in, or create your own, [proplist plugin](#proplist);
2. Convert proplists to maps before the encoding;
3. Override the `list_encoder` option in the encoder to handle them, for example:

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

## Plugins

Euneus has a mechanism to easily plug in encoders and decoders. You can use the [built-in plugins](#built-in-plugins) to handle common types or create your own in a module by implementing the [euneus_plugin](/src/euneus_plugin.erl) behavior.

If you have a built-in plugin suggestion, feel free to open a [new issue](https://github.com/williamthome/euneus/issues/new) to discuss it.

> [!IMPORTANT]
> The plugins mechanism deprecated the `datetime_encoder` and the `timestamp_encoder` option in favor of the `datetime` and `timestamp` plugins.

### Usage

#### Encode

```erlang
euneus:encode(Term, #{plugins => [
    % list of built-in or custom plugins
]})
```

#### Decode

```erlang
euneus:decode(Term, #{plugins => [
    % list of built-in or custom plugins
]})
```

### Built-in Plugins

#### datetime

Encodes `calendar:datetime()` to ISO8601 as JSON string and decodes it back, for example:

```erlang
1> {ok, JSON} = euneus:encode_to_binary({{1970,1,1},{0,0,0}}, #{plugins => [datetime]}).
{ok,<<"\"1970-01-01T00:00:00Z\"">>}

2> euneus:decode(JSON, #{plugins => [datetime]}).
{ok,{{1970,1,1},{0,0,0}}}
```

#### inet

Encodes `inet:ip_address()` to IPV4 or IPV6 as JSON string and decodes it back, for example:

```erlang
1> {ok, JSON} = euneus:encode_to_binary({127,0,0,1}, #{plugins => [inet]}).
{ok,<<"\"127.0.0.1\"">>}

2> euneus:decode(JSON, #{plugins => [inet]}).
{ok,{127,0,0,1}}
```

#### pid

Encodes `erlang:pid()` to JSON string and decodes it back, for example:

```erlang
1> {ok, JSON} = euneus:encode_to_binary(list_to_pid("<0.92.0>"), #{plugins => [pid]}).
{ok,<<"\"<0.92.0>\"">>}

2> euneus:decode(JSON, #{plugins => [pid]}).
{ok,<0.92.0>}
```

#### port

Encodes `erlang:port()` to JSON string and decodes it back, for example:

```erlang
1> {ok, JSON} = euneus:encode_to_binary(list_to_port("#Port<0.1>"), #{plugins => [port]}).
{ok,<<"\"#Port<0.1>\"">>}

2> euneus:decode(JSON, #{plugins => [port]}).
{ok,#Port<0.1>}
```

#### proplist

Encodes `[{binary() | atom() | integer(), term()}]` to JSON object, for example:

```erlang
1> {ok, JSON} = euneus:encode_to_binary([{foo, bar}], #{plugins => [proplist]}).
{ok,<<"{\"foo\":\"bar\"}">>}
```

#### reference

Encodes `erlang:reference()` to JSON string and decodes it back, for example:

```erlang
1> {ok, JSON} = euneus:encode_to_binary(make_ref(), #{plugins => [reference]}).
{ok,<<"\"#Ref<0.957048870.857473026.108035>\"">>}

2> euneus:decode(JSON, #{plugins => [reference]}).
{ok,#Ref<0.957048870.857473026.108035>}
```

#### timestamp

Encodes `erlang:timestamp()` to ISO8601 as JSON string and decodes it back, for example:

```erlang
1> {ok, JSON} = euneus:encode_to_binary({0,0,0}, #{plugins => [timestamp]}).
{ok,<<"\"1970-01-01T00:00:00.000Z\"">>}

2> euneus:decode(JSON, #{plugins => [timestamp]}).
{ok,{0,0,0}}
```

## Differences to Thoas

The main difference between `Euneus` to `Thoas` is that Euneus gives more control to encoding or decoding data. All encode functions can be overridden and extended and all decoded data can be overridden and transformed. Also, there is no plugin mechanism in Thoas.

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
    %% unhandled_encoder allow encode any custom term (default: raise unsupported_type error).
    unhandled_encoder => function((term(), euneus_encoder:options()) -> iolist()),
    %% escaper allow override the binary escaping (default: json).
    escaper => json
             | html
             | javascript
             | unicode
             | function((binary(), euneus_encoder:options()) -> iolist()),
    error_handler => function(( error | exit | throw
                              , term()
                              , erlang:stacktrace() ) -> euneus_encoder:result()),
    %% plugins extends the encode types.
    plugins => datetime
             | inet
             | pid
             | port
             | proplist
             | reference
             | timestamp
             | module() % that implements the `euneus_plugin` behavior.
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
            | function((binary(), euneus_decoder:options()) -> term()),
    %% plugins extends the decode types.
    plugins => datetime
             | inet
             | pid
             | port
             | reference
             | timestamp
             | module() % that implements the `euneus_plugin` behavior.
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

> [!NOTE]
>
> By using `euneus_decoder:resume/6` the replacement will be the `null_term` option.

### Why Euneus over Thoas?

`Thoas` is incredible, works performant and perfectly fine, but `Euneus` is more flexible, permitting more customizations, and is more performant than Thoas. See the [benchmarks](#benchmarks).

The motivation for Euneus is [this PR](https://github.com/lpil/thoas/pull/28).

## Benchmarks

All the latest runs details can be found under the [runs](./euneus_bench/runs/) directory in the [benchmark project](./euneus_bench/).

### Encode

#### Smart encoding

This benchmark uses the JSON smart module via the `euneus:encode/1` function. Smart modules only receive the input as the argument, so no option is available to set. Smart modules are the fastest euneus modules.

<div style="overflow-x: auto;">

| **Blockchain** 	|         	|            	|   	| **Giphy**  	|         	|            	|   	| **GitHub** 	|         	|            	|   	| **GovTrack** 	|         	|            	|   	| **Issue 90** 	|         	|            	|   	| **JSON Generator** 	|         	|            	|   	| **Pokedex** 	|         	|            	|   	| **UTF-8 unescaped** 	|         	|            	|
|----------------	|---------	|------------	|---	|------------	|---------	|------------	|---	|------------	|---------	|------------	|---	|--------------	|---------	|------------	|---	|--------------	|---------	|------------	|---	|--------------------	|---------	|------------	|---	|-------------	|---------	|------------	|---	|---------------------	|---------	|------------	|
| **Name**       	| **IPS** 	| **Slower** 	|   	| **Name**   	| **IPS** 	| **Slower** 	|   	| **Name**   	| **IPS** 	| **Slower** 	|   	| **Name**     	| **IPS** 	| **Slower** 	|   	| **Name**     	| **IPS** 	| **Slower** 	|   	| **Name**           	| **IPS** 	| **Slower** 	|   	| **Name**    	| **IPS** 	| **Slower** 	|   	| **Name**            	| **IPS** 	| **Slower** 	|
| jiffy          	| 11.14 K 	|            	|   	| jiffy      	| 1162.06 	|            	|   	| jiffy      	|  3.55 K 	|            	|   	| jiffy        	|   52.04 	|            	|   	| jiffy        	|   36.73 	|            	|   	| jiffy              	| 1191.94 	|            	|   	| jiffy       	| 1669.42 	|            	|   	| jiffy               	| 14.32 K 	|            	|
| Jason          	|  4.60 K 	|      2.42x 	|   	| Jason      	|  429.19 	|      2.71x 	|   	| Jason      	|  1.51 K 	|      2.35x 	|   	| Jason        	|   17.91 	|      2.91x 	|   	| Jason        	|   27.44 	|      1.34x 	|   	| **euneus**         	|  417.78 	|      2.85x 	|   	| **euneus**  	|  644.19 	|      2.59x 	|   	| Jason               	| 10.11 K 	|      1.42x 	|
| **euneus**     	|  4.49 K 	|      2.48x 	|   	| **euneus** 	|  427.23 	|      2.72x 	|   	| **euneus** 	|  1.45 K 	|      2.44x 	|   	| thoas        	|   16.56 	|      3.14x 	|   	| **euneus**   	|   23.24 	|      1.58x 	|   	| Jason              	|  411.69 	|       2.9x 	|   	| Jason       	|  628.42 	|      2.66x 	|   	| **euneus**          	|  9.40 K 	|      1.52x 	|
| thoas          	|  3.78 K 	|      2.94x 	|   	| thoas      	|  403.55 	|      2.88x 	|   	| thoas      	|  1.30 K 	|      2.72x 	|   	| **euneus**   	|   14.72 	|      3.53x 	|   	| thoas        	|   17.01 	|      2.16x 	|   	| thoas              	|  405.74 	|      2.94x 	|   	| thoas       	|  613.91 	|      2.72x 	|   	| thoas               	|  9.07 K 	|      1.58x 	|
| jsone          	|  2.54 K 	|      4.39x 	|   	| jsone      	|  213.42 	|      5.44x 	|   	| jsone      	|  0.67 K 	|      5.34x 	|   	| jsone        	|    8.58 	|      6.06x 	|   	| jsone        	|   16.65 	|      2.21x 	|   	| jsone              	|  273.00 	|      4.37x 	|   	| jsone       	|  322.22 	|      5.18x 	|   	| jsx                 	|  4.76 K 	|      3.01x 	|
| jsx            	|  1.00 K 	|     11.18x 	|   	| jsx        	|   78.73 	|     14.76x 	|   	| jsx        	| 0.197 K 	|     18.01x 	|   	| jsx          	|    3.07 	|     16.95x 	|   	| jsx          	|    8.75 	|       4.2x 	|   	| jsx                	|   97.47 	|     12.23x 	|   	| jsx         	|  108.10 	|     15.44x 	|   	| jsone               	|  1.83 K 	|      7.83x 	|

</div>

All benchmark details are available [here](./euneus_bench/runs/encode.md).

#### Encoding with empty map as option

This benchmark passes the input and options parsed as the `euneus:encode_parsed/2` function arguments. There is no option set, just an empty map, so all the default options are used. This function it's a bit slower than the smart one, but all options are analyzed in the run.

<div style="overflow-x: auto;">

| **Blockchain** 	|         	|            	|   	| **Giphy**  	|         	|            	|   	| **GitHub** 	|         	|            	|   	| **GovTrack** 	|         	|            	|   	| **Issue 90** 	|         	|            	|   	| **JSON Generator** 	|         	|            	|   	| **Pokedex** 	|         	|            	|   	| **UTF-8 unescaped** 	|         	|            	|
|----------------	|---------	|------------	|---	|------------	|---------	|------------	|---	|------------	|---------	|------------	|---	|--------------	|---------	|------------	|---	|--------------	|---------	|------------	|---	|--------------------	|---------	|------------	|---	|-------------	|---------	|------------	|---	|---------------------	|---------	|------------	|
| **Name**       	| **IPS** 	| **Slower** 	|   	| **Name**   	| **IPS** 	| **Slower** 	|   	| **Name**   	| **IPS** 	| **Slower** 	|   	| **Name**     	| **IPS** 	| **Slower** 	|   	| **Name**     	| **IPS** 	| **Slower** 	|   	| **Name**           	| **IPS** 	| **Slower** 	|   	| **Name**    	| **IPS** 	| **Slower** 	|   	| **Name**            	| **IPS** 	| **Slower** 	|
| jiffy          	| 11.13 K 	|            	|   	| jiffy      	| 1163.05 	|            	|   	| jiffy      	|  3.50 K 	|            	|   	| jiffy        	|   51.97 	|            	|   	| jiffy        	|   36.68 	|            	|   	| jiffy              	| 1177.53 	|            	|   	| jiffy       	| 1659.13 	|            	|   	| jiffy               	| 14.36 K 	|            	|
| Jason          	|  4.62 K 	|      2.41x 	|   	| Jason      	|  416.91 	|      2.79x 	|   	| Jason      	|  1.50 K 	|      2.34x 	|   	| Jason        	|   17.96 	|      2.89x 	|   	| Jason        	|   27.35 	|      1.34x 	|   	| **euneus**         	|  409.80 	|      2.87x 	|   	| Jason       	|  620.01 	|      2.68x 	|   	| Jason               	| 10.07 K 	|      1.43x 	|
| **euneus**     	|  3.92 K 	|      2.84x 	|   	| thoas      	|  396.88 	|      2.93x 	|   	| thoas      	|  1.28 K 	|      2.74x 	|   	| **euneus**   	|   17.40 	|      2.99x 	|   	| **euneus**   	|   24.04 	|      1.53x 	|   	| Jason              	|  406.64 	|       2.9x 	|   	| thoas       	|  609.24 	|      2.72x 	|   	| **euneus**          	|  9.27 K 	|      1.55x 	|
| thoas          	|  3.74 K 	|      2.98x 	|   	| **euneus** 	|  335.54 	|      3.47x 	|   	| **euneus** 	|  1.25 K 	|      2.81x 	|   	| thoas        	|   16.46 	|      3.16x 	|   	| thoas        	|   16.94 	|      2.17x 	|   	| thoas              	|  399.93 	|      2.94x 	|   	| **euneus**  	|  487.64 	|       3.4x 	|   	| thoas               	|  9.07 K 	|      1.58x 	|
| jsone          	|  2.55 K 	|      4.37x 	|   	| jsone      	|  212.68 	|      5.47x 	|   	| jsone      	|  0.66 K 	|      5.31x 	|   	| jsone        	|    8.54 	|      6.09x 	|   	| jsone        	|   16.68 	|       2.2x 	|   	| jsone              	|  270.72 	|      4.35x 	|   	| jsone       	|  321.06 	|      5.17x 	|   	| jsx                 	|  4.78 K 	|       3.0x 	|
| jsx            	|  1.01 K 	|     11.03x 	|   	| jsx        	|   79.30 	|     14.67x 	|   	| jsx        	| 0.199 K 	|     17.63x 	|   	| jsx          	|    3.08 	|      16.9x 	|   	| jsx          	|    8.76 	|      4.19x 	|   	| jsx                	|   97.89 	|     12.03x 	|   	| jsx         	|  108.35 	|     15.31x 	|   	| jsone               	|  1.83 K 	|      7.86x 	|

</div>

All benchmark details are available [here](./euneus_bench/runs/encode_parsed.md).

#### Encoding with all built-in plugins

This benchmark passes all the encode built-in plugins to the plugins option:

```erlang
euneus:parse_encode_opts(#{
  plugins => [
    datetime,
    inet,
    pid,
    port,
    proplist,
    reference,
    timestamp
  ]
}).
```

It's the slowest euneus encode run, but at the same time it is very efficient.

<div style="overflow-x: auto;">

| **Blockchain** 	|         	|            	|   	| **Giphy**  	|         	|            	|   	| **GitHub** 	|         	|            	|   	| **GovTrack** 	|         	|            	|   	| **Issue 90** 	|         	|            	|   	| **JSON Generator** 	|         	|            	|   	| **Pokedex** 	|         	|            	|   	| **UTF-8 unescaped** 	|         	|            	|
|----------------	|---------	|------------	|---	|------------	|---------	|------------	|---	|------------	|---------	|------------	|---	|--------------	|---------	|------------	|---	|--------------	|---------	|------------	|---	|--------------------	|---------	|------------	|---	|-------------	|---------	|------------	|---	|---------------------	|---------	|------------	|
| **Name**       	| **IPS** 	| **Slower** 	|   	| **Name**   	| **IPS** 	| **Slower** 	|   	| **Name**   	| **IPS** 	| **Slower** 	|   	| **Name**     	| **IPS** 	| **Slower** 	|   	| **Name**     	| **IPS** 	| **Slower** 	|   	| **Name**           	| **IPS** 	| **Slower** 	|   	| **Name**    	| **IPS** 	| **Slower** 	|   	| **Name**            	| **IPS** 	| **Slower** 	|
| jiffy          	| 11.28 K 	|            	|   	| jiffy      	| 1172.88 	|            	|   	| jiffy      	|  3.26 K 	|            	|   	| jiffy        	|   52.16 	|            	|   	| jiffy        	|   36.70 	|            	|   	| jiffy              	| 1188.50 	|            	|   	| jiffy       	| 1648.51 	|            	|   	| jiffy               	| 14.38 K 	|            	|
| Jason          	|  4.59 K 	|      2.46x 	|   	| Jason      	|  424.49 	|      2.76x 	|   	| Jason      	|  1.52 K 	|      2.14x 	|   	| Jason        	|   18.21 	|      2.86x 	|   	| Jason        	|   27.43 	|      1.34x 	|   	| Jason              	|  412.20 	|      2.88x 	|   	| Jason       	|  628.07 	|      2.62x 	|   	| Jason               	| 10.09 K 	|      1.42x 	|
| **euneus**     	|  3.79 K 	|      2.97x 	|   	| thoas      	|  402.26 	|      2.92x 	|   	| thoas      	|  1.30 K 	|      2.51x 	|   	| thoas        	|   16.95 	|      3.08x 	|   	| **euneus**   	|   24.12 	|      1.52x 	|   	| thoas              	|  405.50 	|      2.93x 	|   	| thoas       	|  613.82 	|      2.69x 	|   	| **euneus**          	|  9.41 K 	|      1.53x 	|
| thoas          	|  3.79 K 	|      2.98x 	|   	| **euneus** 	|  332.49 	|      3.53x 	|   	| **euneus** 	|  1.22 K 	|      2.67x 	|   	| **euneus**   	|   16.66 	|      3.13x 	|   	| thoas        	|   16.98 	|      2.16x 	|   	| **euneus**         	|  397.61 	|      2.99x 	|   	| **euneus**  	|  470.18 	|      3.51x 	|   	| thoas               	|  9.13 K 	|      1.57x 	|
| jsone          	|  2.54 K 	|      4.44x 	|   	| jsone      	|  213.21 	|       5.5x 	|   	| jsone      	|  0.66 K 	|      4.93x 	|   	| jsone        	|    8.55 	|       6.1x 	|   	| jsone        	|   16.66 	|       2.2x 	|   	| jsone              	|  272.44 	|      4.36x 	|   	| jsone       	|  321.84 	|      5.12x 	|   	| jsx                 	|  4.76 K 	|      3.02x 	|
| jsx            	|  1.01 K 	|      11.2x 	|   	| jsx        	|   79.30 	|     14.79x 	|   	| jsx        	| 0.196 K 	|     16.62x 	|   	| jsx          	|    3.10 	|     16.83x 	|   	| jsx          	|    8.78 	|      4.18x 	|   	| jsx                	|   94.44 	|     12.59x 	|   	| jsx         	|  108.45 	|      15.2x 	|   	| jsone               	|  1.79 K 	|      8.02x 	|

</div>

All benchmark details are available [here](./euneus_bench/runs/encode_parsed_plugins.md).

### Decode

#### Smart decoding

This benchmark uses the decode smart module via the `euneus:decode/1` function. Smart modules only receive the input as the argument, so no option is available to set. Smart modules are the fastest euneus modules.

<div style="overflow-x: auto;">

| **Blockchain** 	|         	|            	|   	| **Giphy**  	|         	|            	|   	| **GitHub** 	|         	|            	|   	| **GovTrack** 	|         	|            	|   	| **Issue 90** 	|         	|            	|   	| **JSON Generator (Pretty)** 	|         	|            	|   	| **JSON Generator** 	|         	|            	|   	| **Pokedex** 	|         	|            	|   	| **UTF-8 escaped** 	|         	|            	|   	| **UTF-8 unescaped** 	|         	|            	|
|----------------	|---------	|------------	|---	|------------	|---------	|------------	|---	|------------	|---------	|------------	|---	|--------------	|---------	|------------	|---	|--------------	|---------	|------------	|---	|-----------------------------	|---------	|------------	|---	|--------------------	|---------	|------------	|---	|-------------	|---------	|------------	|---	|-------------------	|---------	|------------	|---	|---------------------	|---------	|------------	|
| **Name**       	| **IPS** 	| **Slower** 	|   	| **Name**   	| **IPS** 	| **Slower** 	|   	| **Name**   	| **IPS** 	| **Slower** 	|   	| **Name**     	| **IPS** 	| **Slower** 	|   	| **Name**     	| **IPS** 	| **Slower** 	|   	| **Name**                    	| **IPS** 	| **Slower** 	|   	| **Name**           	| **IPS** 	| **Slower** 	|   	| **Name**    	| **IPS** 	| **Slower** 	|   	| **Name**          	| **IPS** 	| **Slower** 	|   	| **Name**            	| **IPS** 	| **Slower** 	|
| jiffy          	|  5.94 K 	|            	|   	| jiffy      	|  925.19 	|            	|   	| jiffy      	|  2.69 K 	|            	|   	| jiffy        	|   27.04 	|            	|   	| jiffy        	|   49.92 	|            	|   	| jiffy                       	| 1143.30 	|            	|   	| jiffy              	|  698.94 	|            	|   	| jiffy       	|  1.36 K 	|            	|   	| jiffy             	| 10.41 K 	|            	|   	| jiffy               	| 18.12 K 	|            	|
| **euneus**     	|  5.77 K 	|      1.03x 	|   	| thoas      	|  498.15 	|      1.86x 	|   	| **euneus** 	|  2.09 K 	|      1.29x 	|   	| Jason        	|   17.02 	|      1.59x 	|   	| **euneus**   	|   26.62 	|      1.88x 	|   	| **euneus**                  	|  707.45 	|      1.62x 	|   	| **euneus**         	|  616.18 	|      1.13x 	|   	| **euneus**  	|  1.17 K 	|      1.16x 	|   	| thoas             	|  1.70 K 	|      6.14x 	|   	| **euneus**          	| 10.44 K 	|      1.74x 	|
| Jason          	|  5.53 K 	|      1.08x 	|   	| **euneus** 	|  494.10 	|      1.87x 	|   	| Jason      	|  2.01 K 	|      1.34x 	|   	| **euneus**   	|   16.79 	|      1.61x 	|   	| Jason        	|   25.25 	|      1.98x 	|   	| Jason                       	|  696.88 	|      1.64x 	|   	| Jason              	|  591.72 	|      1.18x 	|   	| thoas       	|  1.15 K 	|      1.18x 	|   	| Jason             	|  1.69 K 	|      6.18x 	|   	| Jason               	| 10.19 K 	|      1.78x 	|
| thoas          	|  4.87 K 	|      1.22x 	|   	| Jason      	|  451.81 	|      2.05x 	|   	| thoas      	|  1.81 K 	|      1.49x 	|   	| thoas        	|   15.69 	|      1.72x 	|   	| jsone        	|   23.25 	|      2.15x 	|   	| thoas                       	|  624.07 	|      1.83x 	|   	| thoas              	|  539.94 	|      1.29x 	|   	| Jason       	|  1.04 K 	|       1.3x 	|   	| **euneus**        	|  1.47 K 	|      7.09x 	|   	| thoas               	|  9.60 K 	|      1.89x 	|
| jsone          	|  4.26 K 	|      1.39x 	|   	| jsone      	|  278.02 	|      3.33x 	|   	| jsone      	|  1.42 K 	|       1.9x 	|   	| jsone        	|    9.35 	|      2.89x 	|   	| thoas        	|   17.79 	|      2.81x 	|   	| jsone                       	|  435.82 	|      2.62x 	|   	| jsone              	|  400.04 	|      1.75x 	|   	| jsone       	|  0.59 K 	|      2.31x 	|   	| jsone             	|  1.29 K 	|      8.09x 	|   	| jsone               	|  9.44 K 	|      1.92x 	|
| jsx            	|  1.83 K 	|      3.25x 	|   	| jsx        	|  162.97 	|      5.68x 	|   	| jsx        	|  0.51 K 	|      5.24x 	|   	| jsx          	|    4.40 	|      6.14x 	|   	| jsx          	|    9.92 	|      5.03x 	|   	| jsx                         	|  185.81 	|      6.15x 	|   	| jsx                	|  176.01 	|      3.97x 	|   	| jsx         	|  0.26 K 	|      5.31x 	|   	| jsx               	|  1.16 K 	|      8.97x 	|   	| jsx                 	|  6.57 K 	|      2.76x 	|

</div>

All benchmark details are available [here](./euneus_bench/runs/decode.md).

#### Decoding with empty map as option

This benchmark passes the input and options parsed as the `euneus:decode_parsed/2` function arguments. There is no option set, just an empty map, so all the default options are used. This function it's a bit slower than the smart one, but all options are analyzed in the run.

<div style="overflow-x: auto;">

| **Blockchain** 	|         	|            	|   	| **Giphy**  	|         	|            	|   	| **GitHub** 	|         	|            	|   	| **GovTrack** 	|         	|            	|   	| **Issue 90** 	|         	|            	|   	| **JSON Generator (Pretty)** 	|         	|            	|   	| **JSON Generator** 	|         	|            	|   	| **Pokedex** 	|         	|            	|   	| **UTF-8 escaped** 	|         	|            	|   	| **UTF-8 unescaped** 	|         	|            	|
|----------------	|---------	|------------	|---	|------------	|---------	|------------	|---	|------------	|---------	|------------	|---	|--------------	|---------	|------------	|---	|--------------	|---------	|------------	|---	|-----------------------------	|---------	|------------	|---	|--------------------	|---------	|------------	|---	|-------------	|---------	|------------	|---	|-------------------	|---------	|------------	|---	|---------------------	|---------	|------------	|
| **Name**       	| **IPS** 	| **Slower** 	|   	| **Name**   	| **IPS** 	| **Slower** 	|   	| **Name**   	| **IPS** 	| **Slower** 	|   	| **Name**     	| **IPS** 	| **Slower** 	|   	| **Name**     	| **IPS** 	| **Slower** 	|   	| **Name**                    	| **IPS** 	| **Slower** 	|   	| **Name**           	| **IPS** 	| **Slower** 	|   	| **Name**    	| **IPS** 	| **Slower** 	|   	| **Name**          	| **IPS** 	| **Slower** 	|   	| **Name**            	| **IPS** 	| **Slower** 	|
| jiffy          	|  5.95 K 	|            	|   	| jiffy      	|  969.77 	|            	|   	| jiffy      	|  2.68 K 	|            	|   	| jiffy        	|   27.26 	|            	|   	| jiffy        	|   49.90 	|            	|   	| jiffy                       	| 1121.63 	|            	|   	| jiffy              	|  690.56 	|            	|   	| jiffy       	|  1.31 K 	|            	|   	| jiffy             	| 10.41 K 	|            	|   	| jiffy               	| 18.10 K 	|            	|
| Jason          	|  5.58 K 	|      1.07x 	|   	| **euneus** 	|  498.92 	|      1.94x 	|   	| Jason      	|  2.00 K 	|      1.34x 	|   	| Jason        	|   16.96 	|      1.61x 	|   	| Jason        	|   25.29 	|      1.97x 	|   	| **euneus**                  	|  693.50 	|      1.62x 	|   	| Jason              	|  589.37 	|      1.17x 	|   	| thoas       	|  1.16 K 	|      1.13x 	|   	| thoas             	|  1.72 K 	|      6.06x 	|   	| **euneus**          	| 10.22 K 	|      1.77x 	|
| **euneus**     	|  5.44 K 	|      1.09x 	|   	| thoas      	|  496.53 	|      1.95x 	|   	| **euneus** 	|  1.94 K 	|      1.38x 	|   	| **euneus**   	|   15.93 	|      1.71x 	|   	| **euneus**   	|   24.88 	|      2.01x 	|   	| Jason                       	|  692.92 	|      1.62x 	|   	| **euneus**         	|  560.86 	|      1.23x 	|   	| **euneus**  	|  1.08 K 	|      1.21x 	|   	| Jason             	|  1.71 K 	|      6.09x 	|   	| Jason               	| 10.10 K 	|      1.79x 	|
| thoas          	|  4.89 K 	|      1.22x 	|   	| Jason      	|  462.17 	|       2.1x 	|   	| thoas      	|  1.80 K 	|      1.49x 	|   	| thoas        	|   15.71 	|      1.74x 	|   	| jsone        	|   23.14 	|      2.16x 	|   	| thoas                       	|  627.27 	|      1.79x 	|   	| thoas              	|  544.72 	|      1.27x 	|   	| Jason       	|  1.04 K 	|      1.26x 	|   	| **euneus**        	|  1.44 K 	|      7.25x 	|   	| thoas               	|  9.62 K 	|      1.88x 	|
| jsone          	|  3.95 K 	|      1.51x 	|   	| jsone      	|  255.41 	|       3.8x 	|   	| jsone      	|  1.29 K 	|      2.07x 	|   	| jsone        	|    8.91 	|      3.06x 	|   	| thoas        	|   17.76 	|      2.81x 	|   	| jsone                       	|  401.43 	|      2.79x 	|   	| jsone              	|  370.06 	|      1.87x 	|   	| jsone       	|  0.53 K 	|      2.46x 	|   	| jsone             	|  1.29 K 	|      8.07x 	|   	| jsone               	|  9.42 K 	|      1.92x 	|
| jsx            	|  1.79 K 	|      3.33x 	|   	| jsx        	|  162.97 	|      5.95x 	|   	| jsx        	|  0.51 K 	|      5.28x 	|   	| jsx          	|    4.34 	|      6.29x 	|   	| jsx          	|   10.07 	|      4.95x 	|   	| jsx                         	|  186.60 	|      6.01x 	|   	| jsx                	|  176.95 	|       3.9x 	|   	| jsx         	|  0.25 K 	|      5.26x 	|   	| jsx               	|  1.18 K 	|      8.85x 	|   	| jsx                 	|  6.57 K 	|      2.75x 	|

</div>

All benchmark details are available [here](./euneus_bench/runs/decode_parsed.md).

#### Decoding with all built-in plugins

This benchmark passes all the decode built-in plugins to the plugins option:

```erlang
euneus:parse_decode_opts(#{
  plugins => [
    datetime,
    timestamp,
    pid,
    port,
    reference,
    inet
  ]
}).
```

> [!NOTE]
>
> The `proplist` plugin is only available for encoding.

It's the slowest euneus decode run, but at the same time it is very efficient.

<div style="overflow-x: auto;">

| **Blockchain** 	|         	|            	|   	| **Giphy**  	|         	|            	|   	| **GitHub** 	|         	|            	|   	| **GovTrack** 	|         	|            	|   	| **Issue 90** 	|         	|            	|   	| **JSON Generator (Pretty)** 	|         	|            	|   	| **JSON Generator** 	|         	|            	|   	| **Pokedex** 	|         	|            	|   	| **UTF-8 escaped** 	|         	|            	|   	| **UTF-8 unescaped** 	|         	|            	|
|----------------	|---------	|------------	|---	|------------	|---------	|------------	|---	|------------	|---------	|------------	|---	|--------------	|---------	|------------	|---	|--------------	|---------	|------------	|---	|-----------------------------	|---------	|------------	|---	|--------------------	|---------	|------------	|---	|-------------	|---------	|------------	|---	|-------------------	|---------	|------------	|---	|---------------------	|---------	|------------	|
| **Name**       	| **IPS** 	| **Slower** 	|   	| **Name**   	| **IPS** 	| **Slower** 	|   	| **Name**   	| **IPS** 	| **Slower** 	|   	| **Name**     	| **IPS** 	| **Slower** 	|   	| **Name**     	| **IPS** 	| **Slower** 	|   	| **Name**                    	| **IPS** 	| **Slower** 	|   	| **Name**           	| **IPS** 	| **Slower** 	|   	| **Name**    	| **IPS** 	| **Slower** 	|   	| **Name**          	| **IPS** 	| **Slower** 	|   	| **Name**            	| **IPS** 	| **Slower** 	|
| jiffy          	|  5.98 K 	|            	|   	| jiffy      	|  990.34 	|            	|   	| jiffy      	|  2.70 K 	|            	|   	| jiffy        	|   27.25 	|            	|   	| jiffy        	|   49.74 	|            	|   	| jiffy                       	| 1162.25 	|            	|   	| jiffy              	|  696.27 	|            	|   	| jiffy       	|  1.35 K 	|            	|   	| jiffy             	| 10.38 K 	|            	|   	| jiffy               	| 18.07 K 	|            	|
| Jason          	|  5.55 K 	|      1.08x 	|   	| thoas      	|  497.98 	|      1.99x 	|   	| Jason      	|  2.01 K 	|      1.35x 	|   	| Jason        	|   17.27 	|      1.58x 	|   	| Jason        	|   25.26 	|      1.97x 	|   	| Jason                       	|  694.53 	|      1.67x 	|   	| Jason              	|  586.85 	|      1.19x 	|   	| thoas       	|  1.15 K 	|      1.18x 	|   	| Jason             	|  1.70 K 	|      6.09x 	|   	| **euneus**          	| 10.37 K 	|      1.74x 	|
| thoas          	|  4.87 K 	|      1.23x 	|   	| Jason      	|  456.78 	|      2.17x 	|   	| thoas      	|  1.82 K 	|      1.49x 	|   	| thoas        	|   15.78 	|      1.73x 	|   	| **euneus**   	|   25.23 	|      1.97x 	|   	| thoas                       	|  625.05 	|      1.86x 	|   	| thoas              	|  541.03 	|      1.29x 	|   	| Jason       	|  1.04 K 	|       1.3x 	|   	| thoas             	|  1.70 K 	|      6.11x 	|   	| Jason               	| 10.20 K 	|      1.77x 	|
| jsone          	|  4.28 K 	|       1.4x 	|   	| **euneus** 	|  278.42 	|      3.56x 	|   	| jsone      	|  1.42 K 	|      1.91x 	|   	| jsone        	|    9.55 	|      2.85x 	|   	| jsone        	|   23.29 	|      2.14x 	|   	| jsone                       	|  436.44 	|      2.66x 	|   	| jsone              	|  400.67 	|      1.74x 	|   	| jsone       	|  0.59 K 	|      2.29x 	|   	| **euneus**        	|  1.45 K 	|      7.18x 	|   	| thoas               	|  9.59 K 	|      1.89x 	|
| **euneus**     	|  3.59 K 	|      1.67x 	|   	| jsone      	|  277.71 	|      3.57x 	|   	| **euneus** 	|  1.17 K 	|       2.3x 	|   	| **euneus**   	|    7.82 	|      3.49x 	|   	| thoas        	|   17.98 	|      2.77x 	|   	| **euneus**                  	|  347.69 	|      3.34x 	|   	| **euneus**         	|  317.16 	|       2.2x 	|   	| **euneus**  	|  0.40 K 	|      3.34x 	|   	| jsone             	|  1.30 K 	|      7.97x 	|   	| jsone               	|  9.51 K 	|       1.9x 	|
| jsx            	|  1.83 K 	|      3.26x 	|   	| jsx        	|  167.26 	|      5.92x 	|   	| jsx        	|  0.52 K 	|       5.2x 	|   	| jsx          	|    4.45 	|      6.13x 	|   	| jsx          	|    9.97 	|      4.99x 	|   	| jsx                         	|  191.09 	|      6.08x 	|   	| jsx                	|  181.31 	|      3.84x 	|   	| jsx         	|  0.26 K 	|      5.27x 	|   	| jsx               	|  1.18 K 	|      8.83x 	|   	| jsx                 	|  6.58 K 	|      2.75x 	|

</div>

All benchmark details are available [here](./euneus_bench/runs/decode_parsed_plugins.md).

## Tests

There are Eunit tests in [euneus_encoder](/src/euneus_encoder.erl) and [euneus_decoder](/src/euneus_decoder.erl) and tests suites in a specific project under the [euneus_test](/euneus_test/) directory. Euneus has more than 330 tests.

Also, the parser is tested using [JSONTestSuite](https://github.com/nst/JSONTestSuite) and all tests passes:

![JSON Test Suite](/assets/images/json-test-suite-result.png)

See the [Euneus parser](https://github.com/nst/JSONTestSuite/tree/master/parsers/test_erlang_euneus) in JSONTestSuite.

> [!NOTE]
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

> [!NOTE]
>
> Open the [Makefile](Makefile) to see all commands.

## License

Euneus is released under the [Apache License 2.0](LICENSE.md).

Euneus is based on [Thoas][thoas], which is also Apache 2.0 licensed.

Some elements have their origins in the [Poison library][poison] and were initially licensed under [CC0-1.0](https://creativecommons.org/publicdomain/zero/1.0/).

[jason]: https://github.com/michalmuskala/jason
[thoas]: https://github.com/lpil/thoas
[poison]: https://github.com/devinus/poison
