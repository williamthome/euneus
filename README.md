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
{deps, [{euneus, "1.0.2"}]}
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

<div style="overflow: auto;">

<div style="display: grid; grid-auto-flow: column; column-gap: 5px;">

<table style="width: 1%">
  <caption><b>Blockchain</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">11.14 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">4.60 K</td>
    <td style="white-space: nowrap; text-align: right">2.42x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">4.49 K</td>
    <td style="white-space: nowrap; text-align: right">2.48x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">3.78 K</td>
    <td style="white-space: nowrap; text-align: right">2.94x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">2.54 K</td>
    <td style="white-space: nowrap; text-align: right">4.39x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.00 K</td>
    <td style="white-space: nowrap; text-align: right">11.18x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Giphy</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1162.06</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">429.19</td>
    <td style="white-space: nowrap; text-align: right">2.71x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">427.23</td>
    <td style="white-space: nowrap; text-align: right">2.72x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">403.55</td>
    <td style="white-space: nowrap; text-align: right">2.88x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">213.42</td>
    <td style="white-space: nowrap; text-align: right">5.44x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">78.73</td>
    <td style="white-space: nowrap; text-align: right">14.76x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>GitHub</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">3.55 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.51 K</td>
    <td style="white-space: nowrap; text-align: right">2.35x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.45 K</td>
    <td style="white-space: nowrap; text-align: right">2.44x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.30 K</td>
    <td style="white-space: nowrap; text-align: right">2.72x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.67 K</td>
    <td style="white-space: nowrap; text-align: right">5.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.197 K</td>
    <td style="white-space: nowrap; text-align: right">18.01x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>GovTrack</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">52.04</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.91</td>
    <td style="white-space: nowrap; text-align: right">2.91x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.56</td>
    <td style="white-space: nowrap; text-align: right">3.14x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">14.72</td>
    <td style="white-space: nowrap; text-align: right">3.53x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">8.58</td>
    <td style="white-space: nowrap; text-align: right">6.06x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">3.07</td>
    <td style="white-space: nowrap; text-align: right">16.95x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Issue 90</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">36.73</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">27.44</td>
    <td style="white-space: nowrap; text-align: right">1.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">23.24</td>
    <td style="white-space: nowrap; text-align: right">1.58x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.01</td>
    <td style="white-space: nowrap; text-align: right">2.16x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">16.65</td>
    <td style="white-space: nowrap; text-align: right">2.21x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">8.75</td>
    <td style="white-space: nowrap; text-align: right">4.2x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>JSON Generator</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1191.94</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">417.78</td>
    <td style="white-space: nowrap; text-align: right">2.85x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">411.69</td>
    <td style="white-space: nowrap; text-align: right">2.9x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">405.74</td>
    <td style="white-space: nowrap; text-align: right">2.94x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">273.00</td>
    <td style="white-space: nowrap; text-align: right">4.37x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">97.47</td>
    <td style="white-space: nowrap; text-align: right">12.23x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Pokedex</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1669.42</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">644.19</td>
    <td style="white-space: nowrap; text-align: right">2.59x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">628.42</td>
    <td style="white-space: nowrap; text-align: right">2.66x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">613.91</td>
    <td style="white-space: nowrap; text-align: right">2.72x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">322.22</td>
    <td style="white-space: nowrap; text-align: right">5.18x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">108.10</td>
    <td style="white-space: nowrap; text-align: right">15.44x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>UTF-8 unescaped</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">14.32 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.11 K</td>
    <td style="white-space: nowrap; text-align: right">1.42x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">9.40 K</td>
    <td style="white-space: nowrap; text-align: right">1.52x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.07 K</td>
    <td style="white-space: nowrap; text-align: right">1.58x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.76 K</td>
    <td style="white-space: nowrap; text-align: right">3.01x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">7.83x</td>
  </tr>

</table>

</div>

</div>

All benchmark details are available [here](./euneus_bench/runs/encode.md).

#### Encoding with empty map as option

This benchmark passes the input and options parsed as the `euneus:encode_parsed/2` function arguments. There is no option set, just an empty map, so all the default options are used. This function it's a bit slower than the smart one, but all options are analyzed in the run.

<div style="overflow: auto;">

<div style="display: grid; grid-auto-flow: column; column-gap: 5px;">

<table style="width: 1%">
  <caption><b>Blockchain</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">11.13 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">4.62 K</td>
    <td style="white-space: nowrap; text-align: right">2.41x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.92 K</td>
    <td style="white-space: nowrap; text-align: right">2.84x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">3.74 K</td>
    <td style="white-space: nowrap; text-align: right">2.98x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">2.55 K</td>
    <td style="white-space: nowrap; text-align: right">4.37x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.01 K</td>
    <td style="white-space: nowrap; text-align: right">11.03x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Giphy</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1163.05</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">416.91</td>
    <td style="white-space: nowrap; text-align: right">2.79x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">396.88</td>
    <td style="white-space: nowrap; text-align: right">2.93x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">335.54</td>
    <td style="white-space: nowrap; text-align: right">3.47x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">212.68</td>
    <td style="white-space: nowrap; text-align: right">5.47x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">79.30</td>
    <td style="white-space: nowrap; text-align: right">14.67x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>GitHub</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">3.50 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.50 K</td>
    <td style="white-space: nowrap; text-align: right">2.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.28 K</td>
    <td style="white-space: nowrap; text-align: right">2.74x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.25 K</td>
    <td style="white-space: nowrap; text-align: right">2.81x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.66 K</td>
    <td style="white-space: nowrap; text-align: right">5.31x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.199 K</td>
    <td style="white-space: nowrap; text-align: right">17.63x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>GovTrack</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">51.97</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.96</td>
    <td style="white-space: nowrap; text-align: right">2.89x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">17.40</td>
    <td style="white-space: nowrap; text-align: right">2.99x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.46</td>
    <td style="white-space: nowrap; text-align: right">3.16x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">8.54</td>
    <td style="white-space: nowrap; text-align: right">6.09x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">3.08</td>
    <td style="white-space: nowrap; text-align: right">16.9x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Issue 90</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">36.68</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">27.35</td>
    <td style="white-space: nowrap; text-align: right">1.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">24.04</td>
    <td style="white-space: nowrap; text-align: right">1.53x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.94</td>
    <td style="white-space: nowrap; text-align: right">2.17x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">16.68</td>
    <td style="white-space: nowrap; text-align: right">2.2x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">8.76</td>
    <td style="white-space: nowrap; text-align: right">4.19x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>JSON Generator</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1177.53</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">409.80</td>
    <td style="white-space: nowrap; text-align: right">2.87x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">406.64</td>
    <td style="white-space: nowrap; text-align: right">2.9x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">399.93</td>
    <td style="white-space: nowrap; text-align: right">2.94x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">270.72</td>
    <td style="white-space: nowrap; text-align: right">4.35x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">97.89</td>
    <td style="white-space: nowrap; text-align: right">12.03x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Pokedex</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1659.13</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">620.01</td>
    <td style="white-space: nowrap; text-align: right">2.68x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">609.24</td>
    <td style="white-space: nowrap; text-align: right">2.72x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">487.64</td>
    <td style="white-space: nowrap; text-align: right">3.4x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">321.06</td>
    <td style="white-space: nowrap; text-align: right">5.17x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">108.35</td>
    <td style="white-space: nowrap; text-align: right">15.31x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>UTF-8 unescaped</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">14.36 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.07 K</td>
    <td style="white-space: nowrap; text-align: right">1.43x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">9.27 K</td>
    <td style="white-space: nowrap; text-align: right">1.55x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.07 K</td>
    <td style="white-space: nowrap; text-align: right">1.58x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.78 K</td>
    <td style="white-space: nowrap; text-align: right">3.0x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">7.86x</td>
  </tr>

</table>

</div>

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

<div style="overflow: auto;">

<div style="display: grid; grid-auto-flow: column; column-gap: 5px;">

<table style="width: 1%">
  <caption><b>Blockchain</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">11.28 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">4.59 K</td>
    <td style="white-space: nowrap; text-align: right">2.46x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.79 K</td>
    <td style="white-space: nowrap; text-align: right">2.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">3.79 K</td>
    <td style="white-space: nowrap; text-align: right">2.98x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">2.54 K</td>
    <td style="white-space: nowrap; text-align: right">4.44x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.01 K</td>
    <td style="white-space: nowrap; text-align: right">11.2x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Giphy</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1172.88</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">424.49</td>
    <td style="white-space: nowrap; text-align: right">2.76x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">402.26</td>
    <td style="white-space: nowrap; text-align: right">2.92x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">332.49</td>
    <td style="white-space: nowrap; text-align: right">3.53x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">213.21</td>
    <td style="white-space: nowrap; text-align: right">5.5x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">79.30</td>
    <td style="white-space: nowrap; text-align: right">14.79x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>GitHub</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">3.26 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.52 K</td>
    <td style="white-space: nowrap; text-align: right">2.14x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.30 K</td>
    <td style="white-space: nowrap; text-align: right">2.51x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.22 K</td>
    <td style="white-space: nowrap; text-align: right">2.67x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.66 K</td>
    <td style="white-space: nowrap; text-align: right">4.93x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.196 K</td>
    <td style="white-space: nowrap; text-align: right">16.62x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>GovTrack</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">52.16</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">18.21</td>
    <td style="white-space: nowrap; text-align: right">2.86x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.95</td>
    <td style="white-space: nowrap; text-align: right">3.08x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">16.66</td>
    <td style="white-space: nowrap; text-align: right">3.13x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">8.55</td>
    <td style="white-space: nowrap; text-align: right">6.1x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">3.10</td>
    <td style="white-space: nowrap; text-align: right">16.83x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Issue 90</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">36.70</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">27.43</td>
    <td style="white-space: nowrap; text-align: right">1.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">24.12</td>
    <td style="white-space: nowrap; text-align: right">1.52x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.98</td>
    <td style="white-space: nowrap; text-align: right">2.16x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">16.66</td>
    <td style="white-space: nowrap; text-align: right">2.2x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">8.78</td>
    <td style="white-space: nowrap; text-align: right">4.18x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>JSON Generator</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1188.50</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">412.20</td>
    <td style="white-space: nowrap; text-align: right">2.88x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">405.50</td>
    <td style="white-space: nowrap; text-align: right">2.93x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">397.61</td>
    <td style="white-space: nowrap; text-align: right">2.99x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">272.44</td>
    <td style="white-space: nowrap; text-align: right">4.36x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">94.44</td>
    <td style="white-space: nowrap; text-align: right">12.59x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Pokedex</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1648.51</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">628.07</td>
    <td style="white-space: nowrap; text-align: right">2.62x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">613.82</td>
    <td style="white-space: nowrap; text-align: right">2.69x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">470.18</td>
    <td style="white-space: nowrap; text-align: right">3.51x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">321.84</td>
    <td style="white-space: nowrap; text-align: right">5.12x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">108.45</td>
    <td style="white-space: nowrap; text-align: right">15.2x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>UTF-8 unescaped</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">14.38 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.09 K</td>
    <td style="white-space: nowrap; text-align: right">1.42x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">9.41 K</td>
    <td style="white-space: nowrap; text-align: right">1.53x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.13 K</td>
    <td style="white-space: nowrap; text-align: right">1.57x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.76 K</td>
    <td style="white-space: nowrap; text-align: right">3.02x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.79 K</td>
    <td style="white-space: nowrap; text-align: right">8.02x</td>
  </tr>

</table>

</div>

</div>

All benchmark details are available [here](./euneus_bench/runs/encode_parsed_plugins.md).

### Decode

#### Smart decoding

This benchmark uses the decode smart module via the `euneus:decode/1` function. Smart modules only receive the input as the argument, so no option is available to set. Smart modules are the fastest euneus modules.

<div style="overflow: auto;">

<div style="display: grid; grid-auto-flow: column; column-gap: 5px;">

<table style="width: 1%">
  <caption><b>Blockchain</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">5.94 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">5.77 K</td>
    <td style="white-space: nowrap; text-align: right">1.03x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">5.53 K</td>
    <td style="white-space: nowrap; text-align: right">1.08x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">4.87 K</td>
    <td style="white-space: nowrap; text-align: right">1.22x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">4.26 K</td>
    <td style="white-space: nowrap; text-align: right">1.39x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">3.25x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Giphy</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">925.19</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">498.15</td>
    <td style="white-space: nowrap; text-align: right">1.86x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">494.10</td>
    <td style="white-space: nowrap; text-align: right">1.87x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">451.81</td>
    <td style="white-space: nowrap; text-align: right">2.05x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">278.02</td>
    <td style="white-space: nowrap; text-align: right">3.33x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">162.97</td>
    <td style="white-space: nowrap; text-align: right">5.68x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>GitHub</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">2.69 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">2.09 K</td>
    <td style="white-space: nowrap; text-align: right">1.29x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">2.01 K</td>
    <td style="white-space: nowrap; text-align: right">1.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.81 K</td>
    <td style="white-space: nowrap; text-align: right">1.49x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.42 K</td>
    <td style="white-space: nowrap; text-align: right">1.9x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.51 K</td>
    <td style="white-space: nowrap; text-align: right">5.24x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>GovTrack</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">27.04</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.02</td>
    <td style="white-space: nowrap; text-align: right">1.59x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">16.79</td>
    <td style="white-space: nowrap; text-align: right">1.61x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">15.69</td>
    <td style="white-space: nowrap; text-align: right">1.72x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.35</td>
    <td style="white-space: nowrap; text-align: right">2.89x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.40</td>
    <td style="white-space: nowrap; text-align: right">6.14x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Issue 90</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">49.92</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">26.62</td>
    <td style="white-space: nowrap; text-align: right">1.88x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">25.25</td>
    <td style="white-space: nowrap; text-align: right">1.98x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">23.25</td>
    <td style="white-space: nowrap; text-align: right">2.15x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.79</td>
    <td style="white-space: nowrap; text-align: right">2.81x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">9.92</td>
    <td style="white-space: nowrap; text-align: right">5.03x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>JSON Generator (Pretty)</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1143.30</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">707.45</td>
    <td style="white-space: nowrap; text-align: right">1.62x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">696.88</td>
    <td style="white-space: nowrap; text-align: right">1.64x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">624.07</td>
    <td style="white-space: nowrap; text-align: right">1.83x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">435.82</td>
    <td style="white-space: nowrap; text-align: right">2.62x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">185.81</td>
    <td style="white-space: nowrap; text-align: right">6.15x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>JSON Generator</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">698.94</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">616.18</td>
    <td style="white-space: nowrap; text-align: right">1.13x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">591.72</td>
    <td style="white-space: nowrap; text-align: right">1.18x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">539.94</td>
    <td style="white-space: nowrap; text-align: right">1.29x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">400.04</td>
    <td style="white-space: nowrap; text-align: right">1.75x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">176.01</td>
    <td style="white-space: nowrap; text-align: right">3.97x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Pokedex</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1.36 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.17 K</td>
    <td style="white-space: nowrap; text-align: right">1.16x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.15 K</td>
    <td style="white-space: nowrap; text-align: right">1.18x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.04 K</td>
    <td style="white-space: nowrap; text-align: right">1.3x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.59 K</td>
    <td style="white-space: nowrap; text-align: right">2.31x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.26 K</td>
    <td style="white-space: nowrap; text-align: right">5.31x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>UTF-8 escaped</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">10.41 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.70 K</td>
    <td style="white-space: nowrap; text-align: right">6.14x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.69 K</td>
    <td style="white-space: nowrap; text-align: right">6.18x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.47 K</td>
    <td style="white-space: nowrap; text-align: right">7.09x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.29 K</td>
    <td style="white-space: nowrap; text-align: right">8.09x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.16 K</td>
    <td style="white-space: nowrap; text-align: right">8.97x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>UTF-8 unescaped</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">18.12 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">10.44 K</td>
    <td style="white-space: nowrap; text-align: right">1.74x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.19 K</td>
    <td style="white-space: nowrap; text-align: right">1.78x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.60 K</td>
    <td style="white-space: nowrap; text-align: right">1.89x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.44 K</td>
    <td style="white-space: nowrap; text-align: right">1.92x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">6.57 K</td>
    <td style="white-space: nowrap; text-align: right">2.76x</td>
  </tr>

</table>

</div>

</div>

All benchmark details are available [here](./euneus_bench/runs/decode.md).

#### Decoding with empty map as option

This benchmark passes the input and options parsed as the `euneus:decode_parsed/2` function arguments. There is no option set, just an empty map, so all the default options are used. This function it's a bit slower than the smart one, but all options are analyzed in the run.

<div style="overflow: auto;">

<div style="display: grid; grid-auto-flow: column; column-gap: 5px;">

<table style="width: 1%">
  <caption><b>Blockchain</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">5.98 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">5.55 K</td>
    <td style="white-space: nowrap; text-align: right">1.08x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">4.87 K</td>
    <td style="white-space: nowrap; text-align: right">1.23x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">4.28 K</td>
    <td style="white-space: nowrap; text-align: right">1.4x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.59 K</td>
    <td style="white-space: nowrap; text-align: right">1.67x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">3.26x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Giphy</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">990.34</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">497.98</td>
    <td style="white-space: nowrap; text-align: right">1.99x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">456.78</td>
    <td style="white-space: nowrap; text-align: right">2.17x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">278.42</td>
    <td style="white-space: nowrap; text-align: right">3.56x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">277.71</td>
    <td style="white-space: nowrap; text-align: right">3.57x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">167.26</td>
    <td style="white-space: nowrap; text-align: right">5.92x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>GitHub</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">2.70 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">2.01 K</td>
    <td style="white-space: nowrap; text-align: right">1.35x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.82 K</td>
    <td style="white-space: nowrap; text-align: right">1.49x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.42 K</td>
    <td style="white-space: nowrap; text-align: right">1.91x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.17 K</td>
    <td style="white-space: nowrap; text-align: right">2.3x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.52 K</td>
    <td style="white-space: nowrap; text-align: right">5.2x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>GovTrack</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">27.25</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.27</td>
    <td style="white-space: nowrap; text-align: right">1.58x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">15.78</td>
    <td style="white-space: nowrap; text-align: right">1.73x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.55</td>
    <td style="white-space: nowrap; text-align: right">2.85x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">7.82</td>
    <td style="white-space: nowrap; text-align: right">3.49x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.45</td>
    <td style="white-space: nowrap; text-align: right">6.13x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Issue 90</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">49.74</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">25.26</td>
    <td style="white-space: nowrap; text-align: right">1.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">25.23</td>
    <td style="white-space: nowrap; text-align: right">1.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">23.29</td>
    <td style="white-space: nowrap; text-align: right">2.14x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.98</td>
    <td style="white-space: nowrap; text-align: right">2.77x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">9.97</td>
    <td style="white-space: nowrap; text-align: right">4.99x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>JSON Generator (Pretty)</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1162.25</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">694.53</td>
    <td style="white-space: nowrap; text-align: right">1.67x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">625.05</td>
    <td style="white-space: nowrap; text-align: right">1.86x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">436.44</td>
    <td style="white-space: nowrap; text-align: right">2.66x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">347.69</td>
    <td style="white-space: nowrap; text-align: right">3.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">191.09</td>
    <td style="white-space: nowrap; text-align: right">6.08x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>JSON Generator</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">696.27</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">586.85</td>
    <td style="white-space: nowrap; text-align: right">1.19x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">541.03</td>
    <td style="white-space: nowrap; text-align: right">1.29x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">400.67</td>
    <td style="white-space: nowrap; text-align: right">1.74x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">317.16</td>
    <td style="white-space: nowrap; text-align: right">2.2x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">181.31</td>
    <td style="white-space: nowrap; text-align: right">3.84x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Pokedex</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1.35 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.15 K</td>
    <td style="white-space: nowrap; text-align: right">1.18x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.04 K</td>
    <td style="white-space: nowrap; text-align: right">1.3x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.59 K</td>
    <td style="white-space: nowrap; text-align: right">2.29x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">0.40 K</td>
    <td style="white-space: nowrap; text-align: right">3.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.26 K</td>
    <td style="white-space: nowrap; text-align: right">5.27x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>UTF-8 escaped</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">10.38 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.70 K</td>
    <td style="white-space: nowrap; text-align: right">6.09x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.70 K</td>
    <td style="white-space: nowrap; text-align: right">6.11x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.45 K</td>
    <td style="white-space: nowrap; text-align: right">7.18x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.30 K</td>
    <td style="white-space: nowrap; text-align: right">7.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.18 K</td>
    <td style="white-space: nowrap; text-align: right">8.83x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>UTF-8 unescaped</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">18.07 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">10.37 K</td>
    <td style="white-space: nowrap; text-align: right">1.74x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.20 K</td>
    <td style="white-space: nowrap; text-align: right">1.77x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.59 K</td>
    <td style="white-space: nowrap; text-align: right">1.89x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.51 K</td>
    <td style="white-space: nowrap; text-align: right">1.9x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">6.58 K</td>
    <td style="white-space: nowrap; text-align: right">2.75x</td>
  </tr>

</table>

</div>

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

<div style="overflow: auto;">

<div style="display: grid; grid-auto-flow: column; column-gap: 5px;">

<table style="width: 1%">
  <caption><b>Blockchain</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">5.98 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">5.55 K</td>
    <td style="white-space: nowrap; text-align: right">1.08x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">4.87 K</td>
    <td style="white-space: nowrap; text-align: right">1.23x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">4.28 K</td>
    <td style="white-space: nowrap; text-align: right">1.4x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.59 K</td>
    <td style="white-space: nowrap; text-align: right">1.67x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">3.26x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Giphy</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">990.34</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">497.98</td>
    <td style="white-space: nowrap; text-align: right">1.99x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">456.78</td>
    <td style="white-space: nowrap; text-align: right">2.17x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">278.42</td>
    <td style="white-space: nowrap; text-align: right">3.56x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">277.71</td>
    <td style="white-space: nowrap; text-align: right">3.57x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">167.26</td>
    <td style="white-space: nowrap; text-align: right">5.92x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>GitHub</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">2.70 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">2.01 K</td>
    <td style="white-space: nowrap; text-align: right">1.35x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.82 K</td>
    <td style="white-space: nowrap; text-align: right">1.49x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.42 K</td>
    <td style="white-space: nowrap; text-align: right">1.91x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.17 K</td>
    <td style="white-space: nowrap; text-align: right">2.3x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.52 K</td>
    <td style="white-space: nowrap; text-align: right">5.2x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>GovTrack</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">27.25</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.27</td>
    <td style="white-space: nowrap; text-align: right">1.58x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">15.78</td>
    <td style="white-space: nowrap; text-align: right">1.73x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.55</td>
    <td style="white-space: nowrap; text-align: right">2.85x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">7.82</td>
    <td style="white-space: nowrap; text-align: right">3.49x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.45</td>
    <td style="white-space: nowrap; text-align: right">6.13x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Issue 90</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">49.74</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">25.26</td>
    <td style="white-space: nowrap; text-align: right">1.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">25.23</td>
    <td style="white-space: nowrap; text-align: right">1.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">23.29</td>
    <td style="white-space: nowrap; text-align: right">2.14x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.98</td>
    <td style="white-space: nowrap; text-align: right">2.77x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">9.97</td>
    <td style="white-space: nowrap; text-align: right">4.99x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>JSON Generator (Pretty</b>)</caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1162.25</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">694.53</td>
    <td style="white-space: nowrap; text-align: right">1.67x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">625.05</td>
    <td style="white-space: nowrap; text-align: right">1.86x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">436.44</td>
    <td style="white-space: nowrap; text-align: right">2.66x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">347.69</td>
    <td style="white-space: nowrap; text-align: right">3.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">191.09</td>
    <td style="white-space: nowrap; text-align: right">6.08x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>JSON Generator</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">696.27</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">586.85</td>
    <td style="white-space: nowrap; text-align: right">1.19x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">541.03</td>
    <td style="white-space: nowrap; text-align: right">1.29x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">400.67</td>
    <td style="white-space: nowrap; text-align: right">1.74x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">317.16</td>
    <td style="white-space: nowrap; text-align: right">2.2x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">181.31</td>
    <td style="white-space: nowrap; text-align: right">3.84x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>Pokedex</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1.35 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.15 K</td>
    <td style="white-space: nowrap; text-align: right">1.18x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.04 K</td>
    <td style="white-space: nowrap; text-align: right">1.3x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.59 K</td>
    <td style="white-space: nowrap; text-align: right">2.29x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">0.40 K</td>
    <td style="white-space: nowrap; text-align: right">3.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.26 K</td>
    <td style="white-space: nowrap; text-align: right">5.27x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>UTF-8 escaped</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">10.38 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.70 K</td>
    <td style="white-space: nowrap; text-align: right">6.09x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.70 K</td>
    <td style="white-space: nowrap; text-align: right">6.11x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.45 K</td>
    <td style="white-space: nowrap; text-align: right">7.18x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.30 K</td>
    <td style="white-space: nowrap; text-align: right">7.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.18 K</td>
    <td style="white-space: nowrap; text-align: right">8.83x</td>
  </tr>

</table>

<table style="width: 1%">
  <caption><b>UTF-8 unescaped</b></caption>
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">18.07 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">10.37 K</td>
    <td style="white-space: nowrap; text-align: right">1.74x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.20 K</td>
    <td style="white-space: nowrap; text-align: right">1.77x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.59 K</td>
    <td style="white-space: nowrap; text-align: right">1.89x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.51 K</td>
    <td style="white-space: nowrap; text-align: right">1.9x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">6.58 K</td>
    <td style="white-space: nowrap; text-align: right">2.75x</td>
  </tr>

</table>

</div>

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
