%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Encode Erlang terms to JSON.

%% Copyright 2023 William Fank Thomé
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(euneus_encoder).

-compile({inline, [
    % misc
    parse_opts/1,
    % encode
    encode_binary/2, encode_atom/2, encode_integer/2, encode_float/2,
    encode_list/2, encode_map/2, encode_datetime/2, encode_timestamp/2,
    encode_unhandled/2,
    % escape
    escape_json/1
]}).
-compile({inline_size, 100}).

% By default, encode_unhandled/2 will raise unsupported_type exception,
% so it is a function without a local return.
-dialyzer({no_return, [parse_opts/1, encode_unhandled/2]}).

-export([ encode/2 ]).
-export([
    encode_binary/2, encode_atom/2, encode_integer/2, encode_float/2,
    encode_list/2, encode_map/2, encode_datetime/2, encode_timestamp/2,
    encode_unhandled/2
]).
-export([ throw_unsupported_type_error/1 ]).
-export([ escape_binary/2, escape_byte/1 ]).
-export([ escape_json/1, escape_html/1, escape_js/1, escape_unicode/1 ]).

-export_type([ input/0, options/0, result/0, error_reason/0 ]).

-define(min(X, Min), is_integer(X) andalso X >= Min).
-define(range(X, Min, Max), is_integer(X) andalso X >= Min andalso X =< Max).

-type input() :: term().

-type options() :: #{
    nulls => list(),
    binary_encoder => encoder(Input :: binary()),
    atom_encoder => encoder(Input :: atom()),
    integer_encoder => encoder(Input :: integer()),
    float_encoder => encoder(Input :: float()),
    list_encoder => encoder(Input :: list()),
    map_encoder => encoder(Input :: map()),
    datetime_encoder => encoder(Input :: calendar:datetime()),
    timestamp_encoder => encoder(Input :: erlang:timestamp()),
    unhandled_encoder => encoder(Input :: term()),
    escaper => escaper(Input :: binary())
}.

-type error_reason() :: {unsupported_type, Unsupported :: term()}
                      | {invalid_byte, Byte :: byte(), Input :: binary()}.

-type result() :: {ok, iolist()} | {error, error_reason()}.

-type encoder(Input) :: fun((Input, options()) -> iolist()).
-type escaper(Input) :: fun((Input, options()) -> iolist()).

-spec encode(Term, Opts) -> Return when
    Term :: term(),
    Opts :: options(),
    Return :: result().

encode(Term, Opts) ->
    try
        {ok, value(Term, parse_opts(Opts))}
    catch
        throw:{unsupported_type, Unsupported} ->
            {error, {unsupported_type, Unsupported}};
        throw:{invalid_byte, Byte0, Input} ->
            Byte = <<"0x"/utf8, (integer_to_binary(Byte0, 16))/binary>>,
            {error, {invalid_byte, Byte, Input}}
    end.

% The explicit call to encode_*/2 wrapped in a function is required
% for the inline optimization.
parse_opts(Opts) ->
    #{
        nulls => maps:get(nulls, Opts, [undefined]),
        binary_encoder => maps:get(binary_encoder, Opts, fun (X, O) ->
            encode_binary(X, O)
        end),
        atom_encoder => maps:get(atom_encoder, Opts, fun (X, O) ->
            encode_atom(X, O)
        end),
        integer_encoder => maps:get(integer_encoder, Opts, fun (X, O) ->
            encode_integer(X, O)
        end),
        float_encoder => maps:get(float_encoder, Opts, fun (X, O) ->
            encode_float(X, O)
        end),
        list_encoder => maps:get(list_encoder, Opts, fun (X, O) ->
            encode_list(X, O)
        end),
        map_encoder => maps:get(map_encoder, Opts, fun (X, O) ->
            encode_map(X, O)
        end),
        datetime_encoder => maps:get(datetime_encoder, Opts, fun (X, O) ->
            encode_datetime(X, O)
        end),
        timestamp_encoder => maps:get(timestamp_encoder, Opts, fun (X, O) ->
            encode_timestamp(X, O)
        end),
        unhandled_encoder => maps:get(unhandled_encoder, Opts, fun (X, O) ->
            encode_unhandled(X, O)
        end),
        escaper => maps:get(escaper, Opts, fun (X, _O) ->
            [$", escape_json(X), $"]
        end)
    }.

key(Atom, #{binary_encoder := Encode} = Opts) when is_atom(Atom) ->
    Encode(atom_to_binary(Atom, utf8), Opts);
key(Bin, #{binary_encoder := Encode} = Opts) when is_binary(Bin) ->
    Encode(Bin, Opts);
key(Int, #{binary_encoder := Encode} = Opts) when is_integer(Int) ->
    Encode(integer_to_binary(Int), Opts).

value(Bin, #{binary_encoder := Encode} = Opts) when is_binary(Bin) ->
    Encode(Bin, Opts);
value(Atom, #{atom_encoder := Encode} = Opts) when is_atom(Atom) ->
    Encode(Atom, Opts);
value(Int, #{integer_encoder := Encode} = Opts) when is_integer(Int) ->
    Encode(Int, Opts);
value(Float, #{float_encoder := Encode} = Opts) when is_float(Float) ->
    Encode(Float, Opts);
value(List, #{list_encoder := Encode} = Opts) when is_list(List) ->
    Encode(List, Opts);
value(Map, #{map_encoder := Encode} = Opts) when is_map(Map) ->
    Encode(Map, Opts);
value({{YYYY,MM,DD},{H,M,S}} = DateTime, #{datetime_encoder := Encode} = Opts)
  when ?min(YYYY, 0), ?range(MM, 1, 12), ?range(DD, 1, 31)
     , ?range(H, 0, 23), ?range(M, 0, 59), ?range(S, 0, 59) ->
    Encode(DateTime, Opts);
value({MegaSecs,Secs,MicroSecs} = Timestamp, #{timestamp_encoder := Encode} = Opts)
  when ?min(MegaSecs, 0), ?min(Secs, 0), ?min(MicroSecs, 0) ->
    Encode(Timestamp, Opts);
value(Term, #{unhandled_encoder := Encode} = Opts) ->
    Encode(Term, Opts).

encode_binary(Bin, Opts) ->
    escape_binary(Bin, Opts).

encode_atom(true, _Opts) ->
    <<"true">>;
encode_atom(false, _Opts) ->
    <<"false">>;
encode_atom(Atom, #{nulls := Nulls} = Opts) ->
    case lists:member(Atom, Nulls) of
        true ->
            <<"null">>;
        false ->
            escape_binary(atom_to_binary(Atom, utf8), Opts)
    end.

encode_integer(Int, _Opts) ->
    integer_to_binary(Int).

encode_float(Float, _Opts) ->
    float_to_binary(Float, [short]).

encode_list([H | T], Opts) ->
    [$[, value(H, Opts), do_encode_list_loop(T, Opts)];
encode_list([], _Opts) ->
    <<"[]">>.

do_encode_list_loop([H | T], Opts) ->
    [$,, value(H, Opts), do_encode_list_loop(T, Opts)];
do_encode_list_loop([], _Opts) ->
    $].

encode_map(Map, Opts) ->
    do_encode_map(maps:to_list(Map), Opts).

do_encode_map([{K, V} | T], Opts) ->
    [${, key(K, Opts), $:, value(V, Opts), do_encode_map_loop(T, Opts)];
do_encode_map([], _) ->
    <<"{}">>.

do_encode_map_loop([{K, V} | T], Opts) ->
    [$,, key(K, Opts), $:, value(V, Opts), do_encode_map_loop(T, Opts)];
do_encode_map_loop([], _Opts) ->
    $}.

encode_datetime({{YYYY,MM,DD},{H,M,S}}, Opts) ->
    DateTime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [YYYY,MM,DD,H,M,S])
    ),
    escape_binary(DateTime, Opts).

encode_timestamp({_,_,MicroSecs} = Timestamp, Opts) ->
    MilliSecs = MicroSecs div 1000,
    {{YYYY,MM,DD},{H,M,S}} = calendar:now_to_datetime(Timestamp),
    DateTime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
        [YYYY,MM,DD,H,M,S,MilliSecs])
    ),
    escape_binary(DateTime, Opts).

encode_unhandled(Term, _Opts) ->
    throw_unsupported_type_error(Term).

escape_binary(Bin, #{escaper := Escape} = Opts) ->
    Escape(Bin, Opts).

escape_byte(0) -> <<"\\u0000">>;
escape_byte(1) -> <<"\\u0001">>;
escape_byte(2) -> <<"\\u0002">>;
escape_byte(3) -> <<"\\u0003">>;
escape_byte(4) -> <<"\\u0004">>;
escape_byte(5) -> <<"\\u0005">>;
escape_byte(6) -> <<"\\u0006">>;
escape_byte(7) -> <<"\\u0007">>;
escape_byte(8) -> <<"\\b">>;
escape_byte(9) -> <<"\\t">>;
escape_byte(10) -> <<"\\n">>;
escape_byte(11) -> <<"\\u000B">>;
escape_byte(12) -> <<"\\f">>;
escape_byte(13) -> <<"\\r">>;
escape_byte(14) -> <<"\\u000E">>;
escape_byte(15) -> <<"\\u000F">>;
escape_byte(16) -> <<"\\u0010">>;
escape_byte(17) -> <<"\\u0011">>;
escape_byte(18) -> <<"\\u0012">>;
escape_byte(19) -> <<"\\u0013">>;
escape_byte(20) -> <<"\\u0014">>;
escape_byte(21) -> <<"\\u0015">>;
escape_byte(22) -> <<"\\u0016">>;
escape_byte(23) -> <<"\\u0017">>;
escape_byte(24) -> <<"\\u0018">>;
escape_byte(25) -> <<"\\u0019">>;
escape_byte(26) -> <<"\\u001A">>;
escape_byte(27) -> <<"\\u001B">>;
escape_byte(28) -> <<"\\u001C">>;
escape_byte(29) -> <<"\\u001D">>;
escape_byte(30) -> <<"\\u001E">>;
escape_byte(31) -> <<"\\u001F">>;
escape_byte(34) -> <<"\\\"">>;
escape_byte(47) -> <<"\\/">>;
escape_byte(92) -> <<"\\\\">>;
escape_byte(Byte) -> throw_invalid_byte_error(Byte, Byte).

escape_json(Bin) ->
    escape_json(Bin, [], Bin, 0).

escape_json(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip)
    when Byte < 32; Byte =:= $\"; Byte =:= $\\ ->
    Acc = [Acc0, escape_byte(Byte)],
    escape_json(Rest, Acc, Input, Skip+1);
escape_json(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip)
    when Byte < 128 ->
    escape_json_chunk(Rest, Acc, Input, Skip, 1);
escape_json(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip)
    when Char < 2048 ->
    escape_json_chunk(Rest, Acc, Input, Skip, 2);
escape_json(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip)
    when Char < 65536 ->
    escape_json_chunk(Rest, Acc, Input, Skip, 3);
escape_json(<<_Char/utf8, Rest/bitstring>>, Acc, Input, Skip) ->
    escape_json_chunk(Rest, Acc, Input, Skip, 4);
escape_json(<<>>, Acc, _Input, _Skip) ->
    Acc;
escape_json(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Skip) ->
    throw_invalid_byte_error(Byte, Input).

escape_json_chunk(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip, Len)
    when Byte < 32; Byte =:= $\"; Byte =:= $\\ ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part, escape_byte(Byte)],
    escape_json(Rest, Acc, Input, Skip+Len+1);
escape_json_chunk(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Byte < 128 ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len+1);
escape_json_chunk(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Char < 2048 ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len+2);
escape_json_chunk(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Char < 65536 ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len+3);
escape_json_chunk(<<_Char/utf8, Rest/bitstring>>, Acc, Input, Skip, Len) ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len+4);
escape_json_chunk(<<>>, [], Input, Skip, Len) ->
    binary_part(Input, Skip, Len);
escape_json_chunk(<<>>, Acc, Input, Skip, Len) ->
    [Acc, binary_part(Input, Skip, Len)];
escape_json_chunk(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Skip, _Len) ->
    throw_invalid_byte_error(Byte, Input).

escape_html(Bin) ->
    escape_html(Bin, [], Bin, 0).

escape_html(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip)
    when Byte < 33; Byte =:= $\"; Byte =:= $/; Byte =:= $\\ ->
    Acc = [Acc0, escape_byte(Byte)],
    escape_html(Rest, Acc, Input, Skip+1);
escape_html(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip)
    when Byte < 128 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 1);
escape_html(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip)
    when Char < 2048 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 2);
escape_html(<<8232/utf8, Rest/bitstring>>, Acc0, Input, Skip) ->
    Acc = [Acc0, <<"\\u2028">>],
    escape_html(Rest, Acc, Input, Skip+3);
escape_html(<<8233/utf8, Rest/bitstring>>, Acc0, Input, Skip) ->
    Acc = [Acc0, <<"\\u2029">>],
    escape_html(Rest, Acc, Input, Skip+3);
escape_html(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip)
    when Char < 65536 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 3);
escape_html(<<_Char/utf8, Rest/bitstring>>, Acc, Input, Skip) ->
    escape_html_chunk(Rest, Acc, Input, Skip, 4);
escape_html(<<>>, Acc, _Input, _Skip) ->
    Acc;
escape_html(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Skip) ->
    throw_invalid_byte_error(Byte, Input).

escape_html_chunk(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Byte < 33; Byte =:= $\"; Byte =:= $/; Byte =:= $\\ ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part, escape_byte(Byte)],
    escape_html(Rest, Acc2, Input, Skip+Len+1);
escape_html_chunk(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Byte < 128 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len+1);
escape_html_chunk(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Char < 2048 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len+2);
escape_html_chunk(<<8232/utf8, Rest/bitstring>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part, <<"\\u2028">>],
    escape_html(Rest, Acc2, Input, Skip+Len+3);
escape_html_chunk(<<8233/utf8, Rest/bitstring>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part, <<"\\u2029">>],
    escape_html(Rest, Acc2, Input, Skip+Len+3);
escape_html_chunk(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Char < 65536 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len+3);
escape_html_chunk(<<_Char/utf8, Rest/bitstring>>, Acc, Input, Skip, Len) ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len+4);
escape_html_chunk(<<>>, [], Input, Skip, Len) ->
    binary_part(Input, Skip, Len);
escape_html_chunk(<<>>, Acc, Input, Skip, Len) ->
    [Acc, binary_part(Input, Skip, Len)];
escape_html_chunk(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Skip, _Len) ->
    throw_invalid_byte_error(Byte, Input).

escape_js(Bin) ->
    escape_js(Bin, [], Bin, 0).

escape_js(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip)
    when Byte < 32; Byte =:= $\"; Byte =:= $\\ ->
    Acc = [Acc0, escape_byte(Byte)],
    escape_js(Rest, Acc, Input, Skip+1);
escape_js(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip)
    when Byte < 128 ->
    escape_js_chunk(Rest, Acc, Input, Skip, 1);
escape_js(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip)
    when Char < 2048 ->
    escape_js_chunk(Rest, Acc, Input, Skip, 2);
escape_js(<<8232/utf8, Rest/bitstring>>, Acc0, Input, Skip) ->
    Acc = [Acc0, <<"\\u2028">>],
    escape_js(Rest, Acc, Input, Skip+3);
escape_js(<<8233/utf8, Rest/bitstring>>, Acc0, Input, Skip) ->
    Acc = [Acc0, <<"\\u2029">>],
    escape_js(Rest, Acc, Input, Skip+3);
escape_js(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip)
    when Char < 65536 ->
    escape_js_chunk(Rest, Acc, Input, Skip, 3);
escape_js(<<_Char/utf8, Rest/bitstring>>, Acc, Input, Skip) ->
    escape_js_chunk(Rest, Acc, Input, Skip, 4);
escape_js(<<>>, Acc, _Input, _Skip) ->
    Acc;
escape_js(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Skip) ->
    throw_invalid_byte_error(Byte, Input).

escape_js_chunk(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip, Len)
    when Byte < 32; Byte =:= $\"; Byte =:= $\\ ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part, escape_byte(Byte)],
    escape_js(Rest, Acc, Input, Skip+Len+1);
escape_js_chunk(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Byte < 128 ->
    escape_js_chunk(Rest, Acc, Input, Skip, Len+1);
escape_js_chunk(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Char < 2048 ->
    escape_js_chunk(Rest, Acc, Input, Skip, Len+2);
escape_js_chunk(<<8232/utf8, Rest/bitstring>>, Acc0, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part, <<"\\u2028">>],
    escape_js(Rest, Acc, Input, Skip+Len+3);
escape_js_chunk(<<8233/utf8, Rest/bitstring>>, Acc0, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part, <<"\\u2029">>],
    escape_js(Rest, Acc, Input, Skip+Len+3);
escape_js_chunk(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Char < 65536 ->
    escape_js_chunk(Rest, Acc, Input, Skip, Len+3);
escape_js_chunk(<<_Char/utf8, Rest/bitstring>>, Acc, Input, Skip, Len) ->
    escape_js_chunk(Rest, Acc, Input, Skip, Len+4);
escape_js_chunk(<<>>, [], Input, Skip, Len) ->
    binary_part(Input, Skip, Len);
escape_js_chunk(<<>>, Acc, Input, Skip, Len) ->
    [Acc, binary_part(Input, Skip, Len)];
escape_js_chunk(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Skip, _Len) ->
    throw_invalid_byte_error(Byte, Input).

escape_unicode(Bin) ->
    escape_unicode(Bin, [], Bin, 0).

escape_unicode(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip)
  when Byte < 32; Byte =:= $\"; Byte =:= $\\ ->
    Acc = [Acc0, escape_byte(Byte)],
    escape_unicode(Rest, Acc, Input, Skip+1);
escape_unicode(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip)
  when Byte < 128 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip)
  when Char < 256 ->
    Acc = [Acc0, <<"\\u00">>, integer_to_binary(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+2);
escape_unicode(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip)
  when Char < 2048 ->
    Acc = [Acc0, <<"\\u0">>, integer_to_binary(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+2);
escape_unicode(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip)
  when Char < 4096 ->
    Acc = [Acc0, <<"\\u0">>, integer_to_binary(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+3);
escape_unicode(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip)
  when Char < 65536 ->
    Acc = [Acc0, <<"\\u">>, integer_to_binary(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+3);
escape_unicode(<<Char0/utf8, Rest/bitstring>>, Acc0, Input, Skip) ->
    Char = Char0 - 65536,
    Acc = [ Acc0
          , <<"\\uD">>
          , integer_to_binary(2048 bor (Char bsr 10), 16)
          , <<"\\uD">>
          , integer_to_binary(3072 bor Char band 1023, 16)
          ],
    escape_unicode(Rest, Acc, Input, Skip+4);
escape_unicode(<<>>, Acc, _Input, _Skip) ->
    Acc;
escape_unicode(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Skip) ->
    throw_invalid_byte_error(Byte, Input).

escape_unicode_chunk(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip, Len)
  when Byte < 32; Byte =:= $\"; Byte =:= $\\ ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part, escape_byte(Byte)],
    escape_unicode(Rest, Acc, Input, Skip+Len+1);
escape_unicode_chunk(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip, Len)
  when Byte < 128 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, Len+1);
escape_unicode_chunk(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip, Len)
  when Char < 256 ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part, <<"\\u00">>, integer_to_binary(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+Len+2);
escape_unicode_chunk(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip, Len)
  when Char < 2048 ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part, <<"\\u0">>, integer_to_binary(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+Len+2);
escape_unicode_chunk(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip, Len)
  when Char < 4096 ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part, <<"\\u0">>, integer_to_binary(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+Len+3);
escape_unicode_chunk(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip, Len)
  when Char < 65536 ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part, <<"\\u">>, integer_to_binary(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+Len+3);
escape_unicode_chunk(<<Char0/utf8, Rest/bitstring>>, Acc0, Input, Skip, Len) ->
    Char = Char0 - 65536,
    Part = binary_part(Input, Skip, Len),
    Acc = [ Acc0
          , Part
          , <<"\\uD">>
          , integer_to_binary(2048 bor (Char bsr 10), 16)
          , <<"\\uD">>
          , integer_to_binary(3072 bor Char band 1023, 16)
          ],
    escape_unicode(Rest, Acc, Input, Skip+Len+4);
escape_unicode_chunk(<<>>, [], Input, Skip, Len) ->
    binary_part(Input, Skip, Len);
escape_unicode_chunk(<<>>, Acc, Input, Skip, Len) ->
    [Acc, binary_part(Input, Skip, Len)];
escape_unicode_chunk(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Skip, _Len) ->
    throw_invalid_byte_error(Byte, Input).

throw_unsupported_type_error(Term) ->
    throw({unsupported_type, Term}).

throw_invalid_byte_error(Byte, Input) ->
    throw({invalid_byte, Byte, Input}).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    [ ?assertEqual(Expect, euneus:encode_to_binary(Input, Opts))
      || {Expect, Input, Opts} <- [
        {{ok, <<"true">>}, true, #{}},
        {{ok, <<"\"foo\"">>}, foo, #{}},
        {{ok, <<"\"foo\"">>}, <<"foo">>, #{}},
        {{ok, <<"0">>}, 0, #{}},
        {{ok, <<"123.456789">>}, 123.45678900, #{}},
        {{ok, <<"[true,0,null]">>}, [true, 0, undefined], #{}},
        {{ok, <<"{\"foo\":\"bar\"}">>}, #{foo => bar}, #{}},
        {{ok, <<"{\"0\":0}">>}, #{0 => 0}, #{}},
        {{ok, <<"\"1970-01-01T00:00:00Z\"">>}, {{1970,1,1},{0,0,0}}, #{}},
        {{ok, <<"\"1970-01-01T00:00:00.000Z\"">>}, {0,0,0}, #{}}
    ]].

-endif.
