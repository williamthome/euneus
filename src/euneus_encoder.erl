-module(euneus_encoder).

-compile({inline, [
    do_encode/2, encode_binary/2, encode_atom/2, encode_integer/2,
    encode_float/2, encode_list/2, encode_map/2, encode_datetime/2,
    encode_timestamp/2, encode_unhandled/2, do_encode_list_loop/2,
    do_encode_map/2, do_encode_map_loop/2, escape_binary/2,
    escape_json/4, escape_json_chunk/5, escape_byte/1
]}).
-compile({inline_size, 100}).

-export([ encode/2 ]).
-export([ escape_byte/1 ]).
-export([ escape_json/1, escape_html/1, escape_js/1, escape_unicode/1 ]).

-define(int_min(X, Min), is_integer(X) andalso X >= Min).
-define(int_range(X, Min, Max), is_integer(X) andalso X >= Min andalso X =< Max).

encode(Term, Opts) ->
    do_encode(Term, parse_opts(Opts)).

% The explicit call to encode_*/2 wrapped in a function is required
% for the inline optimization.
parse_opts(Opts) ->
    #{
        encode_binary => maps:get(encode_binary, Opts, fun (X, O) ->
            encode_binary(X, O)
        end),
        encode_atom => maps:get(encode_atom, Opts, fun (X, O) ->
            encode_atom(X, O)
        end),
        encode_integer => maps:get(encode_integer, Opts, fun (X, O) ->
            encode_integer(X, O)
        end),
        encode_float => maps:get(encode_float, Opts, fun (X, O) ->
            encode_float(X, O)
        end),
        encode_list => maps:get(encode_list, Opts, fun (X, O) ->
            encode_list(X, O)
        end),
        encode_map => maps:get(encode_map, Opts, fun (X, O) ->
            encode_map(X, O)
        end),
        encode_datetime => maps:get(encode_datetime, Opts, fun (X, O) ->
            encode_datetime(X, O)
        end),
        encode_timestamp => maps:get(encode_timestamp, Opts, fun (X, O) ->
            encode_timestamp(X, O)
        end),
        encode_unhandled => maps:get(encode_unhandled, Opts, fun (X, O) ->
            encode_unhandled(X, O)
        end),
        escape_binary => maps:get(escape_binary, Opts, fun (X, _O) ->
            escape_json(X, [], X, 0)
        end)
    }.

do_encode(Bin, #{encode_binary := Encode} = Opts) when is_binary(Bin) ->
    Encode(Bin, Opts);
do_encode(Atom, #{encode_atom := Encode} = Opts) when is_atom(Atom) ->
    Encode(Atom, Opts);
do_encode(Int, #{encode_integer := Encode} = Opts) when is_integer(Int) ->
    Encode(Int, Opts);
do_encode(Float, #{encode_float := Encode} = Opts) when is_float(Float) ->
    Encode(Float, Opts);
do_encode(List, #{encode_list := Encode} = Opts) when is_list(List) ->
    Encode(List, Opts);
do_encode(Map, #{encode_map := Encode} = Opts) when is_map(Map) ->
    Encode(Map, Opts);
do_encode({{YYYY,MM,DD},{H,M,S}} = DateTime, #{encode_datetime := Encode} = Opts)
  when ?int_min(YYYY, 0), ?int_range(MM, 1, 12), ?int_range(DD, 1, 31)
     , ?int_range(H, 0, 23), ?int_range(M, 0, 59), ?int_range(S, 0, 59) ->
    Encode(DateTime, Opts);
do_encode({MegaSecs,Secs,MicroSecs} = Timestamp, #{encode_timestamp := Encode} = Opts)
  when ?int_min(MegaSecs, 0), ?int_min(Secs, 0), ?int_min(MicroSecs, 0) ->
    Encode(Timestamp, Opts);
do_encode(Term, #{encode_unhandled := Encode} = Opts) ->
    Encode(Term, Opts).

encode_binary(Bin, Opts) ->
    [$", escape_binary(Bin, Opts), $"].

encode_atom(true, _Opts) ->
    <<"true">>;
encode_atom(false, _Opts) ->
    <<"false">>;
encode_atom(undefined, _Opts) ->
    <<"null">>;
encode_atom(nil, _Opts) ->
    <<"null">>;
encode_atom(null, _Opts) ->
    <<"null">>;
encode_atom(Atom, Opts) ->
    [$", escape_binary(atom_to_binary(Atom, utf8), Opts), $"].

encode_integer(Int, _Opts) ->
    integer_to_binary(Int).

encode_float(Float, _Opts) ->
    float_to_binary(Float, [short]).

encode_list([First | Rest], Opts) ->
    [$[, do_encode(First, Opts) | do_encode_list_loop(Rest, Opts)];
encode_list([], _Opts) ->
    <<"[]">>.

do_encode_list_loop([], _Opts) ->
    [$]];
do_encode_list_loop([First | Rest], Opts) ->
    [$,, do_encode(First, Opts) | do_encode_list_loop(Rest, Opts)].

encode_map(Map, Opts) ->
    do_encode_map(maps:to_list(Map), Opts).

do_encode_map([{K, V} | T], Opts) ->
    [${, do_encode(K, Opts), $:, do_encode(V, Opts) | do_encode_map_loop(T, Opts)];
do_encode_map([], _) ->
    <<"{}">>.

do_encode_map_loop([], _Opts) ->
    [$}];
do_encode_map_loop([{K, V} | T], Opts) ->
    [$,, do_encode(K, Opts), $:, do_encode(V, Opts) | do_encode_map_loop(T, Opts)].

encode_datetime({{YYYY,MM,DD},{H,M,S}}, Opts) ->
    DateTime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [YYYY,MM,DD,H,M,S])
    ),
    [$", escape_binary(DateTime, Opts), $"].

encode_timestamp({_,_,MicroSecs} = Timestamp, Opts) ->
    MilliSecs = MicroSecs div 1000,
    {{YYYY,MM,DD},{H,M,S}} = calendar:now_to_datetime(Timestamp),
    DateTime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
        [YYYY,MM,DD,H,M,S,MilliSecs])
    ),
    [$", escape_binary(DateTime, Opts), $"].

encode_unhandled(Term, Opts) ->
    error(unsupported_type, [Term, Opts]).

escape_binary(Bin, #{escape_binary := Escape} = Opts) ->
    Escape(Bin, Opts).

escape_json(Bin) ->
    escape_json(Bin, [], Bin, 0).

escape_json(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip)
    when Byte < 32; Byte =:= 34; Byte =:= 92 ->
    Acc = [Acc0 | escape_byte(Byte)],
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
    invalid_byte_error(Byte, Input).

escape_json_chunk(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip, Len)
    when Byte < 32; Byte =:= 34; Byte =:= 92 ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part | escape_byte(Byte)],
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
    Part = binary_part(Input, Skip, Len),
    [Acc | Part];
escape_json_chunk(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Skip, _Len) ->
    invalid_byte_error(Byte, Input).

escape_html(Bin) ->
    escape_html(Bin, [], Bin, 0).

escape_html(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip)
    when Byte < 33; Byte =:= 34; Byte =:= 47; Byte =:= 92 ->
    Acc = [Acc0 | escape_byte(Byte)],
    escape_html(Rest, Acc, Input, Skip+1);
escape_html(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip)
    when Byte < 128 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 1);
escape_html(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip)
    when Char < 2048 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 2);
escape_html(<<8232/utf8, Rest/bitstring>>, Acc0, Input, Skip) ->
    Acc = [Acc0 | <<"\\u2028">>],
    escape_html(Rest, Acc, Input, Skip+3);
escape_html(<<8233/utf8, Rest/bitstring>>, Acc0, Input, Skip) ->
    Acc = [Acc0 | <<"\\u2029">>],
    escape_html(Rest, Acc, Input, Skip+3);
escape_html(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip)
    when Char < 65536 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 3);
escape_html(<<_Char/utf8, Rest/bitstring>>, Acc, Input, Skip) ->
    escape_html_chunk(Rest, Acc, Input, Skip, 4);
escape_html(<<>>, Acc, _Input, _Skip) ->
    Acc;
escape_html(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Skip) ->
    invalid_byte_error(Byte, Input).

escape_html_chunk(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Byte < 33; Byte =:= 34; Byte =:= 47; Byte =:= 92 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape_byte(Byte)],
    escape_html(Rest, Acc2, Input, Skip+Len+1);
escape_html_chunk(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Byte < 128 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len+1);
escape_html_chunk(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Char < 2048 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len+2);
escape_html_chunk(<<8232/utf8, Rest/bitstring>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | <<"\\u2028">>],
    escape_html(Rest, Acc2, Input, Skip+Len+3);
escape_html_chunk(<<8233/utf8, Rest/bitstring>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | <<"\\u2029">>],
    escape_html(Rest, Acc2, Input, Skip+Len+3);
escape_html_chunk(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Char < 65536 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len+3);
escape_html_chunk(<<_Char/utf8, Rest/bitstring>>, Acc, Input, Skip, Len) ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len+4);
escape_html_chunk(<<>>, [], Input, Skip, Len) ->
    binary_part(Input, Skip, Len);
escape_html_chunk(<<>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    [Acc | Part];
escape_html_chunk(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Skip, _Len) ->
    invalid_byte_error(Byte, Input).

escape_js(Bin) ->
    escape_js(Bin, [], Bin, 0).

escape_js(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip)
    when Byte < 32; Byte =:= 34; Byte =:= 92 ->
    Acc = [Acc0 | escape_byte(Byte)],
    escape_js(Rest, Acc, Input, Skip+1);
escape_js(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip)
    when Byte < 128 ->
    escape_js_chunk(Rest, Acc, Input, Skip, 1);
escape_js(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip)
    when Char < 2048 ->
    escape_js_chunk(Rest, Acc, Input, Skip, 2);
escape_js(<<8232/utf8, Rest/bitstring>>, Acc0, Input, Skip) ->
    Acc = [Acc0 | <<"\\u2028">>],
    escape_js(Rest, Acc, Input, Skip+3);
escape_js(<<8233/utf8, Rest/bitstring>>, Acc0, Input, Skip) ->
    Acc = [Acc0 | <<"\\u2029">>],
    escape_js(Rest, Acc, Input, Skip+3);
escape_js(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip)
    when Char < 65536 ->
    escape_js_chunk(Rest, Acc, Input, Skip, 3);
escape_js(<<_Char/utf8, Rest/bitstring>>, Acc, Input, Skip) ->
    escape_js_chunk(Rest, Acc, Input, Skip, 4);
escape_js(<<>>, Acc, _Input, _Skip) ->
    Acc;
escape_js(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Skip) ->
    invalid_byte_error(Byte, Input).

escape_js_chunk(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip, Len)
    when Byte < 32; Byte =:= 34; Byte =:= 92 ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part | escape_byte(Byte)],
    escape_js(Rest, Acc, Input, Skip+Len+1);
escape_js_chunk(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Byte < 128 ->
    escape_js_chunk(Rest, Acc, Input, Skip, Len+1);
escape_js_chunk(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Char < 2048 ->
    escape_js_chunk(Rest, Acc, Input, Skip, Len+2);
escape_js_chunk(<<8232/utf8, Rest/bitstring>>, Acc0, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part | <<"\\u2028">>],
    escape_js(Rest, Acc, Input, Skip+Len+3);
escape_js_chunk(<<8233/utf8, Rest/bitstring>>, Acc0, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part | <<"\\u2029">>],
    escape_js(Rest, Acc, Input, Skip+Len+3);
escape_js_chunk(<<Char/utf8, Rest/bitstring>>, Acc, Input, Skip, Len)
    when Char < 65536 ->
    escape_js_chunk(Rest, Acc, Input, Skip, Len+3);
escape_js_chunk(<<_Char/utf8, Rest/bitstring>>, Acc, Input, Skip, Len) ->
    escape_js_chunk(Rest, Acc, Input, Skip, Len+4);
escape_js_chunk(<<>>, [], Input, Skip, Len) ->
    binary_part(Input, Skip, Len);
escape_js_chunk(<<>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    [Acc | Part];
escape_js_chunk(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Skip, _Len) ->
    invalid_byte_error(Byte, Input).

escape_unicode(Bin) ->
    escape_unicode(Bin, [], Bin, 0).

escape_unicode(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip)
  when Byte < 32; Byte =:= 34; Byte =:= 92 ->
    Acc = [Acc0 | escape_byte(Byte)],
    escape_unicode(Rest, Acc, Input, Skip+1);
escape_unicode(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip)
  when Byte < 128 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip)
  when Char < 256 ->
    Acc = [Acc0, <<"\\u00">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+2);
escape_unicode(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip)
  when Char < 2048 ->
    Acc = [Acc0, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+2);
escape_unicode(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip)
  when Char < 4096 ->
    Acc = [Acc0, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+3);
escape_unicode(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip)
  when Char < 65536 ->
    Acc = [Acc0, <<"\\u">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+3);
escape_unicode(<<Char0/utf8, Rest/bitstring>>, Acc0, Input, Skip) ->
    Char = Char0 - 65536,
    Acc = [ Acc0
          , <<"\\uD">>
          , integer_to_list(2048 bor (Char bsr 10), 16)
          , <<"\\uD">>
          | integer_to_list(3072 bor Char band 1023, 16)
          ],
    escape_unicode(Rest, Acc, Input, Skip+4);
escape_unicode(<<>>, Acc, _Input, _Skip) ->
    Acc;
escape_unicode(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Skip) ->
    invalid_byte_error(Byte, Input).

escape_unicode_chunk(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip, Len)
  when Byte < 32; Byte =:= 34; Byte =:= 92 ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part | escape_byte(Byte)],
    escape_unicode(Rest, Acc, Input, Skip+Len+1);
escape_unicode_chunk(<<Byte/integer, Rest/bitstring>>, Acc, Input, Skip, Len)
  when Byte < 128 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, Len+1);
escape_unicode_chunk(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip, Len)
  when Char < 256 ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part, <<"\\u00">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+Len+2);
escape_unicode_chunk(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip, Len)
  when Char < 2048 ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+Len+2);
escape_unicode_chunk(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip, Len)
  when Char < 4096 ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+Len+3);
escape_unicode_chunk(<<Char/utf8, Rest/bitstring>>, Acc0, Input, Skip, Len)
  when Char < 65536 ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part, <<"\\u">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc, Input, Skip+Len+3);
escape_unicode_chunk(<<Char0/utf8, Rest/bitstring>>, Acc0, Input, Skip, Len) ->
    Char = Char0 - 65536,
    Part = binary_part(Input, Skip, Len),
    Acc = [ Acc0
          , Part
          , <<"\\uD">>
          , integer_to_list(2048 bor (Char bsr 10), 16)
          , <<"\\uD">>
          | integer_to_list(3072 bor Char band 1023, 16)
          ],
    escape_unicode(Rest, Acc, Input, Skip+Len+4);
escape_unicode_chunk(<<>>, [], Input, Skip, Len) ->
    binary_part(Input, Skip, Len);
escape_unicode_chunk(<<>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    [Acc | Part];
escape_unicode_chunk(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Skip, _Len) ->
    invalid_byte_error(Byte, Input).

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
escape_byte(Byte) -> invalid_byte_error(Byte, Byte).

invalid_byte_error(Byte0, Input) ->
    Byte = <<"0x"/utf8, (integer_to_binary(Byte0, 16))/binary>>,
    error({invalid_byte, Byte, Input}).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    [ ?assertEqual(Expect, iolist_to_binary(encode(Input, #{})))
      || {Expect, Input} <- [
        {<<"true">>, true},
        {<<"\"foo\"">>, foo},
        {<<"\"foo\"">>, <<"foo">>},
        {<<"0">>, 0},
        {<<"123.456789">>, 123.45678900},
        {<<"[true,0]">>, [true, 0]},
        {<<"{\"foo\":\"bar\"}">>, #{foo => bar}},
        {<<"\"1970-01-01T00:00:00Z\"">>, {{1970,1,1},{0,0,0}}},
        {<<"\"1970-01-01T00:00:00.000Z\"">>, {0,0,0}}
    ]].

-endif.
