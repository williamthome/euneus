-module(euneus_encoder).

-compile({inline, [ escape_json/1, escape_html/1, escape_js/1, escape_unicode/1 ]}).

-export([ encode/2 ]).
-export([ escape_byte/1 ]).
-export([ escape_json/1, escape_html/1, escape_js/1, escape_unicode/1 ]).

-define(int_min(X, Min), is_integer(X) andalso X >= Min).
-define(int_range(X, Min, Max), is_integer(X) andalso X >= Min andalso X =< Max).

encode(Term, Opts) ->
    do_encode(Term, Opts).

-ifdef(EUNEUS_ENABLE_CALENDAR).

do_encode(Bin, Opts) when is_binary(Bin) ->
    encode_binary(Opts, Bin);
do_encode(Atom, Opts) when is_atom(Atom) ->
    encode_atom(Opts, Atom);
do_encode(Int, Opts) when is_integer(Int) ->
    encode_integer(Opts, Int);
do_encode(Float, Opts) when is_float(Float) ->
    encode_float(Opts, Float);
do_encode(List, Opts) when is_list(List) ->
    encode_list(Opts, List);
do_encode(Map, Opts) when is_map(Map) ->
    encode_map(Opts, Map);
do_encode({{YYYY,MM,DD},{H,M,S}} = DateTime, Opts)
  when ?int_min(YYYY, 0), ?int_range(MM, 1, 12), ?int_range(DD, 1, 31)
     , ?int_range(H, 0, 23), ?int_range(M, 0, 59), ?int_range(S, 0, 59) ->
    encode_datetime(Opts, DateTime);
do_encode({MegaSecs,Secs,MicroSecs} = Timestamp, Opts)
  when ?int_min(MegaSecs, 0), ?int_min(Secs, 0), ?int_min(MicroSecs, 0) ->
    encode_timestamp(Opts, Timestamp);
do_encode(Term, #{encode_unhandled := Encode} = Opts) ->
    Encode(Term, Opts);
do_encode(Term , Opts) ->
    error(unsupported_type, [Term, Opts]).

encode_datetime(#{encode_datetime := Encode} = Opts, DateTime) ->
    Encode(DateTime, Opts);
encode_datetime(Opts, {{YYYY,MM,DD},{H,M,S}}) ->
    DateTime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [YYYY,MM,DD,H,M,S])
    ),
    [$", escape_binary(Opts, DateTime), $"].

encode_timestamp(#{encode_timestamp := Encode} = Opts, Timestamp) ->
    Encode(Timestamp, Opts);
encode_timestamp(Opts, {_,_,MicroSecs} = Timestamp) ->
    MilliSecs = MicroSecs div 1000,
    {{YYYY,MM,DD},{H,M,S}} = calendar:now_to_datetime(Timestamp),
    DateTime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
        [YYYY,MM,DD,H,M,S,MilliSecs])
    ),
    [$", escape_binary(Opts, DateTime), $"].

-else.

do_encode(Bin, Opts) when is_binary(Bin) ->
    encode_binary(Opts, Bin);
do_encode(Atom, Opts) when is_atom(Atom) ->
    encode_atom(Opts, Atom);
do_encode(Int, Opts) when is_integer(Int) ->
    encode_integer(Opts, Int);
do_encode(Float, Opts) when is_float(Float) ->
    encode_float(Opts, Float);
do_encode(List, Opts) when is_list(List) ->
    encode_list(Opts, List);
do_encode(Map, Opts) when is_map(Map) ->
    encode_map(Opts, Map);
do_encode(Term, #{encode_unhandled := Encode} = Opts) ->
    Encode(Term, Opts);
do_encode(Term , Opts) ->
    error(unsupported_type, [Term, Opts]).

-endif.

encode_binary(#{encode_binary := Encode} = Opts, Bin) ->
    Encode(Bin, Opts);
encode_binary(Opts, Bin) ->
    [$", escape_binary(Opts, Bin), $"].

encode_atom(#{encode_atom := Encode} = Opts, Atom) ->
    Encode(Atom, Opts);
encode_atom(Opts, Atom) ->
    do_encode_atom(Atom, Opts).

do_encode_atom(true, _Opts) ->
    <<"true">>;
do_encode_atom(false, _Opts) ->
    <<"false">>;
do_encode_atom(undefined, _Opts) ->
    <<"null">>;
do_encode_atom(nil, _Opts) ->
    <<"null">>;
do_encode_atom(null, _Opts) ->
    <<"null">>;
do_encode_atom(Atom, Opts) ->
    [$", escape_binary(Opts, atom_to_binary(Atom, utf8)), $"].

encode_integer(#{encode_integer := Encode} = Opts, Int) ->
    Encode(Int, Opts);
encode_integer(_Opts, Int) ->
    integer_to_binary(Int).

encode_float(#{encode_float := Encode} = Opts, Float) ->
    Encode(Float, Opts);
encode_float(_Opts, Float) ->
    float_to_binary(Float, [short]).

encode_list(#{encode_list := Encode} = Opts, List) ->
    Encode(List, Opts);
encode_list(Opts, List) ->
    do_encode_list(List, Opts).

do_encode_list([First | Rest], Opts) ->
    [$[, do_encode(First, Opts) | do_encode_list_loop(Rest, Opts)];
do_encode_list([], _Opts) ->
    <<"[]">>.

do_encode_list_loop([], _Opts) ->
    [$]];
do_encode_list_loop([First | Rest], Opts) ->
    [$,, do_encode(First, Opts) | do_encode_list_loop(Rest, Opts)].

encode_map(#{encode_map := Encode} = Opts, Map) ->
    Encode(Map, Opts);
encode_map(Opts, Map) ->
    do_encode_map(maps:to_list(Map), Opts).

do_encode_map([{K, V} | T], Opts) ->
    [${, do_encode(K, Opts), $:, do_encode(V, Opts) | do_encode_map_loop(T, Opts)];
do_encode_map([], _) ->
    <<"{}">>.

do_encode_map_loop([], _Opts) ->
    [$}];
do_encode_map_loop([{K, V} | T], Opts) ->
    [$,, do_encode(K, Opts), $:, do_encode(V, Opts) | do_encode_map_loop(T, Opts)].

escape_binary(#{escape_binary := Escape}, Bin) ->
    Escape(Bin);
escape_binary(_, Bin) ->
    escape_json(Bin).

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
