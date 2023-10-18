-module(euneus_unicode_escaper).

-behaviour(euneus_escaper).

-export([ escape/1 ]).

escape(Bin) ->
    escape_unicode(Bin, [], Bin, 0).

escape_unicode(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip)
  when Byte < 32; Byte =:= 34; Byte =:= 92 ->
    Acc = [Acc0 | euneus_escaper:escape(Byte)],
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
    euneus_escaper:invalid_byte_error(Byte, Input).

escape_unicode_chunk(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Skip, Len)
  when Byte < 32; Byte =:= 34; Byte =:= 92 ->
    Part = binary_part(Input, Skip, Len),
    Acc = [Acc0, Part | euneus_escaper:escape(Byte)],
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
    euneus_escaper:invalid_byte_error(Byte, Input).
