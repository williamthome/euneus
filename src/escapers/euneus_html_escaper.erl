-module(euneus_html_escaper).

-behaviour(euneus_escaper).

-export([ escape/1 ]).

escape(Bin) ->
    escape_html(Bin, Bin, 0).

escape_html(Data, Input, Skip) ->
    escape_html(Data, [], Input, Skip).

escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte < 33 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte =:= 33 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte =:= 34 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte < 47 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte =:= 47 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte < 92 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte =:= 92 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte < 128 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 1);
escape_html(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip) when Char =< 2047 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 2);
escape_html(<<8232/utf8,Rest/bitstring>>, Acc, Input, Skip) ->
    Acc2 = [Acc | <<"\\u2028">>],
    escape_html(Rest, Acc2, Input, Skip + 3);
escape_html(<<8233/utf8,Rest/bitstring>>, Acc, Input, Skip) ->
    Acc2 = [Acc | <<"\\u2029">>],
    escape_html(Rest, Acc2, Input, Skip + 3);
escape_html(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip)
    when Char =< 65535 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 3);
escape_html(<<_Char/utf8,Rest/bitstring>>, Acc, Input, Skip) ->
    escape_html_chunk(Rest, Acc, Input, Skip, 4);
escape_html(<<>>, Acc, _Input, _Skip) ->
    Acc;
escape_html(<<Byte/integer,_Rest/bitstring>>, _Acc, Input, _Skip) ->
    euneus_escaper:invalid_byte_error(Byte, Input).

escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 32 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 34 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte =:= 34 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 47 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte =:= 47 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 92 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte =:= 92 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 128 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_html_chunk(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) when Char =< 2047 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len + 2);
escape_html_chunk(<<8232/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | <<"\\u2028">>],
    escape_html(Rest, Acc2, Input, Skip + Len + 3);
escape_html_chunk(<<8233/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | <<"\\u2029">>],
    escape_html(Rest, Acc2, Input, Skip + Len + 3);
escape_html_chunk(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) when Char =< 65535 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len + 3);
escape_html_chunk(<<_Char/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len + 4);
escape_html_chunk(<<>>, [], Input, Skip, Len) ->
    binary_part(Input, Skip, Len);
escape_html_chunk(<<>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    [Acc | Part];
escape_html_chunk(<<Byte/integer,_Rest/bitstring>>, _Acc, Input, _Skip, _Len) ->
    euneus_escaper:invalid_byte_error(Byte, Input).
