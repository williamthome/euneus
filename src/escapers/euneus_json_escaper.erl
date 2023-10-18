-module(euneus_json_escaper).

-behaviour(euneus_escaper).

-export([ escape/1 ]).

escape(Bin) ->
    escape_json(Bin, Bin, 0).

escape_json(Data, Input, Skip) ->
    escape_json(Data, [], Input, Skip).

escape_json(<<Byte/integer,Rest/bitstring>>, Acc1, Input, Skip) when Byte < 32 ->
    Acc2 = [Acc1 | euneus_escaper:escape(Byte)],
    escape_json(Rest, Acc2, Input, Skip + 1);
escape_json(<<Byte/integer,Rest/bitstring>>, Acc1, Input, Skip) when Byte < 34 ->
    escape_json_chunk(Rest, Acc1, Input, Skip, 1);
escape_json(<<Byte/integer,Rest/bitstring>>, Acc1, Input, Skip) when Byte =:= 34 ->
    Acc2 = [Acc1 | euneus_escaper:escape(Byte)],
    escape_json(Rest, Acc2, Input, Skip + 1);
escape_json(<<Byte/integer,Rest/bitstring>>, Acc1, Input, Skip) when Byte < 92 ->
    escape_json_chunk(Rest, Acc1, Input, Skip, 1);
escape_json(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte =:= 92 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_json(Rest, Acc2, Input, Skip + 1);
escape_json(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte < 128 ->
    escape_json_chunk(Rest, Acc, Input, Skip, 1);
escape_json(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip) when Char =< 2047 ->
    escape_json_chunk(Rest, Acc, Input, Skip, 2);
escape_json(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip) when Char =< 65535 ->
    escape_json_chunk(Rest, Acc, Input, Skip, 3);
escape_json(<<_Char/utf8,Rest/bitstring>>, Acc, Input, Skip) ->
    escape_json_chunk(Rest, Acc, Input, Skip, 4);
escape_json(<<>>, Acc, _Input, _Skip) ->
    Acc;
escape_json(<<Byte/integer,_Rest/bitstring>>, _Acc, Input, _Skip) ->
    euneus_escaper:invalid_byte_error(Byte, Input).

escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 32 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_json(Rest, Acc2, Input, Skip + Len + 1);
escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 34 ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte =:= 34 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_json(Rest, Acc2, Input, Skip + Len + 1);
escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 92 ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte =:= 92 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_json(Rest, Acc2, Input, Skip + Len + 1);
escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 128 ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_json_chunk(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) when Char =< 2047 ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len + 2);
escape_json_chunk(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) when Char =< 65535 ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len + 3);
escape_json_chunk(<<_Char/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len + 4);
escape_json_chunk(<<>>, [], Input, Skip, Len) ->
    binary_part(Input, Skip, Len);
escape_json_chunk(<<>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    [Acc | Part];
escape_json_chunk(<<Byte/integer,_Rest/bitstring>>, _Acc, Input, _Skip, _Len) ->
    euneus_escaper:invalid_byte_error(Byte, Input).
