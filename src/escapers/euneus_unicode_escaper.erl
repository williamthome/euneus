-module(euneus_unicode_escaper).

-behaviour(euneus_escaper).

-export([ escape/1 ]).

escape(Bin) ->
    escape_unicode(Bin, Bin, 0).

escape_unicode(Data, Input, Skip) ->
    escape_unicode(Data, [], Input, Skip).

escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 0 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 1 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 2 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 3 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 4 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 5 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 6 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 7 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 8 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 9 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 10 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 11 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 12 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 13 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 14 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 15 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 16 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 17 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 18 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 19 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 20 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 21 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 22 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 23 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 24 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 25 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 26 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 27 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 28 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 29 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 30 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 31 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 32 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 33 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 34 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 35 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 36 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 37 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 38 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 39 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 40 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 41 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 42 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 43 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 44 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 45 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 46 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 47 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 48 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 49 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 50 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 51 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 52 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 53 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 54 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 55 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 56 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 57 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 58 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 59 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 60 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 61 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 62 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 63 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 64 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 65 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 66 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 67 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 68 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 69 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 70 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 71 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 72 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 73 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 74 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 75 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 76 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 77 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 78 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 79 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 80 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 81 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 82 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 83 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 84 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 85 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 86 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 87 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 88 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 89 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 90 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 91 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 92 ->
    Acc2 = [Acc | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 93 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 94 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 95 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 96 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 97 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 98 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 99 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 100 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 101 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 102 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 103 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 104 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 105 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 106 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 107 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 108 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 109 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 110 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 111 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 112 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 113 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 114 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 115 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 116 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 117 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 118 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 119 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 120 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 121 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 122 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 123 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 124 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 125 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 126 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
                Acc, Input, Skip)
    when Byte =:= 127 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Char/utf8,Rest/bitstring>>,
                Acc, Input, Skip)
    when Char =< 255 ->
    Acc2 = [Acc, <<"\\u00">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + 2);
escape_unicode(<<Char/utf8,Rest/bitstring>>,
                Acc, Input, Skip)
    when Char =< 2047 ->
    Acc2 = [Acc, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + 2);
escape_unicode(<<Char/utf8,Rest/bitstring>>,
                Acc, Input, Skip)
    when Char =< 4095 ->
    Acc2 = [Acc, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + 3);
escape_unicode(<<Char/utf8,Rest/bitstring>>,
                Acc, Input, Skip)
    when Char =< 65535 ->
    Acc2 = [Acc, <<"\\u">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + 3);
escape_unicode(<<Char/utf8,Rest/bitstring>>,
                Acc, Input, Skip) ->
    _char@2 = Char - 65536,
    Acc2 =
        [Acc,
            <<"\\uD">>,
            integer_to_list(2048 bor (_char@2 bsr 10), 16),
            <<"\\uD">> |
            integer_to_list(3072 bor _char@2 band 1023, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + 4);
escape_unicode(<<>>, Acc, _Input, _Skip) ->
    Acc;
escape_unicode(<<Byte/integer,_Rest/bitstring>>,
                _Acc, Input, _Skip) ->
    euneus_escaper:invalid_byte_error(Byte, Input).

escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 0 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 1 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 2 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 3 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 4 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 5 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 6 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 7 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 8 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 9 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 10 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 11 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 12 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 13 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 14 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 15 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 16 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 17 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 18 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 19 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 20 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 21 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 22 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 23 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 24 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 25 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 26 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 27 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 28 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 29 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 30 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 31 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 32 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 33 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 34 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 35 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 36 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 37 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 38 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 39 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 40 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 41 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 42 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 43 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 44 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 45 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 46 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 47 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 48 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 49 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 50 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 51 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 52 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 53 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 54 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 55 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 56 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 57 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 58 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 59 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 60 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 61 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 62 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 63 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 64 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 65 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 66 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 67 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 68 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 69 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 70 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 71 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 72 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 73 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 74 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 75 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 76 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 77 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 78 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 79 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 80 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 81 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 82 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 83 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 84 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 85 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 86 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 87 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 88 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 89 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 90 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 91 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 92 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | euneus_escaper:escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 93 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 94 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 95 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 96 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 97 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 98 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 99 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 100 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 101 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 102 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 103 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 104 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 105 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 106 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 107 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 108 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 109 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 110 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 111 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 112 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 113 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 114 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 115 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 116 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 117 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 118 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 119 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 120 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 121 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 122 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 123 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 124 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 125 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 126 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Byte =:= 127 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                            Len + 1);
escape_unicode_chunk(<<Char/utf8,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Char =< 255 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 =
        [Acc, Part, <<"\\u00">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 2);
escape_unicode_chunk(<<Char/utf8,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Char =< 2047 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 =
        [Acc, Part, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 2);
escape_unicode_chunk(<<Char/utf8,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Char =< 4095 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 =
        [Acc, Part, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 3);
escape_unicode_chunk(<<Char/utf8,Rest/bitstring>>,
                        Acc, Input, Skip, Len)
    when Char =< 65535 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part, <<"\\u">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 3);
escape_unicode_chunk(<<Char/utf8,Rest/bitstring>>,
                        Acc, Input, Skip, Len) ->
    _char@2 = Char - 65536,
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part, <<"\\uD">>,
        integer_to_list(2048 bor (_char@2 bsr 10), 16),
        <<"\\uD">>
        | integer_to_list(3072 bor _char@2 band 1023, 16)
    ],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 4);
escape_unicode_chunk(<<>>, [], Input, Skip, Len) ->
    binary_part(Input, Skip, Len);
escape_unicode_chunk(<<>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    [Acc | Part];
escape_unicode_chunk(<<Byte/integer,_Rest/bitstring>>,
                        _Acc, Input, _Skip, _Len) ->
    euneus_escaper:invalid_byte_error(Byte, Input).
