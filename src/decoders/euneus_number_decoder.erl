-module(euneus_number_decoder).

-behaviour(euneus_decoder).

-export([ decode/2 ]).

-include("euneus_decoder.hrl").

decode(Bin, _Opts) ->
    do_decode(Bin, integer, 0, Bin).

do_decode(<<H, T/binary>>, Kind, Len, Input) when ?is_number(H) ->
    do_decode(T, Kind, Len+1, Input);
do_decode(<<$., T/binary>>, _Kind, Len, Input) ->
    do_decode(T, float, Len+1, Input);
do_decode(<<$+, T/binary>>, Kind, Len, Input) ->
    do_decode(T, Kind, Len+1, Input);
do_decode(<<$-, T/binary>>, Kind, Len, Input) ->
    do_decode(T, Kind, Len+1, Input);
do_decode(<<$e, T/binary>>, _Kind, Len, Input) ->
    do_decode(T, float, Len+1, Input);
do_decode(T, integer, Len, Input) ->
    Part = binary_part(Input, 0, Len),
    {T, binary_to_integer(Part)};
do_decode(T, float, Len, Input) ->
    Part = binary_part(Input, 0, Len),
    {T, binary_to_float(Part)}.
