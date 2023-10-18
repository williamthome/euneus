-module(euneus_number_decoder).

-behaviour(euneus_decoder).

-export([ decode/2 ]).

-include("euneus_decoder.hrl").

decode(Bin, _Opts) ->
    do_decode(Bin, integer, <<>>).

do_decode(<<H, T/binary>>, Kind, Buffer) when ?is_number(H) ->
    do_decode(T, Kind, <<Buffer/binary, H>>);
do_decode(<<$., T/binary>>, _, Buffer) ->
    do_decode(T, float, <<Buffer/binary, $.>>);
do_decode(<<$+, T/binary>>, Kind, Buffer) ->
do_decode(T, Kind, <<Buffer/binary, $+>>);
do_decode(<<$-, T/binary>>, Kind, Buffer) ->
do_decode(T, Kind, <<Buffer/binary, $->>);
do_decode(<<$e, T/binary>>, _, Buffer) ->
    do_decode(T, float, <<Buffer/binary, $e>>);
do_decode(T, integer, Buffer) ->
    {T, binary_to_integer(Buffer)};
do_decode(T, float, Buffer) ->
    {T, binary_to_float(Buffer)}.
