-module(euneus_string_decoder).

-behaviour(euneus_decoder).

-export([ decode/2 ]).

decode(Bin, _Opts) ->
    do_decode(Bin, <<>>).

do_decode(<<$\\, $", T/binary>>, Buffer) ->
    do_decode(T, <<Buffer/binary, $\\, $">>);
do_decode(<<$", T/binary>>, Buffer) ->
    {T, Buffer};
do_decode(<<H, T/binary>>, Buffer) ->
    do_decode(T, <<Buffer/binary, H>>).
