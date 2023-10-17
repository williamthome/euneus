-module(euneus_string_decoder).

-behaviour(euneus_decoder).

-export([ decode/3 ]).

decode(Bin, Opts, Buffer) ->
    {Rest, Decoded} = do_decode(Bin, <<>>),
    euneus_decoder:decode(Rest, Opts, [Decoded | Buffer]).

do_decode(<<$\\, $", T/binary>>, Buffer) ->
    do_decode(T, <<Buffer/binary, $\\, $">>);
do_decode(<<$", T/binary>>, Buffer) ->
    {T, Buffer};
do_decode(<<H, T/binary>>, Buffer) ->
    do_decode(T, <<Buffer/binary, H>>).
