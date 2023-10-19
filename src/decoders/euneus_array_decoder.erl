-module(euneus_array_decoder).

-behaviour(euneus_decoder).

-export([ decode/2 ]).

-include("euneus_decoder.hrl").

decode(Bin, Opts) ->
    do_decode(Bin, Opts, []).

do_decode(T0, Opts, Buffer0) ->
    {T1, Decoded} = euneus_decoder:do_decode(T0, Opts),
    Buffer = [Decoded | Buffer0],
    case continue(T1) of
        {true, T} ->
            do_decode(T, Opts, Buffer);
        {false, T} ->
            {T, lists:reverse(Buffer)}
    end.

continue(<<H, T/binary>>) when ?is_whitespace(H) ->
    continue(T);
continue(<<$,, T/binary>>) ->
    {true, T};
continue(<<$], T/binary>>) ->
    {false, T}.
