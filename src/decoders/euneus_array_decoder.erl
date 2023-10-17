-module(euneus_array_decoder).

-behaviour(euneus_decoder).

-export([ decode/3 ]).

decode(Bin, Opts, Buffer) ->
    {Rest, Decoded} = do_decode(Bin, Opts, []),
    euneus_decoder:decode(Rest, Opts, [Decoded | Buffer]).

do_decode(T, Opts, Buffer) ->
    case euneus_decoder:decode(T, Opts, []) of
        {more, Rest, _Opts, Term} ->
            do_decode(Rest, Opts, [Term | Buffer]);
        {array_end, Rest, _Opts, Term} ->
            {Rest, lists:reverse([Term | Buffer])}
    end.
