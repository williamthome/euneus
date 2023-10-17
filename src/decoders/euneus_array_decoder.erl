-module(euneus_array_decoder).

-behaviour(euneus_decoder).

-export([ decode/2 ]).

decode(Bin, Opts) ->
    do_decode(Bin, Opts, []).

do_decode(T, Opts, Buffer) ->
    case euneus_decoder:decode(T, Opts, []) of
        {value_separator, Rest, _Opts, Term} ->
            do_decode(Rest, Opts, [Term | Buffer]);
        {end_array, Rest, _Opts, Term} ->
            {Rest, lists:reverse([Term | Buffer])}
    end.
