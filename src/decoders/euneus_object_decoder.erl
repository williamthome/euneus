-module(euneus_object_decoder).

-behaviour(euneus_decoder).

-export([ decode/2 ]).

-include("euneus_decoder.hrl").

decode(Bin, Opts) ->
    do_decode(Bin, Opts, []).

do_decode(T0, Opts, Buffer0) ->
    {T1, Key} = euneus_decoder:do_decode(T0, Opts),
    {T2, Value} = value(T1, Opts),
    Buffer = [{Key, Value} | Buffer0],
    case continue(T2) of
        {true, T} ->
            do_decode(T, Opts, Buffer);
        {false, T} ->
            {T, maps:from_list(Buffer)}
    end.

value(<<H, T/binary>>, Opts) when ?is_whitespace(H) ->
    value(T, Opts);
value(<<$:, T/binary>>, Opts) ->
    euneus_decoder:do_decode(T, Opts).

continue(<<H, T/binary>>) when ?is_whitespace(H) ->
    continue(T);
continue(<<$,, T/binary>>) ->
    {true, T};
continue(<<$}, T/binary>>) ->
    {false, T}.
