-module(euneus_object_decoder).

-behaviour(euneus_decoder).

-export([ decode/3 ]).

decode(Bin, Opts, Buffer) ->
    {Rest, Decoded} = do_decode(Bin, Opts, []),
    euneus_decoder:decode(Rest, Opts, [Decoded | Buffer]).

do_decode(T0, Opts, Buffer) ->
    case euneus_decoder:decode(T0, Opts, []) of
        {object_key, T, _Opts, Key} ->
            case euneus_decoder:decode(T, Opts, []) of
                {more, Rest, _, Val} ->
                    do_decode(Rest, Opts, [{Key, Val} | Buffer]);
                {object_end, Rest, _, Val} ->
                    {Rest, maps:from_list([{Key, Val} | Buffer])}
            end
    end.
