-module(euneus_object_decoder).

-behaviour(euneus_decoder).

-export([ decode/2 ]).

decode(Bin, Opts) ->
    do_decode(Bin, Opts, []).

do_decode(T0, Opts, Buffer) ->
    case euneus_decoder:decode(T0, Opts, []) of
        {name_separator, T, _Opts, Key} ->
            case euneus_decoder:decode(T, Opts, []) of
                {value_separator, Rest, _, Val} ->
                    do_decode(Rest, Opts, [{Key, Val} | Buffer]);
                {end_object, Rest, _, Val} ->
                    {Rest, maps:from_list([{Key, Val} | Buffer])}
            end
    end.
