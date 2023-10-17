-module(euneus_decoder).

-export([ decode/1, decode/2, decode/3 ]).

-callback decode(Bin :: binary(), Opts :: map(), Buffer :: iolist()) -> iolist().

decode(Bin) ->
    decode(Bin, #{}).

decode(Bin, Opts) ->
    decode(Bin, Opts, []).

decode(Bin, Opts0, Buffer) ->
    Opts = #{
        decoders => decoders(Opts0)
    },
    do_decode(Bin, Opts, Buffer).

decoders(Options) ->
    Defaults = #{
        string => euneus_string_decoder
    },
    maps:merge(Defaults, maps:get(decoders, Options, #{})).

do_decode(<<$", T/binary>>, #{decoders := #{string := Decoder}} = Opts, Buffer) ->
    Decoder:decode(T, Opts, Buffer);
do_decode(<<>>, _, Buffer) ->
    lists:reverse(Buffer).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    ?assertEqual([<<"foo">>], decode(<<"\"foo\"">>)).

-endif.
