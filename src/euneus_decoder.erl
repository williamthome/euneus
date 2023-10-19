-module(euneus_decoder).

-export([ decode/1, decode/2, decode/3 ]).

-callback decode(Bin, Opts) -> Result
    when Bin :: binary()
       , Opts :: map()
       , Result :: {Rest, Decoded}
       , Rest :: binary()
       , Decoded :: decoded()
       .

-type decoded() :: map()
                 | list()
                 | binary()
                 | number()
                 | true
                 | false
                 | undefined
                 .

-include("euneus_decoder.hrl").

decode(Bin) ->
    decode(Bin, #{}).

decode(Bin, Opts) ->
    decode(Bin, Opts, []).

decode(Bin, Opts0, Decoded) ->
    Opts = #{
        null_term => undefined,
        decoders => decoders(Opts0)
    },
    do_decode(Bin, Opts, Decoded).

decoders(Options) ->
    Defaults = #{
        object => euneus_object_decoder,
        array => euneus_array_decoder,
        string => euneus_string_decoder,
        number => euneus_number_decoder
    },
    maps:merge(Defaults, maps:get(decoders, Options, #{})).

do_decode(<<$", T/binary>>, #{decoders := #{string := Decoder}} = Opts, []) ->
    do_decode_1(T, Opts, Decoder);
do_decode(<<H, _/binary>> = T, #{decoders := #{number := Decoder}} = Opts, [])
  when ?is_number(H); H =:= $- ->
    do_decode_1(T, Opts, Decoder);
do_decode(<<H, T/binary>>, Opts, Decoded)
  when ?is_whitespace(H) ->
    do_decode(T, Opts, Decoded);
do_decode(<<$,, T/binary>>, Opts, Decoded) ->
    {value_separator, T, Opts, Decoded};
do_decode(<<"true", T/binary>>, Opts, []) ->
    do_decode(T, Opts, true);
do_decode(<<"false", T/binary>>, Opts, []) ->
    do_decode(T, Opts, false);
do_decode(<<"null", T/binary>>, Opts, []) ->
    do_decode(T, Opts, maps:get(null_term, Opts));
do_decode(<<$[, T/binary>>, #{decoders := #{array := Decoder}} = Opts, []) ->
    do_decode_1(T, Opts, Decoder);
do_decode(<<$], T/binary>>, Opts, Decoded) ->
    {end_array, T, Opts, Decoded};
do_decode(<<${, T/binary>>, #{decoders := #{object := Decoder}} = Opts, []) ->
    do_decode_1(T, Opts, Decoder);
do_decode(<<$:, T/binary>>, Opts, Decoded) ->
    {name_separator, T, Opts, Decoded};
do_decode(<<$}, T/binary>>, Opts, Decoded) ->
    {end_object, T, Opts, Decoded};
do_decode(<<>>, _, Decoded) ->
    Decoded.

do_decode_1(T0, Opts, Decoder) ->
    {T, Decoded} = Decoder:decode(T0, Opts),
    do_decode(T, Opts, Decoded).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    ?assertEqual(<<"foo">>, decode(<<"\"foo\"">>)),
    ?assertEqual(123, decode(<<"123">>)),
    ?assertEqual(1.234, decode(<<"1.234">>)),
    ?assertEqual(6.02e23, decode(<<"6.02e+23">>)),
    ?assertEqual([<<"foo">>, 123], decode(<<"[\"foo\",123]">>)),
    ?assertEqual(#{<<"foo">> => 123}, decode(<<"{\"foo\":123}">>)),
    ?assertEqual(undefined, decode(<<"null">>)),
    ?assertEqual(<<"ABC">>, decode(<<"\"\\u0041\\u0042\\u0043\"">>)).

-endif.
