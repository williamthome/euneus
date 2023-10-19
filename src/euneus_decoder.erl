-module(euneus_decoder).

-export([ decode/1, decode/2, do_decode/2 ]).

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
    element(2, do_decode(Bin, parse_opts(Opts))).

parse_opts(Opts) ->
    #{
        null_term => maps:get(null_term, Opts, undefined),
        decoders => decoders(Opts)
    }.

decoders(Opts) ->
    Defaults = #{
        object => euneus_object_decoder,
        array => euneus_array_decoder,
        string => euneus_string_decoder,
        number => euneus_number_decoder
    },
    maps:merge(Defaults, maps:get(decoders, Opts, #{})).

do_decode(<<H, T/binary>>, Opts) when ?is_whitespace(H) ->
    do_decode(T, Opts);
do_decode(<<$", T/binary>>, #{decoders := #{string := Decoder}} = Opts) ->
    Decoder:decode(T, Opts);
do_decode(<<H, _/binary>> = T, #{decoders := #{number := Decoder}} = Opts)
  when ?is_number(H); H =:= $- ->
    Decoder:decode(T, Opts);
do_decode(<<"true", T/binary>>, _Opts) ->
    {T, true};
do_decode(<<"false", T/binary>>, _Opts) ->
    {T, false};
do_decode(<<"null", T/binary>>, Opts) ->
    {T, maps:get(null_term, Opts)};
do_decode(<<$[, T/binary>>, #{decoders := #{array := Decoder}} = Opts) ->
    Decoder:decode(T, Opts);
do_decode(<<${, T/binary>>, #{decoders := #{object := Decoder}} = Opts) ->
    Decoder:decode(T, Opts).

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
