-module(euneus_encoder).

-export([ encode/1, encode/2, do_encode/2 ]).
-export([ encode_to_binary/1, encode_to_binary/2 ]).

-callback encode(Term :: term(), Opts :: map()) -> iodata().

encode(Term) ->
    encode(Term, #{}).

encode(Term, Opts) ->
    do_encode(Term, parse_opts(Opts)).

encode_to_binary(Term) ->
    encode_to_binary(Term, #{}).

encode_to_binary(Term, Opts) ->
    iolist_to_binary(encode(Term, Opts)).

parse_opts(Opts) ->
    #{
        escaper => escaper(Opts),
        encoders => encoders(Opts)
    }.

escaper(Opts) ->
    maps:get(escaper, Opts, euneus_json_escaper).

encoders(Opts) ->
    Defaults = #{
        map => euneus_map_encoder,
        list => euneus_list_encoder,
        float => euneus_float_encoder,
        integer => euneus_integer_encoder,
        binary => euneus_binary_encoder,
        atom => euneus_atom_encoder
    },
    maps:merge(Defaults, maps:get(encoders, Opts, #{})).

do_encode(Term, #{encoders := #{binary := Encoder}} = Opts) when is_binary(Term) ->
    Encoder:encode(Term, Opts);
do_encode(Term, #{encoders := #{atom := Encoder}} = Opts) when is_atom(Term) ->
    Encoder:encode(Term, Opts);
do_encode(Term, #{encoders := #{integer := Encoder}} = Opts) when is_integer(Term) ->
    Encoder:encode(Term, Opts);
do_encode(Term, #{encoders := #{float := Encoder}} = Opts) when is_float(Term) ->
    Encoder:encode(Term, Opts);
do_encode(Term, #{encoders := #{list := Encoder}} = Opts) when is_list(Term) ->
    Encoder:encode(Term, Opts);
do_encode(Term, #{encoders := #{map := Encoder}} = Opts) when is_map(Term) ->
    Encoder:encode(Term, Opts).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    ?assertEqual(<<"true">>, encode_to_binary(true)),
    ?assertEqual(<<"\"foo\"">>, encode_to_binary(foo)),
    ?assertEqual(<<"\"foo\"">>, encode_to_binary(<<"foo">>)),
    ?assertEqual(<<"0">>, encode_to_binary(0)),
    ?assertEqual(<<"123.456789">>, encode_to_binary(123.45678900)),
    ?assertEqual(<<"[true,0]">>, encode_to_binary([true, 0])),
    ?assertEqual(<<"{\"foo\":\"bar\"}">>, encode_to_binary(#{foo => bar})).

-endif.
