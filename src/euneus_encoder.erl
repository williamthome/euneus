-module(euneus_encoder).

-export([ encode/1 ]).

-callback encode(Term :: term(), Opts :: map()) -> iodata().

encode(Term) ->
    encode(Term, #{}).

encode(Term, Opts0) ->
    Opts = #{
        encoders => encoders(Opts0)
    },
    do_encode(Term, Opts).

encoders(Options) ->
    Defaults = #{
        integer => euneus_integer_encoder,
        binary => euneus_binary_encoder,
        atom => euneus_atom_encoder
    },
    maps:merge(Defaults, maps:get(encoders, Options, #{})).

do_encode(Term, #{encoders := #{integer := Encoder}} = Opts) when is_integer(Term) ->
    Encoder:encode(Term, Opts);
do_encode(Term, #{encoders := #{binary := Encoder}} = Opts) when is_binary(Term) ->
    Encoder:encode(Term, Opts);
do_encode(Term, #{encoders := #{atom := Encoder}} = Opts) when is_atom(Term) ->
    Encoder:encode(Term, Opts).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    ?assertEqual(<<"true">>, encode(true)),
    ?assertEqual([$", <<"foo">>, $"], encode(foo)),
    ?assertEqual([$", <<"foo">>, $"], encode(<<"foo">>)),
    ?assertEqual(<<"0">>, encode(0)).

-endif.
