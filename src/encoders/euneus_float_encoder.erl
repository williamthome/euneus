-module(euneus_float_encoder).

-behaviour(euneus_encoder).

-export([ encode/2 ]).

encode(Float, _Opts) ->
    float_to_binary(Float, [short]).
