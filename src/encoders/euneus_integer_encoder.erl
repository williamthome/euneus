-module(euneus_integer_encoder).

-behaviour(euneus_encoder).

-export([ encode/2 ]).

encode(Int, _Opts) ->
    integer_to_binary(Int).
