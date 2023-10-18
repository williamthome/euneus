-module(euneus_binary_encoder).

-behaviour(euneus_encoder).

-export([ encode/2 ]).

encode(Bin, #{escaper := Escaper}) ->
    [$", Escaper:escape(Bin), $"].
