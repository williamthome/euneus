-module(euneus_atom_encoder).

-behaviour(euneus_encoder).

-export([ encode/2 ]).

encode(true, _Opts) ->
    <<"true">>;
encode(false, _Opts) ->
    <<"false">>;
encode(undefined, _Opts) ->
    <<"null">>;
encode(nil, _Opts) ->
    <<"null">>;
encode(null, _Opts) ->
    <<"null">>;
encode(Atom, #{escaper := Escaper}) ->
    [$", Escaper:escape(atom_to_binary(Atom, utf8)), $"].
