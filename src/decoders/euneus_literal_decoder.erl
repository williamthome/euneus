-module(euneus_literal_decoder).

-behaviour(euneus_decoder).

-export([ decode/2 ]).

decode(<<"true", Rest/binary>>, _Opts) ->
    {Rest, true};
decode(<<"false", Rest/binary>>, _Opts) ->
    {Rest, false};
decode(<<"null", Rest/binary>>, Opts) ->
    {Rest, maps:get(null_term, Opts)}.
