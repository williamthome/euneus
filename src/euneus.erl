-module(euneus).

-compile({inline, [ encode/1, encode_to_binary/1, encode_to_binary/2, decode/1 ]}).

-export([ encode/1, encode/2 ]).
-export([ encode_to_binary/1, encode_to_binary/2 ]).
-export([ decode/1, decode/2 ]).

encode(Term) ->
    encode(Term, #{}).

encode(Term, Opts) ->
    euneus_encoder:encode(Term, Opts).

encode_to_binary(Term) ->
    encode_to_binary(Term, #{}).

encode_to_binary(Term, Opts) ->
    iolist_to_binary(encode(Term, Opts)).

decode(Bin) ->
    decode(Bin, #{}).

decode(Bin, Opts) ->
    euneus_decoder:decode(Bin, Opts).
