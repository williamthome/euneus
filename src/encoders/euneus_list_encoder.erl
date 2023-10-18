-module(euneus_list_encoder).

-behaviour(euneus_encoder).

-export([ encode/2 ]).

encode(List, Opts) ->
    do_encode(List, Opts).

do_encode([First | Rest], Opts) ->
    [$[, First | do_encode_loop(Rest, Opts)];
do_encode([], _Opts) ->
    <<"[]">>.

do_encode_loop([], _Opts) ->
    [$]];
do_encode_loop([First | Rest], Opts) ->
    [$,, euneus_encoder:do_encode(First, Opts) | do_encode_loop(Rest, Opts)].
