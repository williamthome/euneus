-module(euneus_map_encoder).

-behaviour(euneus_encoder).

-export([ encode/2 ]).

encode(Map, Opts) ->
    do_encode(maps:to_list(Map), Opts).

do_encode([{K, V} | T], Opts) ->
    [
        ${, euneus_encoder:do_encode(K, Opts),
        $:, euneus_encoder:do_encode(V, Opts)
        | do_encode_loop(T, Opts)
    ];
do_encode([], _) ->
    <<"{}">>.

do_encode_loop([], _Opts) ->
    [$}];
do_encode_loop([{K, V} | T], Opts) ->
    [
        $,, euneus_encoder:do_encode(K, Opts),
        $:, euneus_encoder:do_encode(V, Opts)
        | do_encode_loop(T, Opts)
    ].
