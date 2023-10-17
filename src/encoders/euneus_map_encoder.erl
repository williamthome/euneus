-module(euneus_map_encoder).

-behaviour(euneus_encoder).

-export([ encode/2 ]).

encode(Map, Opts) ->
    [${, lists:join($,, maps:fold(fun(Key, Val, Acc) ->
        [ [euneus_encoder:encode(Key, Opts), $:,
           euneus_encoder:encode(Val, Opts) ] | Acc]
    end, [], Map)), $}].
