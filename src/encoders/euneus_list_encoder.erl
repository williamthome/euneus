-module(euneus_list_encoder).

-behaviour(euneus_encoder).

-export([ encode/2 ]).

encode([], _Opts) ->
    <<"[]">>;
encode(List, Opts) ->
    [$[, lists:join($,, lists:map(fun(Term) ->
        euneus_encoder:encode(Term, Opts)
    end, List)), $]].
