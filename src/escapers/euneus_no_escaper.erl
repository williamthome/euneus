-module(euneus_no_escaper).

-behaviour(euneus_escaper).

-export([ escape/1 ]).

escape(Bin) ->
    Bin.
