-define(is_number(X), X >= $0 andalso X =< $9).

-define(is_whitespace(X), X =:= 32 orelse X =:= $\t orelse
                          X =:= $\n orelse X =:= $\r).
