-define(is_number(X), X >= $0 andalso X =< $9).

-define(is_whitespace(X), X =:= $\s orelse X =:= $\t orelse
                          X =:= $\n orelse X =:= $\r).
