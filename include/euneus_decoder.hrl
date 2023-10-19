-define(is_number(X), X >= $0 andalso X =< $9).

-define(is_whitespace(X), X =:= $\s orelse X =:= $\t orelse
                          X =:= $\n orelse X =:= $\r).

-define(terminate, 0).
-define(array, 1).
-define(key, 2).
-define(object, 3).
-define(value, 4).
-define(continue, 5).
