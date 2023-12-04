-compile({ inline, escape/4 }).
-compile({ inline, escape_chunk/5 }).

-dialyzer( no_improper_lists ).

%% API functions

-export([ encode/1 ]).

%% Types

-type input() :: euneus_encoder:input().
-type result() :: euneus_encoder:result().

%% Macros

-define(NON_PRINTABLE_LAST, 31).
-define(ONE_BYTE_LAST, 127).
-define(TWO_BYTE_LAST, 2_047).
-define(THREE_BYTE_LAST, 65_535).

-define(NULL, null).
-define(UNDEFINED, undefined).
-define(INTEGER_BASE, 10).
-define(FLOAT_TO_BIN_OPTS, [short]).

%%%=====================================================================
%%% API functions
%%%=====================================================================

-spec encode(input()) -> result().

encode(Term) ->
    try
        {ok, value(Term)}
    catch
        throw:{unsupported_type, Unsupported} ->
            {error, {unsupported_type, Unsupported}};
        throw:{invalid_byte, Byte0, Input} ->
            Byte = <<"0x"/utf8, (integer_to_binary(Byte0, 16))/binary>>,
            {error, {invalid_byte, Byte, Input}};
        throw:Reason ->
            {error, Reason};
        Class:Reason:Stacktrace ->
            erlang:raise(Class, Reason, Stacktrace)
    end.

key(Key) when is_binary(Key) ->
    [$", escape(Key, [], Key, 0), $"];
key(Atom) when is_atom(Atom) ->
    Key = atom_to_binary(Atom, utf8),
    [$", escape(Key, [], Key, 0), $"];
key(String) when is_list(String) ->
    Key = list_to_binary(String),
    [$", escape(Key, [], Key, 0), $"];
key(Int) when is_integer(Int) ->
    Key = integer_to_binary(Int),
    [$", escape(Key, [], Key, 0), $"].

value(Bin) when is_binary(Bin) ->
    [$", escape(Bin, [], Bin, 0), $"];
value(Atom) when is_atom(Atom) ->
    case Atom of
        true ->
            <<"true">>;
        false ->
            <<"false">>;
        ?NULL ->
            <<"null">>;
        ?UNDEFINED ->
            <<"null">>;
        _ ->
            Bin = atom_to_binary(Atom, utf8),
            [$", escape(Bin, [], Bin, 0), $"]
    end;
value(Int) when is_integer(Int) ->
    integer_to_binary(Int, ?INTEGER_BASE);
value(Float) when is_float(Float) ->
    float_to_binary(Float, ?FLOAT_TO_BIN_OPTS);
value(List) when is_list(List) ->
    case List of
        [] ->
            <<"[]">>;
        [H | T] ->
            [$[, value(H) | do_encode_list_loop(T)]
    end;
value(Map) when is_map(Map) ->
    case maps_to_list(erts_internal:map_next(0, Map, [])) of
        [] ->
            <<"{}">>;
        [{_, ?UNDEFINED} | T] ->
            [${, do_encode_map_loop(T, true)];
        [{K, V} | T] ->
            [${, key(K), $:, value(V) | do_encode_map_loop(T, false)]
    end;
value(Tuple) when is_tuple(Tuple) ->
    case tuple_to_list(Tuple) of
        [] ->
            <<"[]">>;
        [H | T] ->
            [$[, value(H) | do_encode_list_loop(T)]
    end;
value(Pid) when is_pid(Pid) ->
    Bin = list_to_binary(pid_to_list(Pid)),
    [$", escape(Bin, [], Bin, 0), $"];
value(Port) when is_port(Port) ->
    Bin = list_to_binary(port_to_list(Port)),
    [$", escape(Bin, [], Bin, 0), $"];
value(Ref) when is_reference(Ref) ->
    Bin = list_to_binary(ref_to_list(Ref)),
    [$", escape(Bin, [], Bin, 0), $"];
value(Term) ->
    throw_unsupported_type_error(Term).

do_encode_list_loop([]) ->
    [$]];
do_encode_list_loop([H | T]) ->
    [$,, value(H) | do_encode_list_loop(T)].

do_encode_map_loop([], _First) ->
    [$}];
do_encode_map_loop([{_, ?UNDEFINED} | T], First) ->
    do_encode_map_loop(T, First);
do_encode_map_loop([{K, V} | T], false) ->
    [$,, key(K), $:, value(V) | do_encode_map_loop(T, false)];
do_encode_map_loop([{K, V} | T], true) ->
    [ key(K), $:, value(V) | do_encode_map_loop(T, false)].

escape_byte(0) -> <<"\\u0000">>;
escape_byte(1) -> <<"\\u0001">>;
escape_byte(2) -> <<"\\u0002">>;
escape_byte(3) -> <<"\\u0003">>;
escape_byte(4) -> <<"\\u0004">>;
escape_byte(5) -> <<"\\u0005">>;
escape_byte(6) -> <<"\\u0006">>;
escape_byte(7) -> <<"\\u0007">>;
escape_byte($\b) -> <<"\\b">>;
escape_byte($\t) -> <<"\\t">>;
escape_byte($\n) -> <<"\\n">>;
escape_byte($\v) -> <<"\\u000B">>;
escape_byte($\f) -> <<"\\f">>;
escape_byte($\r) -> <<"\\r">>;
escape_byte(14) -> <<"\\u000E">>;
escape_byte(15) -> <<"\\u000F">>;
escape_byte(16) -> <<"\\u0010">>;
escape_byte(17) -> <<"\\u0011">>;
escape_byte(18) -> <<"\\u0012">>;
escape_byte(19) -> <<"\\u0013">>;
escape_byte(20) -> <<"\\u0014">>;
escape_byte(21) -> <<"\\u0015">>;
escape_byte(22) -> <<"\\u0016">>;
escape_byte(23) -> <<"\\u0017">>;
escape_byte(24) -> <<"\\u0018">>;
escape_byte(25) -> <<"\\u0019">>;
escape_byte(26) -> <<"\\u001A">>;
escape_byte($\e) -> <<"\\u001B">>;
escape_byte(28) -> <<"\\u001C">>;
escape_byte(29) -> <<"\\u001D">>;
escape_byte(30) -> <<"\\u001E">>;
escape_byte(31) -> <<"\\u001F">>;
escape_byte($\") -> <<"\\\"">>;
escape_byte($/) -> <<"\\/">>;
escape_byte($\\) -> <<"\\\\">>;
escape_byte(Byte) -> throw_invalid_byte_error(Byte, Byte).

throw_unsupported_type_error(Term) ->
    throw({unsupported_type, Term}).

throw_invalid_byte_error(Byte, Input) ->
    throw({invalid_byte, Byte, Input}).

%%%=====================================================================
%%% Support functions
%%%=====================================================================

maps_to_list([Iter, Map | Acc]) when is_integer(Iter) ->
    maps_to_list(erts_internal:map_next(Iter, Map, Acc));
maps_to_list(Acc) ->
    Acc.

%%%=====================================================================
%%% Eunit tests
%%%=====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    [ ?assertEqual(Expect, euneus:encode_to_binary(Input))
      || {Expect, Input} <- [
        {{ok, <<"true">>}, true},
        {{ok, <<"\"foo\"">>}, foo},
        {{ok, <<"\"foo\"">>}, <<"foo">>},
        {{ok, <<"0">>}, 0},
        {{ok, <<"123.456789">>}, 123.45678900},
        { {ok, <<"[true,0,null,null]">>}
        , [true, 0, ?NULL, ?UNDEFINED]},
        { {ok, <<"{\"a\":\"foo\",\"b\":null}">>}
        , #{a => foo, b => ?NULL, c => ?UNDEFINED} },
        {{ok, <<"{\"0\":0}">>}, #{0 => 0}}
    ]].

-endif.
