%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc JSON generator without any options but with better performance.

%% Copyright 2023 William Fank Thomé
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(euneus_smart_encoder).

-compile({ inline, encode_binary/1 }).
-compile({ inline, encode_atom/1 }).
-compile({ inline, encode_integer/1 }).
-compile({ inline, encode_float/1 }).
-compile({ inline, encode_list/1 }).
-compile({ inline, encode_map/1 }).
-compile({ inline, encode_datetime/1 }).
-compile({ inline, encode_timestamp/1 }).
-compile({ inline, encode_unhandled/1 }).
-compile({ inline, escape_json/1 }).
-compile({ inline, handle_error/3 }).
-compile({ inline, maps_to_list/1 }).
-compile({ inline_size, 100 }).

% By default, encode_unhandled/2 will raise unsupported_type exception,
% so it is a function without a local return.
% Note that parse_opts/1 is included because unhandled_encoder option
% has no local return.
-dialyzer({ no_return, encode_unhandled/1 }).

-export([ encode/1 ]).

%% Types

-type input() :: euneus_encoder:input().
-type result() :: euneus_encoder:result().

%% Macros

-define(min(X, Min), is_integer(X) andalso X >= Min).
-define(range(X, Min, Max), is_integer(X) andalso X >= Min andalso X =< Max).

-define(NON_PRINTABLE_LAST, 31).
-define(ONE_BYTE_LAST, 127).
-define(TWO_BYTE_LAST, 2_047).
-define(THREE_BYTE_LAST, 65_535).

%%%=====================================================================
%%% API functions
%%%=====================================================================

-spec encode(input()) -> result().

encode(Term) ->
    try
        {ok, value(Term)}
    catch
        Class:Reason:Stacktrace ->
            handle_error(Class, Reason, Stacktrace)
    end.

key(Atom) when is_atom(Atom) ->
    encode_binary(atom_to_binary(Atom, utf8));
key(Bin) when is_binary(Bin) ->
    encode_binary(Bin);
key(Int) when is_integer(Int) ->
    encode_binary(integer_to_binary(Int));
key(String) when is_list(String) ->
    encode_binary(list_to_binary(String)).

value(Bin) when is_binary(Bin) ->
    encode_binary(Bin);
value(Atom) when is_atom(Atom) ->
    encode_atom(Atom);
value(Int) when is_integer(Int) ->
    encode_integer(Int);
value(Float) when is_float(Float) ->
    encode_float(Float);
value(List) when is_list(List) ->
    encode_list(List);
value(Map) when is_map(Map) ->
    encode_map(Map);
value({{YYYY,MM,DD},{H,M,S}} = DateTime)
  when ?min(YYYY, 0), ?range(MM, 1, 12), ?range(DD, 1, 31)
     , ?range(H, 0, 23), ?range(M, 0, 59), ?range(S, 0, 59) ->
    encode_datetime(DateTime);
value({MegaSecs,Secs,MicroSecs} = Timestamp)
  when ?min(MegaSecs, 0), ?min(Secs, 0), ?min(MicroSecs, 0) ->
    encode_timestamp(Timestamp);
value(Term) ->
    encode_unhandled(Term).

encode_atom(true) ->
    <<"true">>;
encode_atom(false) ->
    <<"false">>;
encode_atom(undefined) ->
    <<"null">>;
encode_atom(Atom) ->
    encode_binary(atom_to_binary(Atom, utf8)).

encode_integer(Int) ->
    integer_to_binary(Int).

encode_float(Float) ->
    float_to_binary(Float, [short]).

encode_list([H | T]) ->
    [$[, value(H), do_encode_list_loop(T)];
encode_list([]) ->
    <<"[]">>.

do_encode_list_loop([H | T]) ->
    [$,, value(H), do_encode_list_loop(T)];
do_encode_list_loop([]) ->
    $].

encode_map(Map) ->
    do_encode_map(maps_to_list(Map)).

do_encode_map([{K, V} | T]) ->
    [${, key(K), $:, value(V), do_encode_map_loop(T)];
do_encode_map([]) ->
    <<"{}">>.

do_encode_map_loop([{K, V} | T]) ->
    [$,, key(K), $:, value(V), do_encode_map_loop(T)];
do_encode_map_loop([]) ->
    $}.

encode_datetime({{YYYY,MM,DD},{H,M,S}}) ->
    DateTime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [YYYY,MM,DD,H,M,S])
    ),
    encode_binary(DateTime).

encode_timestamp({_,_,MicroSecs} = Timestamp) ->
    MilliSecs = MicroSecs div 1000,
    {{YYYY,MM,DD},{H,M,S}} = calendar:now_to_datetime(Timestamp),
    DateTime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
        [YYYY,MM,DD,H,M,S,MilliSecs])
    ),
    encode_binary(DateTime).

encode_unhandled(Term) ->
    throw_unsupported_type_error(Term).

encode_binary(Bin) ->
    [$", escape_json(Bin), $"].

escape_json(Bin) ->
    escape_json(Bin, [], Bin, 0).

escape_json(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Pos)
  when Byte =< ?NON_PRINTABLE_LAST; Byte =:= $\"; Byte =:= $\\ ->
    Acc = [Acc0, escape_byte(Byte)],
    escape_json(Rest, Acc, Input, Pos+1);
escape_json(<<Byte/integer, Rest/bitstring>>, Acc, Input, Pos)
  when Byte =< ?ONE_BYTE_LAST ->
    escape_json_chunk(Rest, Acc, Input, Pos, 1);
escape_json(<<Char/utf8, Rest/bitstring>>, Acc, Input, Pos)
  when Char =< ?TWO_BYTE_LAST ->
    escape_json_chunk(Rest, Acc, Input, Pos, 2);
escape_json(<<Char/utf8, Rest/bitstring>>, Acc, Input, Pos)
  when Char =< ?THREE_BYTE_LAST ->
    escape_json_chunk(Rest, Acc, Input, Pos, 3);
escape_json(<<_Char/utf8, Rest/bitstring>>, Acc, Input, Pos) ->
    escape_json_chunk(Rest, Acc, Input, Pos, 4);
escape_json(<<>>, Acc, _Input, _Pos) ->
    Acc;
escape_json(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Pos) ->
    throw_invalid_byte_error(Byte, Input).

escape_json_chunk(<<Byte/integer, Rest/bitstring>>, Acc0, Input, Pos, Len)
  when Byte =< ?NON_PRINTABLE_LAST; Byte =:= $\"; Byte =:= $\\ ->
    Part = binary_part(Input, Pos, Len),
    Acc = [Acc0, Part, escape_byte(Byte)],
    escape_json(Rest, Acc, Input, Pos+Len+1);
escape_json_chunk(<<Byte/integer, Rest/bitstring>>, Acc, Input, Pos, Len)
  when Byte =< ?ONE_BYTE_LAST ->
    escape_json_chunk(Rest, Acc, Input, Pos, Len+1);
escape_json_chunk(<<>>, Acc, Input, Pos, Len) ->
    case Acc =:= [] of
        true ->
            binary_part(Input, Pos, Len);
        false ->
            [Acc, binary_part(Input, Pos, Len)]
    end;
escape_json_chunk(<<Char/utf8, Rest/bitstring>>, Acc, Input, Pos, Len)
  when Char =< ?TWO_BYTE_LAST ->
    escape_json_chunk(Rest, Acc, Input, Pos, Len+2);
escape_json_chunk(<<Char/utf8, Rest/bitstring>>, Acc, Input, Pos, Len)
  when Char =< ?THREE_BYTE_LAST ->
    escape_json_chunk(Rest, Acc, Input, Pos, Len+3);
escape_json_chunk(<<_Char/utf8, Rest/bitstring>>, Acc, Input, Pos, Len) ->
    escape_json_chunk(Rest, Acc, Input, Pos, Len+4);
escape_json_chunk(<<Byte/integer, _Rest/bitstring>>, _Acc, Input, _Pos, _Len) ->
    throw_invalid_byte_error(Byte, Input).

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

handle_error(throw, {unsupported_type, Unsupported}, _Stacktrace) ->
    {error, {unsupported_type, Unsupported}};
handle_error(throw, {invalid_byte, Byte0, Input}, _Stacktrace) ->
    Byte = <<"0x"/utf8, (integer_to_binary(Byte0, 16))/binary>>,
    {error, {invalid_byte, Byte, Input}};
handle_error(throw, Reason, _Stacktrace) ->
    {error, Reason};
handle_error(Class, Reason, Stacktrace) ->
    erlang:raise(Class, Reason, Stacktrace).

%%%=====================================================================
%%% Support functions
%%%=====================================================================

maps_to_list(Map) ->
    do_maps_to_list(erts_internal:map_next(0, Map, [])).

do_maps_to_list([Iter, Map | Acc]) when is_integer(Iter) ->
    do_maps_to_list(erts_internal:map_next(Iter, Map, Acc));
do_maps_to_list(Acc) ->
    Acc.

%%%=====================================================================
%%% Eunit tests
%%%=====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    [ ?assertEqual(Expect, euneus:encode_to_binary(Input, Opts))
      || {Expect, Input, Opts} <- [
        {{ok, <<"true">>}, true, #{}},
        {{ok, <<"\"foo\"">>}, foo, #{}},
        {{ok, <<"\"foo\"">>}, <<"foo">>, #{}},
        {{ok, <<"0">>}, 0, #{}},
        {{ok, <<"123.456789">>}, 123.45678900, #{}},
        {{ok, <<"[true,0,null]">>}, [true, 0, undefined], #{}},
        {{ok, <<"{\"foo\":\"bar\"}">>}, #{foo => bar}, #{}},
        {{ok, <<"{\"0\":0}">>}, #{0 => 0}, #{}},
        {{ok, <<"\"1970-01-01T00:00:00Z\"">>}, {{1970,1,1},{0,0,0}}, #{}},
        {{ok, <<"\"1970-01-01T00:00:00.000Z\"">>}, {0,0,0}, #{}}
    ]].

-endif.
