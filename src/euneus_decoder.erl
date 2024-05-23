%%%---------------------------------------------------------------------
%%% @copyright 2023-2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc JSON parser.
%%%
%%% Copyright 2023-2024 William Fank Thomé
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @end
%%%---------------------------------------------------------------------
-module(euneus_decoder).

-dialyzer({no_opaque, [decode/2, decode_continue/2, parse_opts/1]}).
-dialyzer({no_return, [decode_continue/2]}).

-export([decode/1, decode/2, parse_opts/1]).

-define(NULL, null).

-define(range(X, Min, Max), (
    is_integer(X) andalso X >= Min andalso X =< Max
)).
-define(is_number(X), (
    X >= $0 andalso X =< $9
)).

-record(state, {decode}).

-type codec(Type) :: fun((Type) -> term()).
-type array_finish_fun() :: fun((list(), list()) -> {term(), list()}).
-type object_finish_fun() :: fun((proplist(), proplist()) -> {term(), proplist()}).
-type proplist() :: proplists:proplist().
-type array_codec() :: codec(list()).
-type object_codec() :: codec(map()).
-type key_codec() :: datetime
                   | timestamp
                   | inet
                   | ipv4
                   | ipv6
                   | pid
                   | port
                   | reference
                   | copy
                   | atom
                   | existing_atom
                   | integer
                   | {integer, Base :: 2..36}
                   | float
                   | list
                   | codec(binary()).
-type value_codec() :: datetime
                     | timestamp
                     | inet
                     | ipv4
                     | ipv6
                     | pid
                     | port
                     | reference
                     | copy
                     | atom
                     | existing_atom
                     | integer
                     | {integer, Base :: 2..36}
                     | float
                     | list
                     | codec(binary()).
-type options() :: #{ null => term()
                    , array => array_codec() | [array_codec()]
                    , object => object_codec() | [object_codec()]
                    , key => key_codec() | [key_codec()]
                    , value => value_codec() | [value_codec()]
                    , array_finish => ordered
                                    | reversed
                                    | array_finish_fun()
                    , object_finish => map
                                     | proplist
                                     | reversed_proplist
                                     | object_finish_fun()
                    , binary_to_integer => fun((binary()) -> integer())
                    , binary_to_float => fun((binary()) -> float())
                    }.

%%%=====================================================================
%%% API functions
%%%=====================================================================

-spec decode(iodata()) -> term().

decode(Bin) when is_binary(Bin) ->
    json:decode(Bin);
decode(Str) when is_list(Str) ->
    decode(iolist_to_binary(Str)).

-spec decode(iodata(), #state{} | options()) -> term().

decode(Bin, #state{decode = Decode}) when is_binary(Bin) ->
    Decode(Bin);
decode(Bin, Opts) when is_binary(Bin) ->
    decode(Bin, parse_opts(Opts));
decode(Str, Opts) when is_list(Str) ->
    decode(iolist_to_binary(Str), Opts).

-spec parse_opts(options()) -> #state{}.

parse_opts(Opts) when is_map(Opts) ->
    ArrCodecs = normalize_array_codecs(get_codecs(array, Opts)),
    ObjCodecs = normalize_object_codecs(get_codecs(object, Opts)),
    KeyCodecs = normalize_key_codecs(get_codecs(key, Opts)),
    ValCodecs = normalize_value_codecs(get_codecs(value, Opts)),
    Decoders = #{
        null => maps:get(null, Opts, null),
        array_push => case ValCodecs =:= [] of
            true ->
                fun(Elem, Acc) -> [Elem | Acc] end;
            false ->
                fun(Elem, Acc) -> [traverse_codecs(ValCodecs, Elem) | Acc] end
        end,
        array_finish => case maps:get(array_finish, Opts, ordered) of
            ordered ->
                case ArrCodecs =:= [] of
                    true ->
                        fun(Acc, OldAcc) -> {lists:reverse(Acc), OldAcc} end;
                    false ->
                        fun(Acc, OldAcc) -> {traverse_codecs(ArrCodecs, lists:reverse(Acc)), OldAcc} end
                end;
            reversed ->
                case ArrCodecs =:= [] of
                    true ->
                        fun(Acc, OldAcc) -> {Acc, OldAcc} end;
                    false ->
                        fun(Acc, OldAcc) -> {traverse_codecs(ArrCodecs, Acc), OldAcc} end
                end;
            ArrFinish when is_function(ArrFinish, 2) ->
                ArrFinish
        end,
        object_push => case {KeyCodecs, ValCodecs} of
            {[], []} ->
                fun(Key, Value, Acc) -> [{Key, Value} | Acc] end;
            {KeyCodecs, []} ->
                fun(Key, Value, Acc) ->
                    [{traverse_codecs(KeyCodecs, Key), Value} | Acc]
                end;
            {[], ValCodecs} ->
                fun(Key, Value, Acc) ->
                    [{Key, traverse_codecs(ValCodecs, Value)} | Acc]
                end;
            {KeyCodecs, ValCodecs} ->
                fun(Key, Value, Acc) ->
                    [{ traverse_codecs(KeyCodecs, Key)
                    ,  traverse_codecs(ValCodecs, Value)} | Acc]
                end
        end,
        object_finish => case maps:get(object_finish, Opts, map) of
            map ->
                case ObjCodecs =:= [] of
                    true ->
                        fun(Acc, OldAcc) -> {maps:from_list(Acc), OldAcc} end;
                    false ->
                        fun(Acc, OldAcc) -> {traverse_codecs(ObjCodecs, maps:from_list(Acc)), OldAcc} end
                end;
            proplist ->
                case ArrCodecs =:= [] of
                    true ->
                        fun(Acc, OldAcc) -> {lists:reverse(Acc), OldAcc} end;
                    false ->
                        fun(Acc, OldAcc) -> {traverse_codecs(ArrCodecs, lists:reverse(Acc)), OldAcc} end
                end;
            reversed_proplist ->
                case ArrCodecs =:= [] of
                    true ->
                        fun(Acc, OldAcc) -> {Acc, OldAcc} end;
                    false ->
                        fun(Acc, OldAcc) -> {traverse_codecs(ArrCodecs, Acc), OldAcc} end
                end;
            ObjFinish when is_function(ObjFinish, 2) ->
                ObjFinish
        end,
        integer => maps:get(binary_to_integer, Opts, fun erlang:binary_to_integer/1),
        float => maps:get(binary_to_float, Opts, fun erlang:binary_to_float/1)
    },
    Decode = case ValCodecs =:= [] of
        true ->
            fun(Bin) ->
                case json:decode(Bin, [], Decoders) of
                    {Result, [], <<>>} ->
                        Result;
                    {_, _, Rest} ->
                        invalid_byte(Rest, 0)
                end
            end;
        false ->
            fun(Bin) ->
                case json:decode_start(Bin, [], Decoders) of
                    {Result, [], _} ->
                        traverse_codecs(ValCodecs, Result);
                    {_, _, Rest} ->
                        invalid_byte(Rest, 0);
                    {continue, State} ->
                        decode_continue(Bin, State)
                end
            end
    end,
    #state{decode = Decode}.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

decode_continue(Cont, State) ->
    case json:decode_continue(Cont, State) of
        {Result, [], _} ->
            Result;
        {_, _, Rest} ->
            invalid_byte(Rest, 0);
        {continue, NewState} ->
            decode_continue(Cont, NewState)
    end.

%%
%% Errors
%%

% That's just a copy of json:invalid_byte/2.
% It is not exported by the json module.
invalid_byte(Bin, Skip) ->
    Byte = binary:at(Bin, Skip),
    error({invalid_byte, Byte}, none, error_info(Skip)).

error_info(Skip) ->
    [{error_info, #{cause => #{position => Skip}}}].

%%
%% Codecs
%%

-spec get_codecs(atom(), options()) -> [codec(term())].

get_codecs(Name, Opts) ->
    case maps:get(Name, Opts, []) of
        Codecs when is_list(Codecs) ->
            Codecs;
        Codec ->
            [Codec]
    end.

traverse_codecs([Codec | Codecs], Term) ->
    case codec(Codec, Term) of
        next ->
            traverse_codecs(Codecs, Term);
        {halt, NewTerm} ->
            NewTerm
    end;
traverse_codecs(_, Term) ->
    Term.

codec({Codec, Opts}, Term) ->
    Codec(Term, Opts);
codec(Codec, Term) ->
    Codec(Term).

normalize_array_codecs(Codecs) when is_list(Codecs) ->
    lists:map(fun parse_array_codec/1, Codecs).

parse_array_codec(Fun) when is_function(Fun, 1) ->
    Fun;
parse_array_codec({Fun, Opts}) when is_function(Fun, 2) ->
    {Fun, Opts}.

normalize_object_codecs(Codecs) when is_list(Codecs) ->
    lists:map(fun parse_object_codec/1, Codecs).

parse_object_codec(Fun) when is_function(Fun, 1) ->
    Fun;
parse_object_codec({Fun, Opts}) when is_function(Fun, 2) ->
    {Fun, Opts}.

normalize_key_codecs(Codecs) when is_list(Codecs) ->
    lists:map(fun parse_codec/1, Codecs).

normalize_value_codecs(Codecs) when is_list(Codecs) ->
    lists:map(fun parse_codec/1, Codecs).

parse_codec(datetimestamp) ->
    fun datetimestamp_codec/1;
parse_codec(datetime) ->
    % TODO: Check when not lists:member(datetimestamp, Codecs) or drop it.
    fun datetime_codec/1;
parse_codec(timestamp) ->
    % TODO: Check when not lists:member(datetimestamp, Codecs) or drop it.
    fun timestamp_codec/1;
parse_codec(inet) ->
    fun inet_codec/1;
parse_codec(ipv4) ->
    % TODO: Check when not lists:member(inet, Codecs) or drop it.
    fun ipv4_codec/1;
parse_codec(ipv6) ->
    % TODO: Check when not lists:member(inet, Codecs) or drop it.
    fun ipv6_codec/1;
parse_codec(pid) ->
    fun pid_codec/1;
parse_codec(port) ->
    fun port_codec/1;
parse_codec(reference) ->
    fun reference_codec/1;
parse_codec(copy) ->
    fun copy_codec/1;
parse_codec(atom) ->
    fun atom_codec/1;
parse_codec(existing_atom) ->
    fun existing_atom_codec/1;
parse_codec(integer) ->
    fun integer_codec/1;
parse_codec({integer, Base}) ->
    {fun integer_codec/2, Base};
parse_codec(float) ->
    fun float_codec/1;
parse_codec(list) ->
    fun list_codec/1;
% TODO: Check if is possible to convert binary to Erlang term via erlang:binary_to_term/1.
% parse_codec(term) ->
%     fun term_codec/1;
parse_codec(Fun) when is_function(Fun, 1) ->
    Fun;
parse_codec({Fun, Opts}) when is_function(Fun, 2) ->
    {Fun, Opts}.

% datetimestamp is a union of datetime and timestamp.
% Exists to boost performance.
datetimestamp_codec(
    << Y4/integer, Y3/integer, Y2/integer, Y1/integer, $-/integer
     , M2/integer, M1/integer, $-/integer
     , D2/integer, D1/integer
     , $T/integer
     , H2/integer, H1/integer, $:/integer
     , Min2/integer, Min1/integer, $:/integer
     , S2/integer, S1/integer
     , Rest/bitstring >>)
    when ?is_number(Y4), ?is_number(Y3), ?is_number(Y2), ?is_number(Y1)
       , ?is_number(M2), ?is_number(M1)
       , ?is_number(D2), ?is_number(D1)
       , ?is_number(H2), ?is_number(H1)
       , ?is_number(Min2), ?is_number(Min1)
       , ?is_number(S2), ?is_number(S1) ->
    case Rest of
        << $Z/integer >> ->
            {halt, datetime(Y4, Y3, Y2, Y1, M2, M1, D2, D1, H2, H1, Min2, Min1, S2, S1)};
        << $./integer, MSec3/integer, MSec2/integer, MSec1/integer, $Z/integer>>
        when ?is_number(MSec3), ?is_number(MSec2), ?is_number(MSec1) ->
            {halt, timestamp(Y4, Y3, Y2, Y1, M2, M1, D2, D1, H2, H1, Min2, Min1, S2, S1, MSec3, MSec2, MSec1)}
    end;
datetimestamp_codec(_) ->
    next.

datetime_codec(
    << Y4/integer, Y3/integer, Y2/integer, Y1/integer, $-/integer
     , M2/integer, M1/integer, $-/integer
     , D2/integer, D1/integer
     , $T/integer
     , H2/integer, H1/integer, $:/integer
     , Min2/integer, Min1/integer, $:/integer
     , S2/integer, S1/integer
     , $Z/integer >>)
    when ?is_number(Y4), ?is_number(Y3), ?is_number(Y2), ?is_number(Y1)
       , ?is_number(M2), ?is_number(M1)
       , ?is_number(D2), ?is_number(D1)
       , ?is_number(H2), ?is_number(H1)
       , ?is_number(Min2), ?is_number(Min1)
       , ?is_number(S2), ?is_number(S1) ->
    {halt, datetime(Y4, Y3, Y2, Y1, M2, M1, D2, D1, H2, H1, Min2, Min1, S2, S1)};
datetime_codec(_) ->
    next.

timestamp_codec(
    << Y4/integer, Y3/integer, Y2/integer, Y1/integer, $-/integer
     , M2/integer, M1/integer, $-/integer
     , D2/integer, D1/integer
     , $T/integer
     , H2/integer, H1/integer, $:/integer
     , Min2/integer, Min1/integer, $:/integer
     , S2/integer, S1/integer
     , $./integer
     , MSec3/integer, MSec2/integer, MSec1/integer
     , $Z/integer >>)
    when ?is_number(Y4), ?is_number(Y3), ?is_number(Y2), ?is_number(Y1)
       , ?is_number(M2), ?is_number(M1)
       , ?is_number(D2), ?is_number(D1)
       , ?is_number(H2), ?is_number(H1)
       , ?is_number(Min2), ?is_number(Min1)
       , ?is_number(S2), ?is_number(S1)
       , ?is_number(MSec3), ?is_number(MSec2), ?is_number(MSec1) ->
    {halt, timestamp(Y4, Y3, Y2, Y1, M2, M1, D2, D1, H2, H1, Min2, Min1, S2, S1, MSec3, MSec2, MSec1)};
timestamp_codec(_) ->
    next.

datetime(Y4, Y3, Y2, Y1, M2, M1, D2, D1, H2, H1, Min2, Min1, S2, S1) ->
    Date = { chars_to_integer(Y4, Y3, Y2, Y1)
           , chars_to_integer(M2, M1)
           , chars_to_integer(D2, D1) },
    Time = { chars_to_integer(H2, H1)
           , chars_to_integer(Min2, Min1)
           , chars_to_integer(S2, S1) },
    {Date, Time}.

timestamp(Y4, Y3, Y2, Y1, M2, M1, D2, D1, H2, H1, Min2, Min1, S2, S1, MSec3, MSec2, MSec1) ->
    Datetime = datetime(Y4, Y3, Y2, Y1, M2, M1, D2, D1, H2, H1, Min2, Min1, S2, S1),
    GregSeconds = calendar:datetime_to_gregorian_seconds(Datetime),
    Seconds = GregSeconds - 62167219200,
    MilliSeconds = chars_to_integer(MSec3, MSec2, MSec1),
    {Seconds div 1000000, Seconds rem 1000000, MilliSeconds * 1000}.

chars_to_integer(N2, N1) ->
    ((N2 - $0) * 10) + (N1 - $0).

chars_to_integer(N3, N2, N1) ->
    ((N3 - $0) * 100) + ((N2 - $0) * 10) + (N1 - $0).

chars_to_integer(N4, N3, N2, N1) ->
    ((N4 - $0) * 1000) + ((N3 - $0) * 100) + ((N2 - $0) * 10) + (N1 - $0).

inet_codec(Bin) when is_binary(Bin) ->
    case ipv4_codec(Bin) of
        next ->
            ipv6_codec(Bin);
        {halt, IPv4} ->
            {halt, IPv4}
    end;
inet_codec(_) ->
    next.

ipv4_codec(
    << A/integer, B/integer, C/integer, $.
     , _/bitstring >> = Bin)
  when ?range(A, 0, 255); ?range(B, 0, 255); ?range(C, 0, 255) ->
    case inet_parse:ipv4_address(binary_to_list(Bin)) of
        {ok, IPv4} ->
            {halt, IPv4};
        {error, einval} ->
            next
    end;
ipv4_codec(<<A/integer, B/integer, $., _/bitstring>> = Bin)
  when ?range(A, 0, 255); ?range(B, 0, 255) ->
    case inet_parse:ipv4_address(binary_to_list(Bin)) of
        {ok, IPv4} ->
            {halt, IPv4};
        {error, einval} ->
            next
    end;
ipv4_codec(<<A/integer, $., _/bitstring>> = Bin)
  when ?range(A, 0, 255) ->
    case inet_parse:ipv4_address(binary_to_list(Bin)) of
        {ok, IPv4} ->
            {halt, IPv4};
        {error, einval} ->
            next
    end;
ipv4_codec(_) ->
    next.

ipv6_codec(<<$:, $:>>) ->
    {halt, {0,0,0,0,0,0,0,0}};
ipv6_codec(<<$:, $:, _/bitstring>> = Bin) ->
    case inet_parse:ipv6strict_address(binary_to_list(Bin)) of
        {ok, Ipv6} ->
            {halt, Ipv6};
        {error, einval} ->
            next
    end;
ipv6_codec(
    << _/integer, _/integer, _/integer, _/integer, $:
     , _/bitstring>> = Bin) ->
    case inet_parse:ipv6strict_address(binary_to_list(Bin)) of
        {ok, Ipv6} ->
            {halt, Ipv6};
        {error, einval} ->
            next
    end;
ipv6_codec(_) ->
    next.

pid_codec(<<$<, _/bitstring>> = Bin) ->
    try {halt, list_to_pid(binary_to_list(Bin))}
    catch _:_ -> next end;
pid_codec(_) ->
    next.

port_codec(<<"#Port<", _/bitstring>> = Bin) ->
    try {halt, list_to_port(binary_to_list(Bin))}
    catch _:_ -> next end;
port_codec(_) ->
    next.

reference_codec(<<"#Ref<", _/bitstring>> = Bin) ->
    try {halt, list_to_ref(binary_to_list(Bin))}
    catch _:_ -> next end;
reference_codec(_) ->
    next.

copy_codec(Bin) when is_binary(Bin) ->
    {halt, binary:copy(Bin)};
copy_codec(_) ->
    next.

atom_codec(Bin) when is_binary(Bin) ->
    {halt, binary_to_atom(Bin)};
atom_codec(_) ->
    next.

existing_atom_codec(Bin) when is_binary(Bin) ->
    {halt, binary_to_existing_atom(Bin)};
existing_atom_codec(_) ->
    next.

integer_codec(Bin) when is_binary(Bin) ->
    {halt, binary_to_integer(Bin)};
integer_codec(_) ->
    next.

integer_codec(Bin, Base) when is_binary(Bin) ->
    {halt, binary_to_integer(Bin, Base)};
integer_codec(_, _) ->
    next.

float_codec(Bin) when is_binary(Bin) ->
    {halt, binary_to_float(Bin)};
float_codec(_) ->
    next.

list_codec(Bin) when is_binary(Bin) ->
    {halt, binary_to_list(Bin)};
list_codec(_) ->
    next.

%%%=====================================================================
%%% Tests
%%%=====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

decode_test() ->
    [ ?assertEqual(Expect, decode(Input, Opts))
      || {Expect, Input, Opts} <- [
        {<<"foo">>, <<"\"foo\"">>, #{}},
        {123, <<"123">>, #{}},
        {1.234, <<"1.234">>, #{}},
        {6.02e23, <<"6.02e+23">>, #{}},
        { [<<"foo">>, 123, 6.02e+23, true]
        , <<"[\"foo\",123,6.02e+23,true]">>, #{}
        },
        { #{<<"foo">> => <<"bar">>, <<"bar">> => <<"baz">>}
        , <<"{\"foo\": \"bar\", \"bar\": \"baz\"}">>
        , #{}
        },
        {true, <<"true">>, #{}},
        {false, <<"false">>, #{}},
        {?NULL, <<"null">>, #{}},
        {<<"ABC">>, <<"\"\\u0041\\u0042\\u0043\"">>, #{}},
        { #{foo => 1}
        , <<"{\"foo\": \"1\"}">>
        , #{key => atom, value => integer}
        },
        { #{keya => <<"valuea">>}
        , <<"{\"key\\u0061\": \"value\\u0061\"}">>
        , #{key => atom}
        },
        { #{<<"keya">> => valuea}
        , <<"{\"key\\u0061\": \"value\\u0061\"}">>
        , #{value => atom}
        },
        { #{keya => valuea}
        , <<"{\"key\\u0061\": \"value\\u0061\"}">>
        , #{key => atom, value => atom}
        }
    ]].

% TODO: datetimestamp_codec_test

datetime_codec_test() ->
    [ ?assertEqual(Expect, decode(Input, Opts))
        || {Expect, Input, Opts} <- [
        {<<"1970-01-01T00:00:00Z">>, <<"\"1970-01-01T00:00:00Z\"">>, #{}},
        {{{1970,1,1},{0,0,0}}, <<"\"1970-01-01T00:00:00Z\"">>, #{value => datetime}}
    ]].

timestamp_codec_test() ->
    [ ?assertEqual(Expect, decode(Input, Opts))
        || {Expect, Input, Opts} <- [
        {<<"1970-01-01T00:00:00.000Z">>, <<"\"1970-01-01T00:00:00.000Z\"">>, #{}},
        {{0,0,0}, <<"\"1970-01-01T00:00:00.000Z\"">>, #{value => timestamp}}
    ]].

% TODO: inet_codec_test

ipv4_codec_test() ->
    [ ?assertEqual(Expect, decode(Input, Opts))
      || {Expect, Input, Opts} <- [
        {<<"0.0.0.0">>, <<"\"0.0.0.0\"">>, #{}},
        {{0,0,0,0}, <<"\"0.0.0.0\"">>, #{value => ipv4}},
        {{255,255,255,255}, <<"\"255.255.255.255\"">>, #{value => ipv4}}
    ]].

ipv6_codec_test() ->
    [ ?assertEqual(Expect, decode(Input, Opts))
      || {Expect, Input, Opts} <- [
        {<<"::">>, <<"\"::\"">>, #{}},
        {{0,0,0,0,0,0,0,0}, <<"\"::\"">>, #{value => ipv6}},
        {{0,0,0,0,0,0,0,1}, <<"\"::1\"">>, #{value => ipv6}},
        { {0,0,0,0,0,0,(192 bsl 8) bor 168,(42 bsl 8) bor 2}
        , <<"\"::192.168.42.2\"">>
        , #{value => ipv6}
        },
        { {0,0,0,0,0,16#FFFF,(192 bsl 8) bor 168,(42 bsl 8) bor 2}
        , <<"\"::ffff:192.168.42.2\"">>
        , #{value => ipv6}
        },
        { {16#3ffe,16#b80,16#1f8d,16#2,16#204,16#acff,16#fe17,16#bf38}
        , <<"\"3ffe:b80:1f8d:2:204:acff:fe17:bf38\"">>
        , #{value => ipv6}
        },
        { {16#fe80,0,0,0,16#204,16#acff,16#fe17,16#bf38}
        , <<"\"fe80::204:acff:fe17:bf38\"">>
        , #{value => ipv6}
        }
    ]].

pid_codec_test() ->
    [ ?assertEqual(Expect, decode(Input, Opts))
        || {Expect, Input, Opts} <- [
        {<<"<0.92.0>">>, <<"\"<0.92.0>\"">>, #{}},
        {list_to_pid("<0.92.0>"), <<"\"<0.92.0>\"">>, #{value => pid}}
    ]].

port_codec_test() ->
    [ ?assertEqual(Expect, decode(Input, Opts))
        || {Expect, Input, Opts} <- [
        {<<"#Port<0.1>">>, <<"\"#Port<0.1>\"">>, #{}},
        {list_to_port("#Port<0.1>"), <<"\"#Port<0.1>\"">>, #{value => port}}
    ]].

reference_codec_test() ->
    [ ?assertEqual(Expect, decode(Input, Opts))
        || {Expect, Input, Opts} <- [
        { <<"#Ref<0.314572725.1088159747.110918>">>
        , <<"\"#Ref<0.314572725.1088159747.110918>\"">>
        , #{}
        },
        { list_to_ref("#Ref<0.314572725.1088159747.110918>")
        , <<"\"#Ref<0.314572725.1088159747.110918>\"">>
        , #{value => reference}
        }
    ]].

% copy_codec_test() ->
    % copy is already tested in ./euneus_test/test/euneus_decoder_test.exs

atom_codec_test() ->
    [ ?assertEqual(Expect, decode(Input, Opts))
        || {Expect, Input, Opts} <- [
        {foo, <<"\"foo\"">>, #{value => atom}}
    ]].

existing_atom_codec_test() ->
    [ ?assertEqual(Expect, decode(Input, Opts))
        || {Expect, Input, Opts} <- [
        {foo, <<"\"foo\"">>, #{value => existing_atom}}
    ]].

integer_codec_test() ->
    [ ?assertEqual(Expect, decode(Input, Opts))
        || {Expect, Input, Opts} <- [
        {1, <<"\"1\"">>, #{value => integer}}
    ]].

float_codec_test() ->
    [ ?assertEqual(Expect, decode(Input, Opts))
        || {Expect, Input, Opts} <- [
        {1.0, <<"\"1.0\"">>, #{value => float}}
    ]].

list_codec_test() ->
    [ ?assertEqual(Expect, decode(Input, Opts))
        || {Expect, Input, Opts} <- [
        {"foo", <<"\"foo\"">>, #{value => list}}
    ]].

type_and_codecs_test() ->
    Codecs = [ datetime
             , timestamp
             , inet
             , pid
             , port
             , reference
             , copy
             , atom
             , existing_atom
             , integer
             , float
             , list
             ],
    [ ?assertEqual(Expect, decode(Input, Opts#{value => Codecs}))
        || {Expect, Input, Opts} <- [
        {<<"foo">>, <<"\"foo\"">>, #{}},
        {<<"foo">>, "\"foo\"", #{}},
        {0, <<"0">>, #{}},
        {0.0, <<"0.0">>, #{}},
        {#{}, <<"{}">>, #{}},
        {[], <<"[]">>, #{}}
    ]].

-endif.
