%%%---------------------------------------------------------------------
%%% @copyright 2023-2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc JSON generator.
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
-module(euneus_encoder).

-dialyzer({no_return, [parse_opts/1]}).

-export([encode/1, encode/2, parse_opts/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(NULLS, [null]).

-define(min(X, Min), (
    is_integer(X) andalso X >= Min
)).

-define(in_range(X, Min, Max), (
    is_integer(X) andalso X >= Min andalso X =< Max)
).

-record(state, { escape
               , encode_atom
               , encode_binary
               , encode_integer
               , encode_float
               , encode_list
               , encode_map
               , encode_tuple
               , encode_pid
               , encode_port
               , encode_ref
               , integer_to_binary
               , float_to_binary
               }).

-type encode() :: fun((term()) -> iodata()).
-type encode_callback(Type) :: fun((Type, encode(), #state{}) -> iodata()).
-type codec(Type) :: fun((Type) -> iodata()).
-type atom_codec() :: codec(atom()).
-type binary_codec() :: codec(binary()).
-type integer_codec() :: codec(integer()).
-type float_codec() :: codec(float()).
-type list_codec() :: proplist
                    | {proplist, IsProplist :: fun((list()) -> boolean())}
                    | codec(list()).
-type map_codec() :: drop_nulls | codec(map()) | [codec(map())].
-type tuple_codec() :: datetime
                     | timestamp
                     | ipv4
                     | ipv6
                     | {records, [{Name :: atom(), Fields :: [atom()]}]}
                     | codec(tuple()).
-type pid_codec() :: codec(pid()).
-type port_codec() :: codec(port()).
-type reference_codec() :: codec(reference()).
-type options() :: #{ escape => fun((binary()) -> iodata())
                    , nulls => [term()]
                    , sort_keys => boolean()
                    , atom => atom_codec() | [atom_codec()]
                    , binary => binary_codec() | [binary_codec()]
                    , integer => integer_codec() | [integer_codec()]
                    , float => float_codec() | [float_codec()]
                    , list => list_codec() | [list_codec()]
                    , map => map_codec() | [map_codec()]
                    , tuple => tuple_codec() | [tuple_codec()]
                    , pid => pid_codec() | [pid_codec()]
                    , port => port_codec() | [port_codec()]
                    , reference => reference_codec() | [reference_codec()]
                    , encode_atom => encode_callback(atom())
                    , encode_binary => encode_callback(binary())
                    , encode_integer => encode_callback(integer())
                    , encode_float => encode_callback(float())
                    , encode_list => encode_callback(list())
                    , encode_map => encode_callback(map())
                    , encode_tuple => encode_callback(tuple())
                    , encode_pid => encode_callback(pid())
                    , encode_port => encode_callback(port())
                    , encode_reference => encode_callback(reference())
                    , integer_to_binary => fun((integer()) -> binary())
                    , float_to_binary => fun((float()) -> binary())
                    }.

%%%=====================================================================
%%% API functions
%%%=====================================================================

-spec encode(term()) -> iodata().

encode(Term) ->
    json:encode(Term).

-spec encode(term(), #state{} | options()) -> iodata().

encode(Term, #state{} = State) ->
    do_encode(Term, State);
encode(Term, Opts) ->
    do_encode(Term, parse_opts(Opts)).

-spec parse_opts(options()) -> #state{}.

parse_opts(Opts) when is_map(Opts) ->
    % TODO: Escape html, javascript, and unicode.
    Escape = maps:get(escape, Opts, fun json:encode_binary/1),
    NullValues = maps:get(nulls, Opts, ?NULLS),
    Nulls = maps:from_keys(NullValues, null),
    IntToBin = maps:get(integer_to_binary, Opts, fun erlang:integer_to_binary/1),
    FloatToBin = maps:get(float_to_binary, Opts, fun(Float) ->
        erlang:float_to_binary(Float, [short])
    end),
    #state{
        escape = Escape,
        encode_atom = encode_callback(
            normalize_atom_codecs(get_codecs(atom, Opts)),
            maps:get(encode_atom, Opts, fun(Atom, _Encode, _State) ->
                encode_atom(Atom, Nulls, Escape)
            end)
        ),
        encode_binary = encode_callback(
            normalize_binary_codecs(get_codecs(binary, Opts)),
            maps:get(encode_binary, Opts, fun(Bin, _Encode, _State) ->
                Escape(Bin)
            end)
        ),
        encode_integer = encode_callback(
            normalize_integer_codecs(get_codecs(integer, Opts)),
            maps:get(encode_integer, Opts, fun(Int, _Encode, _State) ->
                IntToBin(Int)
            end)
        ),
        encode_float = encode_callback(
            normalize_float_codecs(get_codecs(float, Opts)),
            maps:get(encode_float, Opts, fun(Float, _Encode, _State) ->
                FloatToBin(Float)
            end)
        ),
        encode_list = encode_callback(
            normalize_list_codecs(get_codecs(list, Opts)),
            maps:get(encode_list, Opts, fun(List, Encode, _State) ->
                json:encode_list(List, Encode)
            end)
        ),
        encode_map = encode_callback(
            normalize_map_codecs(get_codecs(map, Opts), Nulls),
            maps:get(encode_map, Opts,
                case maps:get(sort_keys, Opts, false) of
                    true ->
                        fun(Map, Encode, State) ->
                            encode_sorted_map(Map, Encode, State)
                        end;
                    false ->
                        fun(Map, Encode, State) ->
                            encode_map(Map, Encode, State)
                        end
                end
            )
        ),
        encode_tuple = encode_callback(
            normalize_tuple_codecs(get_codecs(tuple, Opts)),
            maps:get(encode_tuple, Opts, fun(Unsupported, _Encode, _State) ->
                unsupported_type_error(Unsupported)
            end)
        ),
        encode_pid = encode_callback(
            normalize_pid_codecs(get_codecs(pid, Opts)),
            maps:get(encode_pid, Opts, fun(Unsupported, _Encode, _State) ->
                unsupported_type_error(Unsupported)
            end)
        ),
        encode_port = encode_callback(
            normalize_port_codecs(get_codecs(port, Opts)),
            maps:get(encode_port, Opts, fun(Unsupported, _Encode, _State) ->
                unsupported_type_error(Unsupported)
            end)
        ),
        encode_ref = encode_callback(
            normalize_reference_codecs(get_codecs(reference, Opts)),
            maps:get(encode_ref, Opts, fun(Unsupported, _Encode, _State) ->
                unsupported_type_error(Unsupported)
            end)
        ),
        integer_to_binary = IntToBin,
        float_to_binary = FloatToBin
    }.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

%%
%% Encode
%%

encode_callback([], Next) when is_function(Next, 3) ->
    Next;
encode_callback(Codecs, Next) when is_list(Codecs) ->
    fun(Term, Encode, State) ->
        traverse_codecs(Codecs, Next, Term, Encode, State)
    end.

do_encode(InitialTerm, State) ->
    json:encode(InitialTerm, fun(Term, Encode) ->
        value(Term, Encode, State)
    end).

key(Key, Encode, State = #state{encode_binary = EncodeBinary})
    when is_binary(Key) ->
    EncodeBinary(Key, Encode, State);
key(Key, Encode, State = #state{encode_binary = EncodeBinary})
    when is_atom(Key) ->
    EncodeBinary(atom_to_binary(Key, utf8), Encode, State);
key(Key, Encode, State = #state{encode_binary = EncodeBinary})
    when is_list(Key) ->
    EncodeBinary(iolist_to_binary(Key), Encode, State);
key(Key, Encode, State = #state{encode_binary = EncodeBinary})
    when is_integer(Key) ->
    IntToBin = State#state.integer_to_binary,
    EncodeBinary(IntToBin(Key), Encode, State);
key(Key, Encode, State = #state{encode_binary = EncodeBinary})
    when is_float(Key) ->
    FloatToBin = State#state.float_to_binary,
    EncodeBinary(FloatToBin(Key), Encode, State).

value(Atom, Encode, State = #state{encode_atom = EncodeAtom})
    when is_atom(Atom) ->
    EncodeAtom(Atom, Encode, State);
value(Bin, Encode, State = #state{encode_binary = EncodeBinary})
    when is_binary(Bin) ->
    EncodeBinary(Bin, Encode, State);
value(Int, Encode, State = #state{encode_integer = EncodeInt})
    when is_integer(Int) ->
    EncodeInt(Int, Encode, State);
value(Float, Encode, State = #state{encode_float = EncodeFloat})
    when is_float(Float) ->
    EncodeFloat(Float, Encode, State);
value(List, Encode, State = #state{encode_list = EncodeList})
    when is_list(List) ->
    EncodeList(List, Encode, State);
value(Map, Encode, State = #state{encode_map = EncodeMap})
    when is_map(Map) ->
    EncodeMap(Map, Encode, State);
value(Tuple, Encode, State = #state{encode_tuple = EncodeTuple})
    when is_tuple(Tuple) ->
    EncodeTuple(Tuple, Encode, State);
value(PID, Encode, State = #state{encode_pid = EncodePID})
    when is_pid(PID) ->
    EncodePID(PID, Encode, State);
value(Port, Encode, State = #state{encode_port = EncodePort})
    when is_port(Port) ->
    EncodePort(Port, Encode, State);
value(Ref, Encode, State = #state{encode_ref = EncodeRef})
    when is_reference(Ref) ->
    EncodeRef(Ref, Encode, State);
value(Unsupported, _Encode, _State) ->
    unsupported_type_error(Unsupported).

%%
%% Errors
%%

unsupported_type_error(Unsupported) ->
    error({unsupported_type, Unsupported}).

%%
%% Custom encoders
%%

encode_atom(true, _Nulls, _Escape) ->
    <<"true">>;
encode_atom(false, _Nulls, _Escape) ->
    <<"false">>;
encode_atom(Atom, Nulls, _Escape) when is_map_key(Atom, Nulls) ->
    <<"null">>;
encode_atom(Atom, _Nulls, Escape) ->
    Escape(atom_to_binary(Atom, utf8)).

encode_map(Map, Encode, State) ->
    encode_object([
        [$,, key(Key, Encode, State), $: | value(Value, Encode, State)]
        || Key := Value <- Map
    ]).

encode_sorted_map(Map, Encode, State) ->
    encode_object([
        [$,, key(Key, Encode, State), $: | value(Value, Encode, State)]
        || {Key, Value} <- lists:keysort(1, maps:to_list(Map))
    ]).

encode_object([]) -> <<"{}">>;
encode_object([[_Comma | Entry] | Rest]) -> ["{", Entry, Rest, "}"].

%%
%% Codecs
%%

get_codecs(Name, Opts) ->
    case maps:get(Name, Opts, []) of
        Codecs when is_list(Codecs) ->
            Codecs;
        Codec ->
            [Codec]
    end.

traverse_codecs([Codec | Codecs], Next, Term, Encode, State) ->
    case codec(Codec, Term, Encode, State) of
        next ->
            traverse_codecs(Codecs, Next, Term, Encode, State);
        {next, NewTerm} ->
            traverse_codecs(Codecs, Next, NewTerm, Encode, State);
        {halt, NewTerm} ->
            NewTerm
    end;
traverse_codecs([], Next, Term, Encode, State) ->
    Next(Term, Encode, State).

codec({Codec, Opts}, Term, Encode, State) ->
    Codec(Term, Opts, Encode, State);
codec(Codec, Term, Encode, State) ->
    Codec(Term, Encode, State).

%%
%% Atom codecs
%%

normalize_atom_codecs(Codecs) ->
    lists:map(fun
        (Fun) when is_function(Fun, 3) ->
            Fun;
        ({Fun, Opts}) when is_function(Fun, 4) ->
            {Fun, Opts}
    end, Codecs).

%%
%% Binary codecs
%%

normalize_binary_codecs(Codecs) ->
    lists:map(fun
        (Fun) when is_function(Fun, 3) ->
            Fun;
        ({Fun, Opts}) when is_function(Fun, 4) ->
            {Fun, Opts}
    end, Codecs).

%%
%% Integer codecs
%%

normalize_integer_codecs(Codecs) ->
    lists:map(fun
        (Fun) when is_function(Fun, 3) ->
            Fun;
        ({Fun, Opts}) when is_function(Fun, 4) ->
            {Fun, Opts}
    end, Codecs).

%%
%% Float codecs
%%

normalize_float_codecs(Codecs) ->
    lists:map(fun
        (Fun) when is_function(Fun, 3) ->
            Fun;
        ({Fun, Opts}) when is_function(Fun, 4) ->
            {Fun, Opts}
    end, Codecs).

%%
%% List codecs
%%

% NOTE: To drop null, this works:
% 1> euneus_encoder:encode([{a,a},b,{c,null}], #{list => [proplist], map => [drop_nulls]}).
% <<"{\"a\":\"a\",\"b\":true}">>
normalize_list_codecs(Codecs) ->
    lists:map(fun
        (proplist) ->
            {fun proplist_list_codec/4, fun is_proplist/1};
        ({proplist, IsProplist}) when is_function(IsProplist, 1) ->
            {fun proplist_list_codec/4, IsProplist};
        (Fun) when is_function(Fun, 3) ->
            Fun;
        ({Fun, Opts}) when is_function(Fun, 4) ->
            {Fun, Opts}
    end, Codecs).

is_proplist(List) ->
    lists:all(fun is_proplist_prop/1, List).

is_proplist_prop({Key, _}) ->
    is_binary(Key) orelse is_atom(Key) orelse is_integer(Key);
is_proplist_prop(Key) ->
    is_binary(Key) orelse is_atom(Key).

proplist_list_codec(List, IsProplist, Encode, _State) ->
    case IsProplist(List) of
        true ->
            {halt, Encode(proplists:to_map(List), Encode)};
        false ->
            next
    end.

%%
%% Map codecs
%%

normalize_map_codecs(Codecs, Nulls) ->
    lists:map(fun
        (drop_nulls) ->
            {fun map_drop_nulls_codec/4, Nulls};
        (Fun) when is_function(Fun, 3) ->
            Fun;
        ({Fun, Opts}) when is_function(Fun, 4) ->
            {Fun, Opts}
    end, Codecs).

map_drop_nulls_codec(Map, Nulls, _Encode, _State) ->
    {next, maps:filter(fun(_Key, Value) ->
        not is_map_key(Value, Nulls)
    end, Map)}.

%%
%% Tuple codecs
%%

normalize_tuple_codecs(Codecs) ->
    lists:map(fun
        (datetime) ->
            fun datetime_tuple_codec/3;
        (timestamp) ->
            fun timestamp_tuple_codec/3;
        (ipv4) ->
            fun ipv4_tuple_codec/3;
        (ipv6) ->
            fun ipv6_tuple_codec/3;
        ({records, RecordsList}) when is_list(RecordsList) ->
            Records = maps:from_list([
                {Name, {length(Fields)+1, Fields}}
                || {Name, Fields} <- RecordsList
            ]),
            {fun records_tuple_codec/4, Records};
        (Fun) when is_function(Fun, 3) ->
            Fun;
        ({Fun, Opts}) when is_function(Fun, 4) ->
            {Fun, Opts}
    end, Codecs).

datetime_tuple_codec({{YYYY, MM, DD}, {H, M, S}}, _Encode, #state{escape = Escape})
  when ?min(YYYY, 0), ?in_range(MM, 1, 12), ?in_range(DD, 1, 31)
     , ?in_range(H, 0, 23), ?in_range(M, 0, 59), ?in_range(S, 0, 59) ->
    Datetime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [YYYY,MM,DD,H,M,S])
    ),
    {halt, Escape(Datetime)};
datetime_tuple_codec(_, _, _) ->
    next.

timestamp_tuple_codec({MegaSecs, Secs, MicroSecs} = Timestamp, _Encode, #state{escape = Escape})
  when ?min(MegaSecs, 0), ?min(Secs, 0), ?min(MicroSecs, 0) ->
    MilliSecs = MicroSecs div 1000,
    {{YYYY,MM,DD},{H,M,S}} = calendar:now_to_datetime(Timestamp),
    DateTime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
        [YYYY,MM,DD,H,M,S,MilliSecs])
    ),
    {halt, Escape(DateTime)};
timestamp_tuple_codec(_, _, _) ->
    next.

ipv4_tuple_codec({_A,_B,_C,_D} = Tuple, _Encode, #state{escape = Escape}) ->
    case inet_parse:ntoa(Tuple) of
        {error, einval} ->
            next;
        Ipv4 ->
            {halt, Escape(list_to_binary(Ipv4))}
    end;
ipv4_tuple_codec(_, _, _) ->
    next.

ipv6_tuple_codec({_A,_B,_C,_D,_E,_F,_G,_H} = Tuple, _Encode, #state{escape = Escape}) ->
    case inet_parse:ntoa(Tuple) of
        {error, einval} ->
            next;
        Ipv6 ->
            {halt, Escape(list_to_binary(Ipv6))}
    end;
ipv6_tuple_codec(_, _, _) ->
    next.

records_tuple_codec(Tuple, Records, Encode, _State) when tuple_size(Tuple) > 1 ->
    Name = element(1, Tuple),
    case Records of
        #{Name := {Size, Keys}} ->
            case tuple_size(Tuple) =:= Size of
                true ->
                    [Name | Values] = tuple_to_list(Tuple),
                    Proplist = lists:zip(Keys, Values),
                    {halt, Encode(proplists:to_map(Proplist), Encode)};
                false ->
                    next
            end;
        #{} ->
            next
    end;
records_tuple_codec(_, _, _, _) ->
    next.

%%
%% PID codecs
%%

normalize_pid_codecs(Codecs) ->
    lists:map(fun
        (to_binary) ->
            fun to_binary_pid_codec/3;
        (Fun) when is_function(Fun, 3) ->
            Fun;
        ({Fun, Opts}) when is_function(Fun, 4) ->
            {Fun, Opts}
    end, Codecs).

to_binary_pid_codec(PID, _Encode, #state{escape = Escape}) ->
    {halt, Escape(iolist_to_binary(pid_to_list(PID)))}.

%%
%% Port codecs
%%

normalize_port_codecs(Codecs) ->
    lists:map(fun
        (to_binary) ->
            fun to_binary_port_codec/3;
        (Fun) when is_function(Fun, 3) ->
            Fun;
        ({Fun, Opts}) when is_function(Fun, 4) ->
            {Fun, Opts}
    end, Codecs).

to_binary_port_codec(Port, _Encode, #state{escape = Escape}) ->
    {halt, Escape(iolist_to_binary(port_to_list(Port)))}.

%%
%% Reference codecs
%%

normalize_reference_codecs(Codecs) ->
    lists:map(fun
        (to_binary) ->
            fun to_binary_ref_codec/3;
        (Fun) when is_function(Fun, 3) ->
            Fun;
        ({Fun, Opts}) when is_function(Fun, 4) ->
            {Fun, Opts}
    end, Codecs).

to_binary_ref_codec(Ref, _Encode, #state{escape = Escape}) ->
    {halt, Escape(iolist_to_binary(ref_to_list(Ref)))}.

%%%=====================================================================
%%% Tests
%%%=====================================================================

-ifdef(TEST).

nulls_test() ->
    ?assertEqual(
        <<"[\"foo\",null,null,\"bar\"]">>,
        iolist_to_binary(encode(
            [foo, null, undefined, bar],
            #{nulls => [null, undefined]}
        ))
    ).

sort_keys_test() ->
    Map = #{c => c, d => d, a => a, e => e, b => b},
    [
        ?assertEqual(
            <<"{\"a\":\"a\",\"b\":\"b\",\"c\":\"c\",\"d\":\"d\",\"e\":\"e\"}">>,
            iolist_to_binary(encode(Map, #{sort_keys => true}))
        )
    ].

% Not implemented yet because there are no built-in codecs:
% - atom_test
% - binary_test
% - integer_test
% - float_test

list_test() ->
    [ { "proplist"
      , ?assertEqual(
            <<"{\"foo\":\"foo\",\"bar\":true}">>,
            iolist_to_binary(encode(
                [{foo, foo}, bar],
                #{list => proplist}
            ))
        )}
    ].

map_test() ->
    [ { "drop_nulls"
      , ?assertEqual(
            <<"{\"foo\":\"foo\"}">>,
            iolist_to_binary(encode(
                #{foo => foo, bar => null},
                #{map => drop_nulls}
            ))
        )}
    ].

-record(foo, {foo, bar}).
-record(bar, {bar, baz}).
tuple_test() ->
    [ { "datetime"
      , ?assertEqual(
            <<"{\"foo\":\"2024-04-29T22:34:35Z\"}">>,
            iolist_to_binary(encode(
                #{foo => {{2024,04,29},{22,34,35}}},
                #{tuple => datetime}
            ))
        )}
    , { "timestamp"
      , ?assertEqual(
            <<"\"1970-01-01T00:00:00.000Z\"">>,
            iolist_to_binary(encode(
                {0,0,0},
                #{tuple => timestamp}
            ))
        )}
    , { "ipv4"
      , ?assertEqual(
            <<"\"0.0.0.0\"">>,
            iolist_to_binary(encode(
                {0,0,0,0},
                #{tuple => ipv4}
            ))
        )}
    , { "ipv6"
      , ?assertEqual(
            <<"\"fe80::204:acff:fe17:bf38\"">>,
            iolist_to_binary(encode(
                {16#fe80,0,0,0,16#204,16#acff,16#fe17,16#bf38},
                #{tuple => ipv6}
            ))
        )}
    , { "record"
      , ?assertEqual(
            <<"[{\"foo\":\"foo\",\"bar\":\"bar\"},{\"bar\":\"bar\",\"baz\":\"baz\"}]">>,
            iolist_to_binary(encode(
                [
                    #foo{foo = foo, bar = bar},
                    #bar{bar = bar, baz = baz}
                ],
                #{tuple => {records, [
                    {foo, record_info(fields, foo)},
                    {bar, record_info(fields, bar)}
                ]}}
            ))
        )}
    ].

pid_test() ->
    [ { "to_binary"
      , ?assertEqual(
            <<"\"<0.92.0>\"">>,
            iolist_to_binary(encode(
                list_to_pid("<0.92.0>"),
                #{pid => to_binary}
            ))
        )}
    ].

port_test() ->
    [ { "to_binary"
      , ?assertEqual(
            <<"\"#Port<0.1>\"">>,
            iolist_to_binary(encode(
                list_to_port("#Port<0.1>"),
                #{port => to_binary}
            ))
        )}
    ].

reference_test() ->
    [ { "to_binary"
      , ?assertEqual(
            <<"\"#Ref<0.314572725.1088159747.110918>\"">>,
            iolist_to_binary(encode(
                list_to_ref("#Ref<0.314572725.1088159747.110918>"),
                #{reference => to_binary}
            ))
        )}
    ].

-endif.
