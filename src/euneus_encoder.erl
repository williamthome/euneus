-module(euneus_encoder).
-compile({no_auto_import, [float/1]}).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([encode/2]).

%

-hank([
    {unnecessary_function_arguments, [
        encode_atom/3,
        encode_float/3,
        encode_integer/3
    ]}
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(IS_MIN(X, Min),
    (is_integer(X) andalso X >= Min)
).

-define(IN_RANGE(X, Min, Max),
    (is_integer(X) andalso X >= Min andalso X =< Max)
).

%

-elvis([{elvis_style, no_macros, #{allow => ['IS_MIN', 'IN_RANGE']}}]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-record(state, {
    nulls :: #{term() := null},
    skip_values :: false | {true, #{term() := skip}},
    escape :: fun((binary()) -> iodata()),
    integer :: encode(integer()),
    float :: encode(float()),
    atom :: encode(atom()),
    list :: encode(list()),
    proplist :: false | {true, is_proplist()},
    map :: encode(map()),
    sort_keys :: boolean(),
    tuple :: encode(tuple()),
    pid :: encode(pid()),
    port :: encode(port()),
    reference :: encode(reference())
}).
-opaque state() :: #state{}.
-export_type([state/0]).

-type encode(Type) :: fun((Type, json:encoder(), #state{}) -> iodata()).
-export_type([encode/1]).

-type is_proplist() :: fun((list()) -> boolean()).
-export_type([is_proplist/0]).

-type options() :: #{
    nulls => [term()],
    skip_values => [term()],
    escape => fun((binary()) -> iodata()),
    integer => encode(integer()),
    float => encode(float()),
    atom => encode(atom()),
    list => encode(list()),
    proplist => boolean() | {true, is_proplist()},
    map => encode(map()),
    sort_keys => boolean(),
    tuple =>
        encode(tuple())
        | [
            datetime
            | timestamp
            | ipv4
            | ipv6
            | {record,
                #{Name :: atom() => {Fields :: [atom()], Size :: pos_integer()}}
                | [{Name :: atom(), Fields :: [atom()]}]}
            | fun((tuple()) -> next | {halt, term()})
        ],
    pid => encode(pid()),
    port => encode(port()),
    reference => encode(reference())
}.
-export_type([options/0]).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-spec encode(term(), options()) -> iodata().
encode(Input, Opts) ->
    State = new_state(Opts),
    json:encode(Input, fun(Term, Encode) ->
        value(Term, Encode, State)
    end).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

% State

new_state(Opts) ->
    #state{
        nulls = nulls(maps:get(nulls, Opts, [null])),
        skip_values = skip_values(maps:get(skip_values, Opts, [undefined])),
        escape = escape(maps:get(escape, Opts, default)),
        integer = integer(maps:get(integer, Opts, default)),
        float = float(maps:get(float, Opts, default)),
        atom = atom(maps:get(atom, Opts, default)),
        list = list(maps:get(list, Opts, default)),
        proplist = proplist(maps:get(proplist, Opts, false)),
        map = map(maps:get(map, Opts, default)),
        sort_keys = sort_keys(maps:get(sort_keys, Opts, false)),
        tuple = tuple(maps:get(tuple, Opts, default)),
        pid = pid(maps:get(pid, Opts, default)),
        port = port(maps:get(port, Opts, default)),
        reference = reference(maps:get(reference, Opts, default))
    }.

nulls(Nulls) when is_list(Nulls) ->
    maps:from_keys(Nulls, null).

skip_values([]) ->
    false;
skip_values(Values) when is_list(Values) ->
    {true, maps:from_keys(Values, skip)}.

escape(default) ->
    fun json:encode_binary/1;
escape(Escape) when is_function(Escape, 1) ->
    Escape.

integer(default) ->
    fun encode_integer/3;
integer(Encode) when is_function(Encode, 3) ->
    Encode.

float(default) ->
    fun encode_float/3;
float(Encode) when is_function(Encode, 3) ->
    Encode.

atom(default) ->
    fun encode_atom/3;
atom(Encode) when is_function(Encode, 3) ->
    Encode.

list(default) ->
    fun encode_list/3;
list(Encode) when is_function(Encode, 3) ->
    Encode.

proplist(false) ->
    false;
proplist(true) ->
    {true, fun is_proplist/1};
proplist({true, IsProplist}) when is_function(IsProplist, 1) ->
    {true, IsProplist}.

is_proplist([]) ->
    false;
is_proplist(List) ->
    lists:all(fun is_proplist_prop/1, List).

% Must be the same types handled by key/2.
is_proplist_prop({Key, _}) ->
    is_binary(Key) orelse
        is_list(Key) orelse
        is_atom(Key) orelse
        is_integer(Key);
is_proplist_prop(Key) ->
    is_atom(Key).

map(default) ->
    fun encode_map/3;
map(Encode) when is_function(Encode, 3) ->
    Encode.

sort_keys(Sort) when is_boolean(Sort) ->
    Sort.

tuple(default) ->
    fun encode_tuple/3;
tuple(Codecs) when is_list(Codecs) ->
    codecs([norm_codec(Codec) || Codec <- Codecs]);
tuple(Encode) when is_function(Encode, 3) ->
    Encode.

pid(default) ->
    fun encode_pid/3;
pid(Encode) when is_function(Encode, 3) ->
    Encode.

port(default) ->
    fun encode_port/3;
port(Encode) when is_function(Encode, 3) ->
    Encode.

reference(default) ->
    fun encode_reference/3;
reference(Encode) when is_function(Encode, 3) ->
    Encode.

% Codecs

codecs(Codecs) ->
    fun(Tuple, Encode, State) ->
        case traverse_codecs(Codecs, Tuple) of
            Tuple ->
                error(unsuported_tuple, [Tuple, Encode, State]);
            Term ->
                value(Term, Encode, State)
        end
    end.

traverse_codecs([Codec | Codecs], Tuple) ->
    case Codec(Tuple) of
        next ->
            traverse_codecs(Codecs, Tuple);
        {halt, NewTerm} ->
            NewTerm
    end;
traverse_codecs([], Tuple) ->
    Tuple.

norm_codec(datetime) ->
    fun datetime_codec/1;
norm_codec(timestamp) ->
    fun timestamp_codec/1;
norm_codec(ipv4) ->
    fun ipv4_codec/1;
norm_codec(ipv6) ->
    fun ipv6_codec/1;
norm_codec({record, Records}) when is_map(Records) ->
    records_codec(Records);
norm_codec({record, Records}) when is_list(Records) ->
    records_codec(norm_records_list(Records));
norm_codec(Codec) when is_function(Codec, 1) ->
    Codec.

norm_records_list(List) ->
    maps:from_list([{Name, {Fields, length(Fields) + 1}} || {Name, Fields} <- List]).

datetime_codec({{YYYY, MM, DD}, {H, M, S}}) when
    ?IS_MIN(YYYY, 0),
    ?IN_RANGE(MM, 1, 12),
    ?IN_RANGE(DD, 1, 31),
    ?IN_RANGE(H, 0, 23),
    ?IN_RANGE(M, 0, 59),
    ?IN_RANGE(S, 0, 59)
->
    DateTime = iolist_to_binary(
        io_lib:format(
            "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
            [YYYY, MM, DD, H, M, S]
        )
    ),
    {halt, DateTime};
datetime_codec(_Tuple) ->
    next.

timestamp_codec({MegaSecs, Secs, MicroSecs} = Timestamp) when
    ?IS_MIN(MegaSecs, 0), ?IS_MIN(Secs, 0), ?IS_MIN(MicroSecs, 0)
->
    MilliSecs = MicroSecs div 1000,
    {{YYYY, MM, DD}, {H, M, S}} = calendar:now_to_datetime(Timestamp),
    DateTime = iolist_to_binary(
        io_lib:format(
            "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
            [YYYY, MM, DD, H, M, S, MilliSecs]
        )
    ),
    {halt, DateTime};
timestamp_codec(_Tuple) ->
    next.

ipv4_codec({_A, _B, _C, _D} = Tuple) ->
    case inet_parse:ntoa(Tuple) of
        {error, einval} ->
            next;
        Ipv4 ->
            {halt, list_to_binary(Ipv4)}
    end;
ipv4_codec(_Tuple) ->
    next.

ipv6_codec({_A, _B, _C, _D, _E, _F, _G, _H} = Tuple) ->
    case inet_parse:ntoa(Tuple) of
        {error, einval} ->
            next;
        Ipv6 ->
            {halt, list_to_binary(Ipv6)}
    end;
ipv6_codec(_Tuple) ->
    next.

records_codec(Records) ->
    fun
        (Tuple) when tuple_size(Tuple) > 1 ->
            Name = element(1, Tuple),
            case Records of
                #{Name := {Fields, Size}} when tuple_size(Tuple) =:= Size ->
                    [Name | Values] = tuple_to_list(Tuple),
                    Map = proplists:to_map(lists:zip(Fields, Values)),
                    {halt, Map};
                #{} ->
                    next
            end;
        (_Tuple) ->
            next
    end.

% Encoders

value(Bin, _Encode, State) when is_binary(Bin) ->
    (State#state.escape)(Bin);
value(Int, Encode, State) when is_integer(Int) ->
    (State#state.integer)(Int, Encode, State);
value(Float, Encode, State) when is_float(Float) ->
    (State#state.float)(Float, Encode, State);
value(Atom, Encode, State) when is_atom(Atom) ->
    (State#state.atom)(Atom, Encode, State);
value(List, Encode, State) when is_list(List) ->
    (State#state.list)(List, Encode, State);
value(Map, Encode, State) when is_map(Map) ->
    (State#state.map)(Map, Encode, State);
value(Tuple, Encode, State) when is_tuple(Tuple) ->
    (State#state.tuple)(Tuple, Encode, State);
value(Pid, Encode, State) when is_pid(Pid) ->
    (State#state.pid)(Pid, Encode, State);
value(Port, Encode, State) when is_port(Port) ->
    (State#state.port)(Port, Encode, State);
value(Ref, Encode, State) when is_reference(Ref) ->
    (State#state.reference)(Ref, Encode, State);
value(Term, Encode, State) ->
    error(unsuported_term, [Term, Encode, State]).

encode_integer(Int, _Encode, _State) ->
    erlang:integer_to_binary(Int, 10).

encode_float(Float, _Encode, _State) ->
    erlang:float_to_binary(Float, [short]).

encode_atom(true, _Encode, _State) ->
    <<"true">>;
encode_atom(false, _Encode, _State) ->
    <<"false">>;
encode_atom(Atom, _Encode, #state{nulls = Nulls}) when is_map_key(Atom, Nulls) ->
    <<"null">>;
encode_atom(Atom, _Encode, #state{escape = Escape}) ->
    Escape(atom_to_binary(Atom, utf8)).

encode_list(List, Encode, #state{proplist = false}) ->
    json:encode_list(List, Encode);
encode_list(List, Encode, #state{proplist = {true, IsProplist}} = State) ->
    case IsProplist(List) of
        true ->
            value(proplists:to_map(List), Encode, State);
        false ->
            json:encode_list(List, Encode)
    end.

encode_map(Map, Encode, #state{sort_keys = false, skip_values = false} = State) ->
    do_encode_map([
        [$,, key(Key, State#state.escape), $: | value(Value, Encode, State)]
     || Key := Value <- Map
    ]);
encode_map(Map, Encode, #state{sort_keys = false, skip_values = {true, Skip}} = State) ->
    do_encode_map([
        [$,, key(Key, State#state.escape), $: | value(Value, Encode, State)]
     || Key := Value <- Map,
        not is_map_key(Value, Skip)
    ]);
encode_map(Map, Encode, #state{sort_keys = true, skip_values = false} = State) ->
    do_encode_map([
        [$,, key(Key, State#state.escape), $: | value(Value, Encode, State)]
     || {Key, Value} <- lists:keysort(1, maps:to_list(Map))
    ]);
encode_map(Map, Encode, #state{sort_keys = true, skip_values = {true, Skip}} = State) ->
    do_encode_map([
        [$,, key(Key, State#state.escape), $: | value(Value, Encode, State)]
     || {Key, Value} <- lists:keysort(1, maps:to_list(Map)),
        not is_map_key(Value, Skip)
    ]).

key(Bin, Escape) when is_binary(Bin) ->
    Escape(Bin);
key(Str, Escape) when is_list(Str) ->
    Escape(iolist_to_binary(Str));
key(Atom, Escape) when is_atom(Atom) ->
    Escape(atom_to_binary(Atom, utf8));
key(Int, Escape) when is_integer(Int) ->
    Escape(integer_to_binary(Int, 10)).

do_encode_map([]) -> <<"{}">>;
do_encode_map([[_Comma | Entry] | Rest]) -> ["{", Entry, Rest, "}"].

encode_tuple(Tuple, Encode, State) ->
    error(unsuported_tuple, [Tuple, Encode, State]).

encode_pid(Pid, Encode, State) ->
    error(unsuported_pid, [Pid, Encode, State]).

encode_port(Port, Encode, State) ->
    error(unsuported_port, [Port, Encode, State]).

encode_reference(Ref, Encode, State) ->
    error(unsuported_reference, [Ref, Encode, State]).
