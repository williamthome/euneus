-module(euneus_decoder).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([decode/2]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(IS_NUMBER(X),
    (X >= $0 andalso X =< $9)
).

-define(IN_RANGE(X, Min, Max),
    (is_integer(X) andalso X >= Min andalso X =< Max)
).

%

-elvis([{elvis_style, no_macros, #{allow => ['IS_NUMBER', 'IN_RANGE']}}]).

%% --------------------------------------------------------------------
%% Type exports
%% --------------------------------------------------------------------

-export_type([options/0]).
-export_type([codec/0]).
-export_type([codec_callback/0]).

%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------

-type options() :: #{
    codecs => [codec()],
    null => term(),
    binary_to_float => json:from_binary_fun(),
    binary_to_integer => json:from_binary_fun(),
    array_start => json:array_start_fun(),
    array_push => json:array_push_fun(),
    array_finish =>
        ordered
        | reversed
        | json:array_finish_fun(),
    object_start => json:object_start_fun(),
    object_keys =>
        copy
        | atom
        | existing_atom
        | json:from_binary_fun(),
    object_push => json:object_push_fun(),
    object_finish =>
        map
        | proplist
        | reversed_proplist
        | json:object_finish_fun()
}.

-type codec() ::
    copy
    | timestamp
    | datetime
    | ipv4
    | ipv6
    | pid
    | port
    | reference
    | codec_callback().

-type codec_callback() :: fun((binary()) -> next | {halt, term()}).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-spec decode(binary(), options()) -> term().
decode(JSON, Opts) when is_binary(JSON), is_map(Opts) ->
    Codecs = maps:get(codecs, Opts, []),
    Decoders = decoders(Codecs, Opts),
    decode(Codecs, JSON, Decoders).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

decode(Codecs, JSON, Decoders) ->
    {Result, [], <<>>} = json:decode(JSON, [], Decoders),
    traverse_codecs(Codecs, Result).

%% Decoders

decoders(Codecs, Opts) ->
    #{
        array_start => array_start_decoder(maps:get(array_start, Opts, empty)),
        array_push => array_push_decoder(maps:get(array_push, Opts, traverse_codecs), Codecs),
        array_finish => array_finish_decoder(maps:get(array_finish, Opts, ordered)),
        object_push => object_push_decoder(
            maps:get(object_push, Opts, push),
            maps:get(object_keys, Opts, binary),
            Codecs
        ),
        object_finish => object_finish_decoder(maps:get(object_finish, Opts, map)),
        integer => maps:get(binary_to_integer, Opts, fun erlang:binary_to_integer/1),
        float => maps:get(binary_to_float, Opts, fun erlang:binary_to_float/1),
        % string => We skip this, since it transforms any string, including object keys.
        null => maps:get(null, Opts, null)
    }.

array_start_decoder(empty) ->
    fun(_) -> [] end;
array_start_decoder(Decoder) when is_function(Decoder, 1) ->
    Decoder.

array_push_decoder(traverse_codecs, Codecs) ->
    fun(Elem, Acc) -> [traverse_codecs(Codecs, Elem) | Acc] end;
array_push_decoder(Decoder, _Codecs) when is_function(Decoder, 2) ->
    Decoder.

array_finish_decoder(ordered) ->
    fun(Acc, OldAcc) -> {lists:reverse(Acc), OldAcc} end;
array_finish_decoder(reversed) ->
    fun(Acc, OldAcc) -> {Acc, OldAcc} end;
array_finish_decoder(Decoder) when is_function(Decoder, 2) ->
    Decoder.

object_push_decoder(push, binary, Codecs) ->
    fun(Key, Value, Acc) ->
        [{Key, traverse_codecs(Codecs, Value)} | Acc]
    end;
object_push_decoder(push, copy, Codecs) ->
    fun(Key, Value, Acc) ->
        [{binary:copy(Key), traverse_codecs(Codecs, Value)} | Acc]
    end;
object_push_decoder(push, atom, Codecs) ->
    fun(Key, Value, Acc) ->
        [{binary_to_atom(Key, utf8), traverse_codecs(Codecs, Value)} | Acc]
    end;
object_push_decoder(push, existing_atom, Codecs) ->
    fun(Key, Value, Acc) ->
        [{binary_to_existing_atom(Key, utf8), traverse_codecs(Codecs, Value)} | Acc]
    end;
object_push_decoder(push, NormKey, Codecs) when is_function(NormKey, 1) ->
    fun(Key, Value, Acc) ->
        [{NormKey(Key), traverse_codecs(Codecs, Value)} | Acc]
    end;
object_push_decoder(Decoder, _NormKey, _Codecs) when is_function(Decoder, 3) ->
    Decoder.

object_finish_decoder(map) ->
    fun(Acc, OldAcc) -> {maps:from_list(Acc), OldAcc} end;
object_finish_decoder(proplist) ->
    fun(Acc, OldAcc) -> {lists:reverse(Acc), OldAcc} end;
object_finish_decoder(reversed_proplist) ->
    fun(Acc, OldAcc) -> {Acc, OldAcc} end;
object_finish_decoder(Decoder) when is_function(Decoder, 2) ->
    Decoder.

%% Codecs

traverse_codecs([], Term) ->
    Term;
traverse_codecs(Codecs, Bin) when is_binary(Bin) ->
    do_traverse_codecs(Codecs, Bin);
traverse_codecs(_Codecs, Term) ->
    Term.

do_traverse_codecs([Codec | Codecs], Bin) ->
    case codec_callback(Codec, Bin) of
        next ->
            do_traverse_codecs(Codecs, Bin);
        {halt, Value} ->
            Value
    end;
do_traverse_codecs([], Bin) ->
    Bin.

codec_callback(copy, Bin) ->
    copy_codec_callback(Bin);
codec_callback(timestamp, Bin) ->
    timestamp_codec_callback(Bin);
codec_callback(datetime, Bin) ->
    datetime_codec_callback(Bin);
codec_callback(ipv4, Bin) ->
    ipv4_codec_callback(Bin);
codec_callback(ipv6, Bin) ->
    ipv6_codec_callback(Bin);
codec_callback(pid, Bin) ->
    pid_codec_callback(Bin);
codec_callback(port, Bin) ->
    port_codec_callback(Bin);
codec_callback(reference, Bin) ->
    reference_codec_callback(Bin);
codec_callback(Callback, Bin) ->
    Callback(Bin).

copy_codec_callback(Bin) ->
    {halt, binary:copy(Bin)}.

% <<"\"1970-01-01T00:00:00.000Z\"">> = {0,0,0}
timestamp_codec_callback(
    <<Y4/integer, Y3/integer, Y2/integer, Y1/integer, $-/integer, M2/integer, M1/integer,
        $-/integer, D2/integer, D1/integer, $T/integer, H2/integer, H1/integer, $:/integer,
        Min2/integer, Min1/integer, $:/integer, S2/integer, S1/integer, $./integer, MSec3/integer,
        MSec2/integer, MSec1/integer, $Z/integer>>
) when
    ?IS_NUMBER(Y4),
    ?IS_NUMBER(Y3),
    ?IS_NUMBER(Y2),
    ?IS_NUMBER(Y1),
    ?IS_NUMBER(M2),
    ?IS_NUMBER(M1),
    ?IS_NUMBER(D2),
    ?IS_NUMBER(D1),
    ?IS_NUMBER(H2),
    ?IS_NUMBER(H1),
    ?IS_NUMBER(Min2),
    ?IS_NUMBER(Min1),
    ?IS_NUMBER(S2),
    ?IS_NUMBER(S1),
    ?IS_NUMBER(MSec3),
    ?IS_NUMBER(MSec2),
    ?IS_NUMBER(MSec1)
->
    {halt,
        chars_to_timestamp(
            {Y4, Y3, Y2, Y1, M2, M1, D2, D1},
            {H2, H1, Min2, Min1, S2, S1},
            {MSec3, MSec2, MSec1}
        )};
timestamp_codec_callback(_Bin) ->
    next.

% <<"\"1970-01-01T00:00:00Z\"">> = {{1970,1,1},{0,0,0}}
datetime_codec_callback(
    <<Y4/integer, Y3/integer, Y2/integer, Y1/integer, $-/integer, M2/integer, M1/integer,
        $-/integer, D2/integer, D1/integer, $T/integer, H2/integer, H1/integer, $:/integer,
        Min2/integer, Min1/integer, $:/integer, S2/integer, S1/integer, $Z/integer>>
) when
    ?IS_NUMBER(Y4),
    ?IS_NUMBER(Y3),
    ?IS_NUMBER(Y2),
    ?IS_NUMBER(Y1),
    ?IS_NUMBER(M2),
    ?IS_NUMBER(M1),
    ?IS_NUMBER(D2),
    ?IS_NUMBER(D1),
    ?IS_NUMBER(H2),
    ?IS_NUMBER(H1),
    ?IS_NUMBER(Min2),
    ?IS_NUMBER(Min1),
    ?IS_NUMBER(S2),
    ?IS_NUMBER(S1)
->
    {halt, chars_to_datetime({Y4, Y3, Y2, Y1, M2, M1, D2, D1}, {H2, H1, Min2, Min1, S2, S1})};
datetime_codec_callback(_Bin) ->
    next.

chars_to_timestamp(Date, Time, {MSec3, MSec2, MSec1}) ->
    DateTime = chars_to_datetime(Date, Time),
    GregSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
    Seconds = GregSeconds - 62167219200,
    MilliSeconds = chars_to_integer(MSec3, MSec2, MSec1),
    {Seconds div 1000000, Seconds rem 1000000, MilliSeconds * 1000}.

chars_to_datetime(Date, Time) ->
    {chars_to_date(Date), chars_to_time(Time)}.

chars_to_date({Y4, Y3, Y2, Y1, M2, M1, D2, D1}) ->
    {chars_to_integer(Y4, Y3, Y2, Y1), chars_to_integer(M2, M1), chars_to_integer(D2, D1)}.

chars_to_time({H2, H1, Min2, Min1, S2, S1}) ->
    {chars_to_integer(H2, H1), chars_to_integer(Min2, Min1), chars_to_integer(S2, S1)}.

chars_to_integer(N2, N1) ->
    ((N2 - $0) * 10) + (N1 - $0).

chars_to_integer(N3, N2, N1) ->
    ((N3 - $0) * 100) + ((N2 - $0) * 10) + (N1 - $0).

chars_to_integer(N4, N3, N2, N1) ->
    ((N4 - $0) * 1000) + ((N3 - $0) * 100) + ((N2 - $0) * 10) + (N1 - $0).

ipv4_codec_callback(<<A/integer, B/integer, C/integer, $., _/binary>> = Bin) when
    ?IN_RANGE(A, 0, 255); ?IN_RANGE(B, 0, 255); ?IN_RANGE(C, 0, 255)
->
    ipv4_codec_parse_callback(Bin);
ipv4_codec_callback(<<A/integer, B/integer, $., _/binary>> = Bin) when
    ?IN_RANGE(A, 0, 255); ?IN_RANGE(B, 0, 255)
->
    ipv4_codec_parse_callback(Bin);
ipv4_codec_callback(<<A/integer, $., _/binary>> = Bin) when
    ?IN_RANGE(A, 0, 255)
->
    ipv4_codec_parse_callback(Bin);
ipv4_codec_callback(_Bin) ->
    next.

ipv4_codec_parse_callback(Bin) ->
    case inet_parse:ipv4strict_address(binary_to_list(Bin)) of
        {ok, IPv4} ->
            {halt, IPv4};
        {error, einval} ->
            next
    end.

ipv6_codec_callback(<<$:, $:>>) ->
    {halt, {0, 0, 0, 0, 0, 0, 0, 0}};
ipv6_codec_callback(<<$:, $:, _/binary>> = Bin) ->
    ipv6_codec_parse_callback(Bin);
ipv6_codec_callback(<<_/integer, _/integer, _/integer, _/integer, $:, _/binary>> = Bin) ->
    ipv6_codec_parse_callback(Bin);
ipv6_codec_callback(_Bin) ->
    next.

ipv6_codec_parse_callback(Bin) ->
    case inet_parse:ipv6strict_address(binary_to_list(Bin)) of
        {ok, Ipv6} ->
            {halt, Ipv6};
        {error, einval} ->
            next
    end.

pid_codec_callback(<<$<, _/binary>> = Bin) ->
    try
        {halt, list_to_pid(binary_to_list(Bin))}
    catch
        _:_ ->
            next
    end;
pid_codec_callback(_Bin) ->
    next.

port_codec_callback(<<"#Port<", _/binary>> = Bin) ->
    try
        {halt, list_to_port(binary_to_list(Bin))}
    catch
        _:_ ->
            next
    end;
port_codec_callback(_Bin) ->
    next.

reference_codec_callback(<<"#Ref<", _/binary>> = Bin) ->
    try
        {halt, list_to_ref(binary_to_list(Bin))}
    catch
        _:_ ->
            next
    end;
reference_codec_callback(_Bin) ->
    next.
