-module(euneus_decoder).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([decode/1]).
-export([decode/2]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(is_number(X), (
    X >= $0 andalso X =< $9
)).
-define(range(X, Min, Max), (
    is_integer(X) andalso X >= Min andalso X =< Max
)).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

decode(JSON) ->
    decode(JSON, #{}).

decode(JSON, Opts) when is_binary(JSON), is_map(Opts) ->
    Codecs = norm_codecs(maps:get(codecs, Opts, [])),
    Decoders = decoders(Codecs, Opts),
    do_decode(Codecs, JSON, Decoders).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

% If no codec is provided, we are good to go with json:decode/3, otherwise,
% we use json:decode_start/3 to be able to use traverse_codecs when the result
% is a simple value, like a string or number, e.g.:
% > decode(<<"\"1970-01-01T00:00:00Z\"">>, #{codecs => [datetime]}).
% > {{1970,1,1},{0,0,0}}
% Otherwise, the result above will be the date as string.
do_decode([], JSON, Decoders) ->
    {Result, [], <<>>} = json:decode(JSON, [], Decoders),
    Result;
do_decode(Codecs, JSON, Decoders) ->
    case json:decode_start(JSON, [], Decoders) of
        {Result, [], <<>>} ->
            traverse_codecs(Codecs, Result);
        Continue ->
            do_decode_continue(Continue, JSON)
    end.

do_decode_continue({continue, State}, JSON) ->
    do_decode_continue(json:decode_continue(JSON, State), JSON);
do_decode_continue({Result, [], <<>>}, _JSON) ->
    Result;
do_decode_continue({_Result, [], Rest}, _JSON) ->
    invalid_byte(Rest, 0).

% This is a copy of json:invalid_byte/2, since it is not exported.
invalid_byte(Bin, Skip) ->
    Byte = binary:at(Bin, Skip),
    error({invalid_byte, Byte}, none, error_info(Skip)).

error_info(Skip) ->
    [{error_info, #{cause => #{position => Skip}}}].

%% Codecs

norm_codecs(Codecs) when is_list(Codecs) ->
    [norm_codec(Codec) || Codec <- Codecs].

norm_codec(copy) ->
    fun copy_codec/1;
norm_codec(timestamp) ->
    fun timestamp_codec/1;
norm_codec(datetime) ->
    fun datetime_codec/1;
norm_codec(ipv4) ->
    fun ipv4_codec/1;
norm_codec(ipv6) ->
    fun ipv6_codec/1;
norm_codec(pid) ->
    fun pid_codec/1;
norm_codec(port) ->
    fun port_codec/1;
norm_codec(reference) ->
    fun reference_codec/1;
norm_codec(Codec) when is_function(Codec, 1) ->
    Codec.

copy_codec(Bin) ->
    {halt, binary:copy(Bin)}.

% <<"\"1970-01-01T00:00:00.000Z\"">> = {0,0,0}
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
timestamp_codec(_Bin) ->
    next.

% <<"\"1970-01-01T00:00:00Z\"">> = {{1970,1,1},{0,0,0}}
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
datetime_codec(_Bin) ->
    next.

timestamp(Y4, Y3, Y2, Y1, M2, M1, D2, D1, H2, H1, Min2, Min1, S2, S1, MSec3, MSec2, MSec1) ->
    Datetime = datetime(Y4, Y3, Y2, Y1, M2, M1, D2, D1, H2, H1, Min2, Min1, S2, S1),
    GregSeconds = calendar:datetime_to_gregorian_seconds(Datetime),
    Seconds = GregSeconds - 62167219200,
    MilliSeconds = chars_to_integer(MSec3, MSec2, MSec1),
    {Seconds div 1000000, Seconds rem 1000000, MilliSeconds * 1000}.

datetime(Y4, Y3, Y2, Y1, M2, M1, D2, D1, H2, H1, Min2, Min1, S2, S1) ->
    {date(Y4, Y3, Y2, Y1, M2, M1, D2, D1), time(H2, H1, Min2, Min1, S2, S1)}.

date(Y4, Y3, Y2, Y1, M2, M1, D2, D1) ->
    {chars_to_integer(Y4, Y3, Y2, Y1), chars_to_integer(M2, M1), chars_to_integer(D2, D1)}.

time(H2, H1, Min2, Min1, S2, S1) ->
    {chars_to_integer(H2, H1), chars_to_integer(Min2, Min1), chars_to_integer(S2, S1)}.

chars_to_integer(N2, N1) ->
    ((N2 - $0) * 10) + (N1 - $0).

chars_to_integer(N3, N2, N1) ->
    ((N3 - $0) * 100) + ((N2 - $0) * 10) + (N1 - $0).

chars_to_integer(N4, N3, N2, N1) ->
    ((N4 - $0) * 1000) + ((N3 - $0) * 100) + ((N2 - $0) * 10) + (N1 - $0).

ipv4_codec(<<A/integer, B/integer, C/integer, $., _/binary>> = Bin)
    when ?range(A, 0, 255); ?range(B, 0, 255); ?range(C, 0, 255) ->
    ipv4_codec_continue(Bin);
ipv4_codec(<<A/integer, B/integer, $., _/binary>> = Bin)
    when ?range(A, 0, 255); ?range(B, 0, 255) ->
    ipv4_codec_continue(Bin);
ipv4_codec(<<A/integer, $., _/binary>> = Bin)
    when ?range(A, 0, 255) ->
    ipv4_codec_continue(Bin);
ipv4_codec(_Bin) ->
    next.

ipv4_codec_continue(Bin) ->
    case inet_parse:ipv4_address(binary_to_list(Bin)) of
        {ok, IPv4} ->
            {halt, IPv4};
        {error, einval} ->
            next
    end.

ipv6_codec(<<$:, $:>>) ->
    {halt, {0,0,0,0,0,0,0,0}};
ipv6_codec(<<$:, $:, _/binary>> = Bin) ->
    ipv6_codec_continue(Bin);
ipv6_codec(<<_/integer, _/integer, _/integer, _/integer, $:, _/binary>> = Bin) ->
    ipv6_codec_continue(Bin);
ipv6_codec(_Bin) ->
    next.

ipv6_codec_continue(Bin) ->
    case inet_parse:ipv6strict_address(binary_to_list(Bin)) of
        {ok, Ipv6} ->
            {halt, Ipv6};
        {error, einval} ->
            next
    end.

pid_codec(<<$<, _/binary>> = Bin) ->
    try
        {halt, list_to_pid(binary_to_list(Bin))}
    catch _:_ ->
        next
    end;
pid_codec(_Bin) ->
    next.

port_codec(<<"#Port<", _/binary>> = Bin) ->
    try
        {halt, list_to_port(binary_to_list(Bin))}
    catch _:_ ->
        next
    end;
port_codec(_Bin) ->
    next.

reference_codec(<<"#Ref<", _/binary>> = Bin) ->
    try
        {halt, list_to_ref(binary_to_list(Bin))}
    catch _:_ ->
        next
    end;
reference_codec(_Bin) ->
    next.

%% Decoders

decoders(Codecs, Opts) ->
    #{
        array_start => array_start_decoder(maps:get(array_start, Opts, empty)),
        array_push => array_push_decoder(maps:get(array_push, Opts, push), Codecs),
        array_finish => array_finish_decoder(maps:get(array_finish, Opts, ordered)),
        object_push => object_push_decoder(maps:get(object_push, Opts, push),
                                           maps:get(object_keys, Opts, binary),
                                           Codecs),
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

array_push_decoder(push, Codecs) ->
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

traverse_codecs([], Term) ->
    Term;
traverse_codecs(Codecs, Bin) when is_binary(Bin) ->
    do_traverse_codecs(Codecs, Bin);
traverse_codecs(_Codecs, Term) ->
    Term.

do_traverse_codecs([Codec | Codecs], Bin) ->
    case Codec(Bin) of
        next ->
            do_traverse_codecs(Codecs, Bin);
        {halt, Value} ->
            Value
    end;
do_traverse_codecs([], Bin) ->
    Bin.

