-module(euneus_decoder).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([decode/2]).
-export([stream_start/2]).
-export([stream_continue/2]).

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
-export_type([stream_state/0]).
-export_type([stream_result/0]).

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
        binary
        | copy
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

% The correct type is 'json:continuation_state()', but dialyzer says it is wrong.
-type stream_state() :: term().

-type stream_result() ::
    {continue, json:continuation_state()}
    | {end_of_input, term()}.

%% --------------------------------------------------------------------
%% DocTest
%% --------------------------------------------------------------------

-if(?OTP_RELEASE >= 27).
-ifdef(TEST).
-include_lib("doctest/include/doctest.hrl").
-endif.
-endif.

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-spec decode(JSON, Options) -> term() when
    JSON :: binary(),
    Options :: options().
%% @doc Decodes a binary JSON into a term.
%%
%% <em>Example:</em>
%%
%% ```
%% 1> euneus_decoder:decode(<<"\"foo\"">>, #{}).
%% <<"foo">>
%% '''
%%
%% Option details:
%%
%% <ul>
%%   <li>
%%     `codecs' - Transforms a JSON binary value into an Erlang term.
%%     By returning `next', the next codec will be called, or by returning
%%     `{halt, Term :: term()}', the Term is returned as the value.
%%
%%     You can use the built-in codecs or your own.
%%     Please see the `t:euneus_decoder:codec/0' type for details.
%%
%%     Default is `[]'.
%%
%%     Built-in codecs:
%%
%%     <ul>
%%       <li>
%%         `timestamp' - Transforms an ISO 8601 string with milliseconds into
%%         an `t:erlang:timestamp/0'.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus_decoder:decode(<<"\"1970-01-01T00:00:00.000Z\"">>, #{codecs => [timestamp]}).
%% {0, 0, 0}
%% '''
%%       </li>
%%       <li>
%%         `datetime' - Transforms an ISO 8601 string into a `t:calendar:datetime/0'.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus_decoder:decode(<<"\"1970-01-01T00:00:00Z\"">>, #{codecs => [datetime]}).
%% {{1970, 01, 01},{00, 00, 00}}
%% '''
%%       </li>
%%       <li>
%%         `ipv4' - Transforms a JSON string into an `t:inet:ip4_address/0'.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus_decoder:decode(<<"\"127.0.0.1\"">>, #{codecs => [ipv4]}).
%% {127, 0, 0, 1}
%% '''
%%       </li>
%%       <li>
%%         `ipv6' - Transforms a JSON string into an `t:inet:ip6_address/0'.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus_decoder:decode(<<"\"::\"">>, #{codecs => [ipv6]}).
%% {0, 0, 0, 0, 0, 0, 0, 0}
%% 2> euneus_decoder:decode(<<"\"::1\"">>, #{codecs => [ipv6]}).
%% {0, 0, 0, 0, 0, 0, 0, 1}
%% 3> euneus_decoder:decode(<<"\"::192.168.42.2\"">>, #{codecs => [ipv6]}).
%% {0, 0, 0, 0, 0, 0, (192 bsl 8) bor 168, (42 bsl 8) bor 2}
%% 4> euneus_decoder:decode(<<"\"::ffff:192.168.42.2\"">>, #{codecs => [ipv6]}).
%% {0, 0, 0, 0, 0, 16#FFFF, (192 bsl 8) bor 168, (42 bsl 8) bor 2}
%% 5> euneus_decoder:decode(<<"\"3ffe:b80:1f8d:2:204:acff:fe17:bf38\"">>, #{codecs => [ipv6]}).
%% {16#3ffe, 16#b80, 16#1f8d, 16#2, 16#204, 16#acff, 16#fe17, 16#bf38}
%% 6> euneus_decoder:decode(<<"\"fe80::204:acff:fe17:bf38\"">>, #{codecs => [ipv6]}).
%% {16#fe80, 0, 0, 0, 16#204, 16#acff, 16#fe17, 16#bf38}
%% '''
%%       </li>
%%       <li>
%%         `pid' - Transforms a JSON string into an `t:erlang:pid/0'.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus_decoder:decode(<<"\"<0.92.0>\"">>, #{codecs => [pid]})
%% .. =:= list_to_pid("<0.92.0>").
%% true
%% '''
%%       </li>
%%       <li>
%%         `port' - Transforms a JSON string into an `t:erlang:port/0'.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus_decoder:decode(<<"\"#Port<0.1>\"">>, #{codecs => [port]})
%% .. =:= list_to_port("#Port<0.1>").
%% true
%% '''
%%       </li>
%%       <li>
%%         `reference' - Transforms a JSON string into an `t:erlang:reference/0'.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus_decoder:decode(<<"\"#Ref<0.314572725.1088159747.110918>\"">>, #{codecs => [reference]})
%% .. =:= list_to_ref("#Ref<0.314572725.1088159747.110918>").
%% true
%% '''
%%       </li>
%%     </ul>
%%
%%     Custom codec example:
%%
%% ```
%% 1> euneus:decode(<<"\"foo\"">>, #{codecs => [fun(<<"foo">>) -> {halt, foo} end]}).
%% foo
%% '''
%%   </li>
%%   <li>
%%     `null' - Defines which term should be considered null.
%%
%%     Default is `null'.
%%
%%     <em>Example:</em>
%%
%% ```
%% 1> euneus_decoder:decode(<<"null">>, #{null => nil}).
%% nil
%% '''
%%   </li>
%%   <li>
%%     `binary_to_float' - Overrides the default binary to float conversion.
%%
%%   </li>
%%   <li>
%%     `binary_to_integer' - Overrides the default binary to integer conversion..
%%
%%   </li>
%%   <li>
%%     `array_start' - Overrides the `t:json:array_start_fun/0' callback.
%%
%%   </li>
%%   <li>
%%     `array_push' - Overrides the `t:json:array_push_fun/0' callback.
%%
%%   </li>
%%   <li>
%%     `array_finish' - Overrides the `t:json:array_finish_fun/0' callback.
%%
%%     In addition to the custom function, there are:
%%
%%     <ul>
%%       <li>
%%         `ordered' - Returns the array in the same order as the JSON.
%%
%%         That's the slower option.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus_decoder:decode(<<"[1,2,3]">>, #{array_finish => ordered}).
%% [1,2,3]
%% '''
%%       </li>
%%       <li>
%%         `reversed' - Returns the array in a reversed order.
%%
%%         That's the faster option.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus_decoder:decode(<<"[1,2,3]">>, #{array_finish => reversed}).
%% [3,2,1]
%% '''
%%       </li>
%%     </ul>
%%
%%     Default is `ordered'.
%%
%%   </li>
%%   <li>
%%     `object_start' - Overrides the `t:json:object_start_fun/0' callback.
%%
%%   </li>
%%   <li>
%%     `object_keys' - Transforms JSON objects key into Erlang term.
%%
%%     In addition to the custom function, there are:
%%
%%     <ul>
%%       <li>
%%         `binary' - Returns the key as `t:erlang:binary/0'.
%%       </li>
%%       <li>
%%         `copy' - Copies the key via `binary:copy/1' returning it as `t:erlang:binary/0'.
%%       </li>
%%       <li>
%%         `atom' - Returns the key as `t:erlang:atom/0' via `erlang:binary_to_atom/2'.
%%       </li>
%%       <li>
%%         `existing_atom' - Returns the key as `t:erlang:atom/0' via
%%         `erlang:binary_to_existing_atom/2'.
%%       </li>
%%     </ul>
%%
%%     Default is `binary'.
%%
%%   </li>
%%   <li>
%%     `object_push' - Overrides the `t:json:object_push_fun/0' callback.
%%
%%   </li>
%%   <li>
%%     `object_finish' - Overrides the `t:json:object_finish_fun/0' callback.
%%
%%     In addition to the custom function, there are:
%%
%%     <ul>
%%       <li>
%%         `map' - Returns the object as a `t:erlang:map/0'.
%%
%%         That's the slower option.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus_decoder:decode(
%% ..     <<"{\"a\":\"a\",\"b\":\"b\",\"c\":\"c\"}">>,
%% ..     #{object_finish => map}
%% .. ).
%% #{<<"a">> => <<"a">>,<<"b">> => <<"b">>,<<"c">> => <<"c">>}
%% '''
%%       </li>
%%       <li>
%%         `proplist' - Returns the object as an ordered `t:proplists:proplist/0'.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus_decoder:decode(
%% ..     <<"{\"a\":\"a\",\"b\":\"b\",\"c\":\"c\"}">>,
%% ..     #{object_finish => proplist}
%% .. ).
%% [{<<"a">>, <<"a">>},{<<"b">>, <<"b">>},{<<"c">>, <<"c">>}]
%% '''
%%       </li>
%%       <li>
%%         `reversed_proplist' - Returns the object as a reversed `t:proplists:proplist/0'.
%%
%%         That's the faster option.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus_decoder:decode(
%% ..     <<"{\"a\":\"a\",\"b\":\"b\",\"c\":\"c\"}">>,
%% ..     #{object_finish => reversed_proplist}
%% .. ).
%% [{<<"c">>, <<"c">>},{<<"b">>, <<"b">>},{<<"a">>, <<"a">>}]
%% '''
%%       </li>
%%     </ul>
%%
%%     Default is `map'.
%%
%%   </li>
%% </ul>
decode(JSON, Opts) when is_binary(JSON), is_map(Opts) ->
    Codecs = maps:get(codecs, Opts, []),
    Decoders = decoders(Codecs, Opts),
    decode(Codecs, JSON, Decoders).

-spec stream_start(JSON, Options) -> stream_result() when
    JSON :: binary(),
    Options :: options().
%% @doc Begin parsing a stream of bytes of a JSON value.
%%
%% Similar to `decode/2' but returns `{end_of_input, Term}' when a complete
%% JSON value is parsed or returns `{continue, State}' for incomplete data.
%%
%% The State can be fed to the `stream_continue/2' function when more data is available.
stream_start(JSON, Opts) ->
    Codecs = maps:get(codecs, Opts, []),
    Decoders = decoders(Codecs, Opts),
    stream_result(json:decode_start(JSON, Codecs, Decoders)).

-spec stream_continue(JSON, State) -> stream_result() when
    JSON :: binary() | end_of_input,
    State :: stream_state().
%% @doc Continue parsing a stream of bytes of a JSON value.
%%
%% Similar to `stream_start/2', if the function returns `{continue, State}'
%% and there is no more data, use `end_of_input' instead of a binary.
%%
%% <em>Example:</em>
%%
%% ```
%% 1> begin
%% .. {continue, State} = euneus_decoder:stream_start(<<"{\"foo\":">>, #{}),
%% .. euneus_decoder:stream_continue(<<"1}">>, State)
%% .. end.
%% {end_of_input,#{<<"foo">> => 1}}
%% '''
stream_continue(<<>>, State) ->
    stream_result(json:decode_continue(end_of_input, State));
stream_continue(JSON, State) ->
    stream_result(json:decode_continue(JSON, State)).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

decode(Codecs, JSON, Decoders) ->
    case json:decode(JSON, [], Decoders) of
        {Result, [], <<>>} ->
            traverse_codecs(Codecs, Result);
        {_Result, [], Rest} ->
            invalid_byte(Rest, 0)
    end.

stream_result({continue, State}) ->
    {continue, State};
stream_result({Result, Codecs, <<>>}) ->
    {end_of_input, traverse_codecs(Codecs, Result)}.

% This is a copy of json:invalid_byte/2, since it is not exported.
invalid_byte(Bin, Skip) ->
    Byte = binary:at(Bin, Skip),
    error({invalid_byte, Byte}, none, error_info(Skip)).

error_info(Skip) ->
    [{error_info, #{cause => #{position => Skip}}}].

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
