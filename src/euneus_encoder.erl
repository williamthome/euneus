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
%% Type exports
%% --------------------------------------------------------------------

-export_type([options/0]).
-export_type([codec_callback/0]).
-export_type([is_proplist/0]).
-export_type([encode/1]).
-export_type([state/0]).

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
%% Types
%% --------------------------------------------------------------------

-type options() :: #{
    codecs => [codec()],
    nulls => [term()],
    skip_values => [term()],
    key_to_binary => fun((term()) -> binary()),
    sort_keys => boolean(),
    proplists => boolean() | {true, is_proplist()},
    escape => fun((binary()) -> iodata()),
    encode_integer => encode(integer()),
    encode_float => encode(float()),
    encode_atom => encode(atom()),
    encode_list => encode(list()),
    encode_map => encode(map()),
    encode_tuple => encode(tuple()),
    encode_pid => encode(pid()),
    encode_port => encode(port()),
    encode_reference => encode(reference())
}.

-type codec() ::
    timestamp
    | datetime
    | ipv4
    | ipv6
    % {records, #{foo => {record_info(fields, foo), record_info(size, foo)}}}
    | {records, #{Name :: atom() := {Fields :: [atom()], Size :: pos_integer()}}}
    | codec_callback().
-export_type([codec/0]).

-type codec_callback() :: fun((tuple()) -> next | {halt, term()}).

-type is_proplist() :: fun((list()) -> boolean()).

-type encode(Type) :: fun((Type, json:encoder(), state()) -> iodata()).

-record(state, {
    codecs :: [codec()],
    nulls :: #{term() := null},
    skip_values :: #{term() := skip},
    key_to_binary :: fun((term()) -> binary()),
    sort_keys :: boolean(),
    proplists :: boolean() | {true, is_proplist()},
    escape :: fun((binary()) -> iodata()),
    encode_integer :: encode(integer()),
    encode_float :: encode(float()),
    encode_atom :: encode(atom()),
    encode_list :: encode(list()),
    encode_map :: encode(map()),
    encode_tuple :: encode(tuple()),
    encode_pid :: encode(pid()),
    encode_port :: encode(port()),
    encode_reference :: encode(reference())
}).
-opaque state() :: #state{}.

%% --------------------------------------------------------------------
%% DocTest
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("doctest/include/doctest.hrl").
-endif.

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-spec encode(term(), options()) -> iodata().
%% @doc Encode a term into an iodata JSON.
%%
%% <em>Example:</em>
%%
%% ```
%% 1> euneus_encoder:encode(foo, #{}).
%% [$", <<"foo">>, $"]
%% '''
%%
%% Option details:
%%
%% <ul>
%%   <blockquote>
%%     <h4 class="info">Note</h4>
%%     For better visualization and understanding, all options examples use
%%     `euneus:encode/2', which returns a binary.
%%   </blockquote>
%%   <li>
%%     `codecs' - Transforms tuples into any other Erlang term that will be encoded
%%     again into a JSON value. By returning `next', the next codec will be called,
%%     or by returning `{halt, Term :: term()}', the Term will be encoded again.
%%
%%     You can use the built-in codecs or your own.
%%     Please see the `t:euneus_encoder:codec/0' type for details.
%%
%%     Default is `[]'.
%%
%%     Built-in codecs:
%%
%%     <ul>
%%       <li>
%%         `timestamp' - Transforms an `t:erlang:timestamp/0' into an ISO 8601 string
%%         with milliseconds.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus:encode({0, 0, 0}, #{codecs => [timestamp]}).
%% <<"\"1970-01-01T00:00:00.000Z\"">>
%% '''
%%       </li>
%%       <li>
%%         `datetime' - Transforms a `t:calendar:datetime/0' into an ISO 8601 string.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus:encode({{1970, 01, 01}, {00, 00, 00}}, #{codecs => [datetime]}).
%% <<"\"1970-01-01T00:00:00Z\"">>
%% '''
%%       </li>
%%       <li>
%%         `ipv4' - Transforms an `t:inet:ip4_address/0' into a JSON string.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus:encode({127, 0, 0, 1}, #{codecs => [ipv4]}).
%% <<"\"127.0.0.1\"">>
%% '''
%%       </li>
%%       <li>
%%         `ipv6' - Transforms an `t:inet:ip6_address/0' into a JSON string.
%%
%%         <em>Example:</em>
%%
%% ```
%% 1> euneus:encode({0, 0, 0, 0, 0, 0, 0, 0}, #{codecs => [ipv6]}).
%% <<"\"::\"">>
%% 2> euneus:encode({0, 0, 0, 0, 0, 0, 0, 1}, #{codecs => [ipv6]}).
%% <<"\"::1\"">>
%% 3> euneus:encode(
%% ..     {0, 0, 0, 0, 0, 0, (192 bsl 8) bor 168, (42 bsl 8) bor 2},
%% ..     #{codecs => [ipv6]}
%% .. ).
%% <<"\"::192.168.42.2\"">>
%% 4> euneus:encode(
%% ..     {0, 0, 0, 0, 0, 16#FFFF, (192 bsl 8) bor 168, (42 bsl 8) bor 2},
%% ..     #{codecs => [ipv6]}
%% .. ).
%% <<"\"::ffff:192.168.42.2\"">>
%% 5> euneus:encode(
%% ..     {16#3ffe, 16#b80, 16#1f8d, 16#2, 16#204, 16#acff, 16#fe17, 16#bf38},
%% ..     #{codecs => [ipv6]}
%% .. ).
%% <<"\"3ffe:b80:1f8d:2:204:acff:fe17:bf38\"">>
%% 6> euneus:encode(
%% ..     {16#fe80, 0, 0, 0, 16#204, 16#acff, 16#fe17, 16#bf38},
%% ..     #{codecs => [ipv6]}
%% .. ).
%% <<"\"fe80::204:acff:fe17:bf38\"">>
%% '''
%%       </li>
%%       <li>
%%         `records' - Transforms records into JSON objects.
%%
%%          <em>Example:</em>
%%
%% ```
%% 1> euneus:encode(
%% ..     % Same as '-record(foo, {bar, baz}).'
%% ..     {foo, bar, baz},
%% ..     #{codecs => [{records, #{
%% ..         % Same as 'foo => {record_info(fields, foo), record_info(size, foo)}'
%% ..         foo => {[bar, baz], 3}
%% ..     }}]}
%% .. ).
%% <<"{\"bar\":\"bar\",\"baz\":\"baz\"}">>
%% '''
%%       </li>
%%     </ul>
%%
%%     Custom codec example:
%%
%% ```
%% 1> euneus:encode({foo}, #{codecs => [fun({foo}) -> {halt, foo} end]}).
%% <<"\"foo\"">>
%% '''
%%   </li>
%%   <li>
%%     `nulls' - Defines which values should be encoded as null.
%%
%%     Default is `[null]'.
%%
%%     <em>Example:</em>
%%
%% ```
%% 1> euneus:encode([null, nil, foo], #{nulls => [null, nil]}).
%% <<"[null,null,\"foo\"]">>
%% '''
%%   </li>
%%   <li>
%%     `skip_values' - Defines which map values should be ignored.
%%     This option permits achieves the same behavior as Javascript,
%%     which ignores undefined values of objects.
%%
%%     Default is `[undefined]'.
%%
%%     <em>Example:</em>
%%
%% ```
%% 1> euneus:encode(
%% ..     #{foo => bar, bar => undefined, baz => null},
%% ..     #{skip_values => [undefined, null]}
%% .. ).
%% <<"{\"foo\":\"bar\"}">>
%% '''
%%   </li>
%%   <li>
%%     `key_to_binary' - Overrides the default conversion of map keys to a string.
%%   </li>
%%   <li>
%%     `sort_keys' - Defines if the object keys should be sorted.
%%
%%     Default is `false'.
%%
%%     <em>Example:</em>
%%
%% ```
%% 1> euneus:encode(#{c => c, a => a, b => b}, #{sort_keys => true}).
%% <<"{\"a\":\"a\",\"b\":\"b\",\"c\":\"c\"}">>
%% '''
%%   </li>
%%   <li>
%%     `proplists' - If true, converts proplists into objects.
%%
%%     Default is `false'.
%%
%%     <em>Example:</em>
%%
%% ```
%% 1> euneus:encode([{foo, bar}, baz], #{proplists => true}).
%% <<"{\"foo\":\"bar\",\"baz\":true}">>
%% 2> euneus:encode(
%% ..     [{foo, bar}, {baz, true}],
%% ..     % Overrides the default is proplist check:
%% ..     #{proplists => {true, fun([{_, _} | _]) -> true end}}
%% .. ).
%% <<"{\"foo\":\"bar\",\"baz\":true}">>
%% '''
%%   </li>
%%   <li>
%%     `escape' - Overrides the default string escaping.
%%   </li>
%%   <li>
%%     `encode_integer' - Overrides the default integer encoder.
%%   </li>
%%   <li>
%%     `encode_float' - Overrides the default float encoder.
%%   </li>
%%   <li>
%%     `encode_atom' - Overrides the default atom encoder.
%%   </li>
%%   <li>
%%     `encode_list' - Overrides the default list encoder.
%%   </li>
%%   <li>
%%     `encode_map' - Overrides the default map encoder.
%%   </li>
%%   <li>
%%     `encode_tuple'- Overrides the default tuple encoder.
%%   </li>
%%   <li>
%%     `encode_pid' - Overrides the default pid encoder.
%%   </li>
%%   <li>
%%     `encode_port' - Overrides the default port encoder.
%%   </li>
%%   <li>
%%     `encode_reference' - Overrides the default reference encoder.
%%   </li>
%% </ul>
encode(Input, Opts) ->
    State = new_state(Opts),
    json:encode(Input, fun(Term, Encode) ->
        encode_term(Term, Encode, State)
    end).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

% State

new_state(Opts) ->
    #state{
        codecs = maps:get(codecs, Opts, []),
        nulls = maps:from_keys(maps:get(nulls, Opts, [null]), null),
        skip_values = maps:from_keys(maps:get(skip_values, Opts, [undefined]), skip),
        key_to_binary = maps:get(key_to_binary, Opts, fun key_to_binary/1),
        sort_keys = maps:get(sort_keys, Opts, false),
        proplists = maps:get(proplists, Opts, false),
        escape = maps:get(escape, Opts, fun json:encode_binary/1),
        encode_integer = maps:get(encode_integer, Opts, fun encode_integer/3),
        encode_float = maps:get(encode_float, Opts, fun encode_float/3),
        encode_atom = maps:get(encode_atom, Opts, fun encode_atom/3),
        encode_list = maps:get(encode_list, Opts, fun encode_list/3),
        encode_map = maps:get(encode_map, Opts, fun encode_map/3),
        encode_tuple = maps:get(encode_tuple, Opts, fun encode_tuple/3),
        encode_pid = maps:get(encode_pid, Opts, fun encode_pid/3),
        encode_port = maps:get(encode_port, Opts, fun encode_port/3),
        encode_reference = maps:get(encode_reference, Opts, fun encode_reference/3)
    }.

key_to_binary(Bin) when is_binary(Bin) ->
    Bin;
key_to_binary(Str) when is_list(Str) ->
    iolist_to_binary(Str);
key_to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
key_to_binary(Int) when is_integer(Int) ->
    integer_to_binary(Int, 10).

% Codecs

traverse_codecs([Codec | Codecs], Tuple) ->
    case codec_callback(Codec, Tuple) of
        next ->
            traverse_codecs(Codecs, Tuple);
        {halt, NewTerm} ->
            NewTerm
    end;
traverse_codecs([], Tuple) ->
    Tuple.

codec_callback(timestamp, Tuple) ->
    timestamp_codec_callback(Tuple);
codec_callback(datetime, Tuple) ->
    datetime_codec_callback(Tuple);
codec_callback(ipv4, Tuple) ->
    ipv4_codec_callback(Tuple);
codec_callback(ipv6, Tuple) ->
    ipv6_codec_callback(Tuple);
codec_callback({records, Records}, Tuple) ->
    records_codec_callback(Tuple, Records);
codec_callback(Callback, Tuple) ->
    Callback(Tuple).

timestamp_codec_callback({MegaSecs, Secs, MicroSecs} = Timestamp) when
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
timestamp_codec_callback(_Tuple) ->
    next.

datetime_codec_callback({{YYYY, MM, DD}, {H, M, S}}) when
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
datetime_codec_callback(_Tuple) ->
    next.

ipv4_codec_callback({_A, _B, _C, _D} = Tuple) ->
    case inet_parse:ntoa(Tuple) of
        {error, einval} ->
            next;
        Ipv4 ->
            {halt, list_to_binary(Ipv4)}
    end;
ipv4_codec_callback(_Tuple) ->
    next.

ipv6_codec_callback({_A, _B, _C, _D, _E, _F, _G, _H} = Tuple) ->
    case inet_parse:ntoa(Tuple) of
        {error, einval} ->
            next;
        Ipv6 ->
            {halt, list_to_binary(Ipv6)}
    end;
ipv6_codec_callback(_Tuple) ->
    next.

records_codec_callback(Tuple, Records) when tuple_size(Tuple) > 1 ->
    Name = element(1, Tuple),
    case Records of
        #{Name := {Fields, Size}} when tuple_size(Tuple) =:= Size ->
            [Name | Values] = tuple_to_list(Tuple),
            Map = proplists:to_map(lists:zip(Fields, Values)),
            {halt, Map};
        #{} ->
            next
    end;
records_codec_callback(_Tuple, _Records) ->
    next.

% Encoders

encode_term(Bin, _Encode, State) when is_binary(Bin) ->
    (State#state.escape)(Bin);
encode_term(Int, Encode, State) when is_integer(Int) ->
    (State#state.encode_integer)(Int, Encode, State);
encode_term(Float, Encode, State) when is_float(Float) ->
    (State#state.encode_float)(Float, Encode, State);
encode_term(Atom, Encode, State) when is_atom(Atom) ->
    (State#state.encode_atom)(Atom, Encode, State);
encode_term(List, Encode, State) when is_list(List) ->
    (State#state.encode_list)(List, Encode, State);
encode_term(Map, Encode, State) when is_map(Map) ->
    (State#state.encode_map)(Map, Encode, State);
encode_term(Tuple, Encode, State) when is_tuple(Tuple) ->
    case traverse_codecs(State#state.codecs, Tuple) of
        NewTuple when is_tuple(NewTuple) ->
            (State#state.encode_tuple)(NewTuple, Encode, State);
        NewTerm ->
            encode_term(NewTerm, Encode, State)
    end;
encode_term(Pid, Encode, State) when is_pid(Pid) ->
    (State#state.encode_pid)(Pid, Encode, State);
encode_term(Port, Encode, State) when is_port(Port) ->
    (State#state.encode_port)(Port, Encode, State);
encode_term(Ref, Encode, State) when is_reference(Ref) ->
    (State#state.encode_reference)(Ref, Encode, State);
encode_term(Term, Encode, State) ->
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

encode_list(List, Encode, #state{proplists = false}) ->
    json:encode_list(List, Encode);
encode_list(List, Encode, #state{proplists = true} = State) ->
    case is_proplist(List) of
        true ->
            encode_proplist(List, Encode, State);
        false ->
            json:encode_list(List, Encode)
    end;
encode_list(List, Encode, #state{proplists = {true, IsProplist}} = State) ->
    case IsProplist(List) of
        true ->
            encode_proplist(List, Encode, State);
        false ->
            json:encode_list(List, Encode)
    end.

encode_proplist(Proplist, Encode, State) ->
    encode_term(proplists:to_map(Proplist), Encode, State).

is_proplist([]) ->
    false;
is_proplist(List) ->
    lists:all(fun is_proplist_prop/1, List).

% Must be the same types handled by key_to_binary/1.
is_proplist_prop({Key, _}) ->
    is_binary(Key) orelse
        is_list(Key) orelse
        is_atom(Key) orelse
        is_integer(Key);
is_proplist_prop(Key) ->
    is_atom(Key).

encode_map(Map, Encode, #state{sort_keys = false, skip_values = ValuesToSkip} = State) ->
    do_encode_map([
        [$,, escape_map_key(Key, State), $: | encode_term(Value, Encode, State)]
     || Key := Value <- Map,
        not is_map_key(Value, ValuesToSkip)
    ]);
encode_map(Map, Encode, #state{sort_keys = true, skip_values = ValuesToSkip} = State) ->
    do_encode_map([
        [$,, escape_map_key(Key, State), $: | encode_term(Value, Encode, State)]
     || {Key, Value} <- lists:keysort(1, maps:to_list(Map)),
        not is_map_key(Value, ValuesToSkip)
    ]).

escape_map_key(Key, State) ->
    (State#state.escape)((State#state.key_to_binary)(Key)).

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
