-module(euneus_encoder_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        Fun
     || {Fun, 1} <- ?MODULE:module_info(exports),
        re:run(atom_to_binary(Fun), <<".*_test$">>) =/= nomatch
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

codecs_test(Config) when is_list(Config) ->
    [
        ?assertEqual(<<"\"foo\"">>, encode(foo, #{codecs => []})),
        ?assertEqual(
            <<"\"foo\"">>,
            encode({foo}, #{
                codecs => [
                    timestamp,
                    datetime,
                    ipv4,
                    ipv6,
                    {records, #{}},
                    fun({foo}) -> {halt, foo} end
                ]
            })
        )
    ].

timestamp_codec_test(Config) when is_list(Config) ->
    ?assertEqual(<<"\"1970-01-01T00:00:00.000Z\"">>, encode({0, 0, 0}, #{codecs => [timestamp]})).

datetime_codec_test(Config) when is_list(Config) ->
    ?assertEqual(
        <<"\"1970-01-01T00:00:00Z\"">>,
        encode({{1970, 01, 01}, {00, 00, 00}}, #{codecs => [datetime]})
    ).

ipv4_codec_test(Config) when is_list(Config) ->
    [
        ?assertError(unsuported_tuple, encode({0, 0, 0, 256}, #{codecs => [ipv4]})),
        ?assertEqual(<<"\"0.0.0.0\"">>, encode({0, 0, 0, 0}, #{codecs => [ipv4]}))
    ].

ipv6_codec_test(Config) when is_list(Config) ->
    [
        ?assertError(unsuported_tuple, encode({0, 0, 0, 0, 0, 0, 0, -1}, #{codecs => [ipv6]})),
        ?assertEqual(
            <<"\"fe80::204:acff:fe17:bf38\"">>,
            encode({16#fe80, 0, 0, 0, 16#204, 16#acff, 16#fe17, 16#bf38}, #{codecs => [ipv6]})
        )
    ].

-record(foo, {foo, bar}).
-record(bar, {bar, baz}).
records_codec_test(Config) when is_list(Config) ->
    [
        ?assertError(
            unsuported_tuple,
            encode({foo, bar, baz, qux}, #{
                codecs => [
                    {records, #{
                        foo => {record_info(fields, foo), record_info(size, foo)}
                    }}
                ]
            })
        ),
        ?assertEqual(
            <<"[{\"foo\":\"foo\",\"bar\":\"bar\"},{\"bar\":\"bar\",\"baz\":\"baz\"}]">>,
            encode(
                [
                    #foo{foo = foo, bar = bar},
                    #bar{bar = bar, baz = baz}
                ],
                #{
                    codecs => [
                        {records, #{
                            foo => {record_info(fields, foo), record_info(size, foo)},
                            bar => {record_info(fields, bar), record_info(size, bar)}
                        }}
                    ]
                }
            )
        )
    ].

nulls_test(Config) when is_list(Config) ->
    [
        ?assertEqual(<<"null">>, encode(null)),
        ?assertEqual(<<"\"null\"">>, encode(null, #{nulls => []}))
    ].

skip_values_test(Config) when is_list(Config) ->
    [
        ?assertEqual(<<"{\"foo\":\"bar\"}">>, encode(#{foo => bar, baz => undefined})),
        ?assertEqual(
            <<"{\"foo\":\"bar\",\"baz\":\"undefined\"}">>,
            encode(#{foo => bar, baz => undefined}, #{skip_values => []})
        )
    ].

key_to_binary_test(Config) when is_list(Config) ->
    [
        ?assertEqual(
            <<"{\"0\":0,\"bar\":\"bar\",\"baz\":\"baz\",\"foo\":\"foo\"}">>,
            encode(#{
                <<"foo">> => foo,
                bar => bar,
                "baz" => baz,
                0 => 0
            })
        ),
        ?assertEqual(
            <<"{\"hello\":\"world\"}">>,
            encode(#{foo => world}, #{key_to_binary => fun(_) -> <<"hello">> end})
        )
    ].

sort_keys_test(Config) when is_list(Config) ->
    [
        ?assertEqual(<<"{}">>, encode(#{}, #{sort_keys => true})),
        ?assertEqual(
            <<"{\"1\":1,\"2\":2,\"3\":3}">>,
            encode(#{3 => 3, 1 => 1, 2 => 2}, #{sort_keys => true})
        )
    ].

proplists_test(Config) when is_list(Config) ->
    [
        ?assertEqual(<<"[]">>, encode([], #{proplists => true})),
        ?assertEqual(
            <<"[null,{\"0\":0,\"foo\":\"bar\",\"baz\":true}]">>,
            encode([null, [{foo, bar}, baz, {0, 0}]], #{proplists => true})
        ),
        ?assertEqual(
            <<"[null,{\"foo\":\"bar\"}]">>,
            encode([null, [{foo, bar}]], #{
                proplists =>
                    {true, fun
                        ([{_, _}]) -> true;
                        (_) -> false
                    end}
            })
        )
    ].

escape_test(Config) when is_list(Config) ->
    ?assertEqual(<<"bar">>, encode(foo, #{escape => fun(_) -> <<"bar">> end})).

encode_integer_test(Config) when is_list(Config) ->
    ?assertEqual(<<"0">>, encode(0)).

encode_float_test(Config) when is_list(Config) ->
    ?assertEqual(<<"0.0001">>, encode(0.0001)).

encode_atom_test(Config) when is_list(Config) ->
    [
        ?assertEqual(<<"true">>, encode(true)),
        ?assertEqual(<<"false">>, encode(false)),
        ?assertEqual(<<"null">>, encode(null)),
        ?assertEqual(<<"\"foo\"">>, encode(foo))
    ].

encode_list_test(Config) when is_list(Config) ->
    [
        ?assertEqual(<<"[]">>, encode([])),
        ?assertEqual(<<"[\"foo\",\"bar\"]">>, encode([foo, bar]))
    ].

encode_map_test(Config) when is_list(Config) ->
    [
        ?assertEqual(<<"{}">>, encode(#{})),
        ?assertEqual(<<"{\"foo\":\"bar\"}">>, encode(#{foo => bar})),
        ?assertEqual(
            <<"[]">>,
            encode(#{}, #{
                encode_map => fun(Map, Encode, _State) ->
                    Encode(maps:to_list(Map), Encode)
                end
            })
        )
    ].

encode_tuple_test(Config) when is_list(Config) ->
    [
        ?assertError(unsuported_tuple, encode({})),
        ?assertEqual(
            <<"[]">>,
            encode({}, #{
                encode_tuple => fun(Tuple, Encode, _State) ->
                    Encode(tuple_to_list(Tuple), Encode)
                end
            })
        )
    ].

encode_pid_test(Config) when is_list(Config) ->
    [
        ?assertError(unsuported_pid, encode(list_to_pid("<0.66.6>"))),
        ?assertEqual(
            <<"\"<0.66.6>\"">>,
            encode(list_to_pid("<0.66.6>"), #{
                encode_pid => fun(Pid, Encode, _State) ->
                    Encode(iolist_to_binary(pid_to_list(Pid)), Encode)
                end
            })
        )
    ].

encode_port_test(Config) when is_list(Config) ->
    [
        ?assertError(unsuported_port, encode(list_to_port("#Port<0.1>"))),
        ?assertEqual(
            <<"\"#Port<0.1>\"">>,
            encode(list_to_port("#Port<0.1>"), #{
                encode_port => fun(Port, Encode, _State) ->
                    Encode(iolist_to_binary(port_to_list(Port)), Encode)
                end
            })
        )
    ].

encode_reference_test(Config) when is_list(Config) ->
    [
        ?assertError(
            unsuported_reference, encode(list_to_ref("#Ref<0.314572725.1088159747.110918>"))
        ),
        ?assertEqual(
            <<"\"#Ref<0.314572725.1088159747.110918>\"">>,
            encode(
                list_to_ref("#Ref<0.314572725.1088159747.110918>"), #{
                    encode_reference => fun(Ref, Encode, _State) ->
                        Encode(iolist_to_binary(ref_to_list(Ref)), Encode)
                    end
                }
            )
        )
    ].

unsuported_term_test() ->
    ?assertError(unsuported_term, encode(fun() -> error end)).

%% --------------------------------------------------------------------
%% Test support
%% --------------------------------------------------------------------

encode(Term) ->
    euneus:encode(Term).

encode(Term, Opts) ->
    euneus:encode(Term, Opts).
