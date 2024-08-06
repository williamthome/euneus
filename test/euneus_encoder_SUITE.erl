-module(euneus_encoder_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%

-elvis([{elvis_style, dont_repeat_yourself, disable}]).

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

encode_test(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    {ok, Files} = file:list_dir(DataDir),
    lists:foreach(
        fun(Filename) ->
            case string:find(Filename, "invalid") of
                nomatch ->
                    AbsFilename = filename:join(DataDir, Filename),
                    {ok, JSON} = file:read_file(AbsFilename),
                    Term = euneus:decode(JSON),
                    ?assertNotException(
                        error,
                        Reason when
                            Reason =:= unexpected_end;
                            (is_tuple(Reason) andalso element(1, Reason) =:= invalid_byte),
                        encode(Term)
                    );
                _Invalid ->
                    ?assert(true)
            end
        end,
        Files
    ).

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
        ?assertError(unsupported_tuple, encode({0, 0, 0, 256}, #{codecs => [ipv4]})),
        ?assertEqual(<<"\"0.0.0.0\"">>, encode({0, 0, 0, 0}, #{codecs => [ipv4]}))
    ].

ipv6_codec_test(Config) when is_list(Config) ->
    [
        ?assertError(unsupported_tuple, encode({0, 0, 0, 0, 0, 0, 0, -1}, #{codecs => [ipv6]})),
        ?assertEqual(
            <<"\"fe80::204:acff:fe17:bf38\"">>,
            encode({16#fe80, 0, 0, 0, 16#204, 16#acff, 16#fe17, 16#bf38}, #{codecs => [ipv6]})
        )
    ].

-record(foo, {foo, bar}).
-record(bar, {bar, baz}).
-if(?OTP_RELEASE >= 26).
records_codec_test(Config) when is_list(Config) ->
    [
        ?assertError(
            unsupported_tuple,
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
-else.
records_codec_test(Config) when is_list(Config) ->
    [
        ?assertError(
            unsupported_tuple,
            encode({foo, bar, baz, qux}, #{
                codecs => [
                    {records, #{
                        foo => {record_info(fields, foo), record_info(size, foo)}
                    }}
                ]
            })
        ),
        ?assertEqual(
            <<"[{\"foo\":\"foo\",\"bar\":\"bar\"},{\"baz\":\"baz\",\"bar\":\"bar\"}]">>,
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
-endif.

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

-if(?OTP_RELEASE >= 26).
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
-else.
key_to_binary_test(Config) when is_list(Config) ->
    [
        ?assertEqual(
            <<"{\"foo\":\"foo\",\"baz\":\"baz\",\"bar\":\"bar\",\"0\":0}">>,
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
-endif.

sort_keys_test(Config) when is_list(Config) ->
    [
        ?assertEqual(<<"{}">>, encode(#{}, #{sort_keys => true})),
        ?assertEqual(
            <<"{\"1\":1,\"2\":2,\"3\":3}">>,
            encode(#{3 => 3, 1 => 1, 2 => 2}, #{sort_keys => true})
        )
    ].

-if(?OTP_RELEASE >= 26).
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
-else.
proplists_test(Config) when is_list(Config) ->
    [
        ?assertEqual(<<"[]">>, encode([], #{proplists => true})),
        ?assertEqual(
            <<"[null,{\"foo\":\"bar\",\"baz\":true,\"0\":0}]">>,
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
-endif.

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
                encode_map => fun(Map, State) ->
                    euneus_encoder:continue(maps:to_list(Map), State)
                end
            })
        )
    ].

encode_tuple_test(Config) when is_list(Config) ->
    [
        ?assertError(unsupported_tuple, encode({})),
        ?assertEqual(
            <<"[]">>,
            encode({}, #{
                encode_tuple => fun(Tuple, State) ->
                    euneus_encoder:continue(tuple_to_list(Tuple), State)
                end
            })
        )
    ].

encode_pid_test(Config) when is_list(Config) ->
    [
        ?assertError(unsupported_pid, encode(list_to_pid("<0.66.6>"))),
        ?assertEqual(
            <<"\"<0.66.6>\"">>,
            encode(list_to_pid("<0.66.6>"), #{
                encode_pid => fun(Pid, State) ->
                    euneus_encoder:continue(iolist_to_binary(pid_to_list(Pid)), State)
                end
            })
        )
    ].

encode_port_test(Config) when is_list(Config) ->
    [
        ?assertError(unsupported_port, encode(list_to_port("#Port<0.1>"))),
        ?assertEqual(
            <<"\"#Port<0.1>\"">>,
            encode(list_to_port("#Port<0.1>"), #{
                encode_port => fun(Port, State) ->
                    euneus_encoder:continue(iolist_to_binary(port_to_list(Port)), State)
                end
            })
        )
    ].

encode_reference_test(Config) when is_list(Config) ->
    [
        ?assertError(
            unsupported_reference, encode(list_to_ref("#Ref<0.314572725.1088159747.110918>"))
        ),
        ?assertEqual(
            <<"\"#Ref<0.314572725.1088159747.110918>\"">>,
            encode(
                list_to_ref("#Ref<0.314572725.1088159747.110918>"), #{
                    encode_reference => fun(Ref, State) ->
                        euneus_encoder:continue(iolist_to_binary(ref_to_list(Ref)), State)
                    end
                }
            )
        )
    ].

encode_term_test(Config) when is_list(Config) ->
    [
        ?assertError(unsupported_term, encode(fun() -> error end)),
        ?assertNotException(
            error,
            unsupported_term,
            encode(
                fun() -> ok end,
                #{
                    encode_term =>
                        fun(Fun, State) when is_function(Fun) ->
                            euneus_encoder:continue(
                                iolist_to_binary(erlang:fun_to_list(Fun)), State
                            )
                        end
                }
            )
        )
    ].

%% --------------------------------------------------------------------
%% Test support
%% --------------------------------------------------------------------

encode(Term) ->
    euneus:encode(Term).

encode(Term, Opts) ->
    euneus:encode(Term, Opts).
