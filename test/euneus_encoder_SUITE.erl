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
        re:run(atom_to_binary(Fun), <<"^test_">>) =/= nomatch
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

-record(foo, {foo, bar}).
-record(bar, {bar, baz}).

test_encode(Config) when is_list(Config) ->
    [
        ?assertEqual(Expect, iolist_to_binary(euneus_encoder:encode(Term, Opts)))
     || {Expect, Term, Opts} <- [
            % Nulls
            {<<"[\"foo\",null,null]">>, [foo, null, undefined], #{nulls => [null, undefined]}},
            % Drop nulls
            {<<"{\"foo\":\"foo\"}">>, #{foo => foo, bar => null}, #{drop_nulls => true}},
            % Atoms
            {<<"true">>, true, #{}},
            {<<"false">>, false, #{}},
            {<<"null">>, null, #{}},
            {<<"\"foo\"">>, foo, #{}},
            {<<"\"bar\"">>, foo, #{
                atom => fun(_Term, Encode, _State) -> Encode(<<"bar">>, Encode) end
            }},
            % Proplist
            {<<"{\"foo\":\"foo\",\"bar\":true}">>, [{foo, foo}, bar], #{proplist => true}},
            % Sort keys
            {<<"{\"a\":\"a\",\"b\":\"b\",\"c\":\"c\",\"d\":\"d\",\"e\":\"e\"}">>,
                #{c => c, d => d, a => a, e => e, b => b}, #{sort_keys => true}},
            % Datetime
            {<<"\"1970-01-01T00:00:00Z\"">>, {{1970, 01, 01}, {00, 00, 00}}, #{
                tuple => [datetime]
            }},
            % Timestamp
            {<<"\"1970-01-01T00:00:00.000Z\"">>, {0, 0, 0}, #{tuple => [timestamp]}},
            % IPv4
            {<<"\"0.0.0.0\"">>, {0, 0, 0, 0}, #{tuple => [ipv4]}},
            % IPv6
            {<<"\"fe80::204:acff:fe17:bf38\"">>,
                {16#fe80, 0, 0, 0, 16#204, 16#acff, 16#fe17, 16#bf38}, #{tuple => [ipv6]}},
            % Record
            {<<"[{\"foo\":\"foo\",\"bar\":\"bar\"},{\"bar\":\"bar\",\"baz\":\"baz\"}]">>,
                [
                    #foo{foo = foo, bar = bar},
                    #bar{bar = bar, baz = baz}
                ],
                #{
                    tuple => [
                        {record, [
                            {foo, record_info(fields, foo)},
                            {bar, record_info(fields, bar)}
                        ]}
                    ]
                }},
            % Pid
            {<<"\"<0.92.0>\"">>, list_to_pid("<0.92.0>"), #{
                pid => fun(Pid, Encode, _State) ->
                    Encode(iolist_to_binary(pid_to_list(Pid)), Encode)
                end
            }},
            % Port
            {<<"\"#Port<0.1>\"">>, list_to_port("#Port<0.1>"), #{
                port => fun(Port, Encode, _State) ->
                    Encode(iolist_to_binary(port_to_list(Port)), Encode)
                end
            }},
            % Reference
            {<<"\"#Ref<0.314572725.1088159747.110918>\"">>,
                list_to_ref("#Ref<0.314572725.1088159747.110918>"), #{
                    reference => fun(Ref, Encode, _State) ->
                        Encode(iolist_to_binary(ref_to_list(Ref)), Encode)
                    end
                }}
        ]
    ].
