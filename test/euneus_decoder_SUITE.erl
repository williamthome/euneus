-module(euneus_decoder_SUITE).
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

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

test_decode(Config) when is_list(Config) ->
    [
        ?assertEqual(Expect, euneus_decoder:decode(JSON, Opts))
     || {Expect, JSON, Opts} <- [
            % Null
            {null, <<"null">>, #{}},
            % Simple values
            {
                [null, <<"foo">>, 1, 1.0, #{<<"foo">> => <<"bar">>, <<"bar">> => 1}],
                <<"[null, \"foo\", 1, 1.0, {\"foo\": \"bar\", \"bar\": 1}]">>,
                #{}
            },
            % Object keys: binary
            {#{<<"foo">> => <<"bar">>}, <<"{\"foo\": \"bar\"}">>, #{object_keys => binary}},
            % Object keys: copy
            {#{<<"foo">> => <<"bar">>}, <<"{\"foo\": \"bar\"}">>, #{object_keys => copy}},
            % Object keys: atom
            {#{foo => <<"bar">>}, <<"{\"foo\": \"bar\"}">>, #{object_keys => atom}},
            % Object keys: existing_atom
            {#{foo => <<"bar">>}, <<"{\"foo\": \"bar\"}">>, #{object_keys => existing_atom}},
            % Object keys: fun/1
            {#{<<"foo">> => <<"bar">>}, <<"{\"foo\": \"bar\"}">>, #{
                object_keys => fun(K) -> K end
            }},
            % Codec: copy
            {<<"foo">>, <<"\"foo\"">>, #{codecs => [copy]}},
            % Codec: timestamp
            {{0, 0, 0}, <<"\"1970-01-01T00:00:00.000Z\"">>, #{codecs => [timestamp]}},
            % Codec: datetime
            {{{1970, 1, 1}, {0, 0, 0}}, <<"\"1970-01-01T00:00:00Z\"">>, #{codecs => [datetime]}},
            % Code: ipv4
            {{0, 0, 0, 0}, <<"\"0.0.0.0\"">>, #{codecs => [ipv4]}},
            % Codec: ipv6
            {{0, 0, 0, 0, 0, 0, 0, 0}, <<"\"::\"">>, #{codecs => [ipv6]}},
            {{0, 0, 0, 0, 0, 0, 0, 1}, <<"\"::1\"">>, #{codecs => [ipv6]}},
            {
                {0, 0, 0, 0, 0, 0, (192 bsl 8) bor 168, (42 bsl 8) bor 2},
                <<"\"::192.168.42.2\"">>,
                #{codecs => [ipv6]}
            },
            {
                {0, 0, 0, 0, 0, 16#FFFF, (192 bsl 8) bor 168, (42 bsl 8) bor 2},
                <<"\"::ffff:192.168.42.2\"">>,
                #{codecs => [ipv6]}
            },
            {
                {16#3ffe, 16#b80, 16#1f8d, 16#2, 16#204, 16#acff, 16#fe17, 16#bf38},
                <<"\"3ffe:b80:1f8d:2:204:acff:fe17:bf38\"">>,
                #{codecs => [ipv6]}
            },
            {
                {16#fe80, 0, 0, 0, 16#204, 16#acff, 16#fe17, 16#bf38},
                <<"\"fe80::204:acff:fe17:bf38\"">>,
                #{codecs => [ipv6]}
            },
            % Codec: pid
            {list_to_pid("<0.92.0>"), <<"\"<0.92.0>\"">>, #{codecs => [pid]}},
            % Codec: port
            {list_to_port("#Port<0.1>"), <<"\"#Port<0.1>\"">>, #{codecs => [port]}},
            % Codec: reference
            {
                list_to_ref("#Ref<0.314572725.1088159747.110918>"),
                <<"\"#Ref<0.314572725.1088159747.110918>\"">>,
                #{codecs => [reference]}
            }
        ]
    ].
