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
        re:run(atom_to_binary(Fun), <<".*_test$">>) =/= nomatch
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

decode_test(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    {ok, Files} = file:list_dir(DataDir),
    lists:foreach(
        fun(Filename) ->
            AbsFilename = filename:join(DataDir, Filename),
            {ok, JSON} = file:read_file(AbsFilename),
            assert(Filename, JSON)
        end,
        Files
    ).

assert("y_" ++ _, JSON) ->
    ?assertNotException(
        error,
        Reason when
            Reason =:= unexpected_end;
            (is_tuple(Reason) andalso element(1, Reason) =:= invalid_byte);
            (is_tuple(Reason) andalso element(1, Reason) =:= unexpected_sequence),
        decode(JSON)
    );
assert("n_" ++ _, JSON) ->
    ?assertException(
        error,
        Reason when
            Reason =:= unexpected_end;
            (is_tuple(Reason) andalso element(1, Reason) =:= invalid_byte);
            (is_tuple(Reason) andalso element(1, Reason) =:= unexpected_sequence),
        decode(JSON)
    );
assert("i_" ++ _, _JSON) ->
    ?assert(true).

% FIXME:
% The CI if failing due to this:
% > test/euneus_decoder_SUITE.erl
% > Line 60 Column 1: Function stream_continue_test/1 has no local return
% > Line 64 Column 10: The created fun has no local return
% > Line 64 Column 77: The call euneus_decoder:stream_continue(
%     <<49>>,
%     State1::json:continuation_state()
%   )
%   contains an opaque term as 2nd argument when terms of different types
%   are expected in these positions
%
% stream_start_test(Config) when is_list(Config) ->
%     [
%         ?assertMatch({continue, _}, euneus_decoder:stream_start(<<"{\"foo\":">>, #{})),
%         ?assertEqual({end_of_input, <<"foo">>}, euneus_decoder:stream_start(<<"\"foo\"">>, #{}))
%     ].
%
% stream_continue_test(Config) when is_list(Config) ->
%     {continue, State1} = euneus_decoder:stream_start(<<"{\"foo\":">>, #{}),
%     {continue, State2} = euneus_decoder:stream_start(<<"123">>, #{}),
%     [
%         ?assertMatch({continue, _}, euneus_decoder:stream_continue(<<"1">>, State1)),
%         ?assertEqual(
%             {end_of_input, #{<<"foo">> => 1}}, euneus_decoder:stream_continue(<<"1}">>, State1)
%         ),
%         ?assertError(unexpected_end, euneus_decoder:stream_continue(end_of_input, State1)),
%         ?assertEqual({end_of_input, 123}, euneus_decoder:stream_continue(<<>>, State2))
%     ].

codecs_test(Config) when is_list(Config) ->
    [
        ?assertEqual([], decode(<<"[]">>, #{codecs => []})),
        ?assertEqual([null], decode(<<"[null]">>, #{codecs => [fun(_) -> next end]})),
        ?assertEqual(
            <<"foo">>,
            decode(<<"\"foo\"">>, #{
                codecs => [
                    timestamp,
                    datetime,
                    ipv4,
                    ipv6,
                    pid,
                    port,
                    reference,
                    fun(_) -> next end,
                    copy
                ]
            })
        )
    ].

copy_codec_test(Config) when is_list(Config) ->
    ?assertEqual(<<"foo">>, decode(<<"\"foo\"">>, #{codecs => [copy]})).

timestamp_codec_test(Config) when is_list(Config) ->
    ?assertEqual({0, 0, 0}, decode(<<"\"1970-01-01T00:00:00.000Z\"">>, #{codecs => [timestamp]})).

datetime_codec_test(Config) when is_list(Config) ->
    ?assertEqual(
        {{1970, 1, 1}, {0, 0, 0}}, decode(<<"\"1970-01-01T00:00:00Z\"">>, #{codecs => [datetime]})
    ).

ipv4_codec_test(Config) when is_list(Config) ->
    [
        ?assertEqual({0, 0, 0, 0}, decode(<<"\"0.0.0.0\"">>, #{codecs => [ipv4]})),
        ?assertEqual(<<"0.0.0.256">>, decode(<<"\"0.0.0.256\"">>, #{codecs => [ipv4]})),
        ?assertEqual(<<"00.256">>, decode(<<"\"00.256\"">>, #{codecs => [ipv4]})),
        ?assertEqual(<<"0.256">>, decode(<<"\"0.256\"">>, #{codecs => [ipv4]}))
    ].

ipv6_codec_test(Config) when is_list(Config) ->
    [
        ?assertEqual({0, 0, 0, 0, 0, 0, 0, 0}, decode(<<"\"::\"">>, #{codecs => [ipv6]})),
        ?assertEqual({0, 0, 0, 0, 0, 0, 0, 1}, decode(<<"\"::1\"">>, #{codecs => [ipv6]})),
        ?assertEqual(
            {0, 0, 0, 0, 0, 0, (192 bsl 8) bor 168, (42 bsl 8) bor 2},
            decode(<<"\"::192.168.42.2\"">>, #{codecs => [ipv6]})
        ),
        ?assertEqual(
            {0, 0, 0, 0, 0, 16#FFFF, (192 bsl 8) bor 168, (42 bsl 8) bor 2},
            decode(<<"\"::ffff:192.168.42.2\"">>, #{codecs => [ipv6]})
        ),
        ?assertEqual(
            {16#3ffe, 16#b80, 16#1f8d, 16#2, 16#204, 16#acff, 16#fe17, 16#bf38},
            decode(<<"\"3ffe:b80:1f8d:2:204:acff:fe17:bf38\"">>, #{codecs => [ipv6]})
        ),
        ?assertEqual(
            {16#fe80, 0, 0, 0, 16#204, 16#acff, 16#fe17, 16#bf38},
            decode(<<"\"fe80::204:acff:fe17:bf38\"">>, #{codecs => [ipv6]})
        ),
        ?assertEqual(<<"::x">>, decode(<<"\"::x\"">>, #{codecs => [ipv6]}))
    ].

pid_codec_test(Config) when is_list(Config) ->
    [
        ?assertEqual(list_to_pid("<0.92.0>"), decode(<<"\"<0.92.0>\"">>, #{codecs => [pid]})),
        ?assertEqual(<<"<x">>, decode(<<"\"<x\"">>, #{codecs => [pid]}))
    ].

port_codec_test(Config) when is_list(Config) ->
    [
        ?assertEqual(
            list_to_port("#Port<0.1>"), decode(<<"\"#Port<0.1>\"">>, #{codecs => [port]})
        ),
        ?assertEqual(<<"#Port<x">>, decode(<<"\"#Port<x\"">>, #{codecs => [port]}))
    ].

reference_codec_test(Config) when is_list(Config) ->
    [
        ?assertEqual(
            list_to_ref("#Ref<0.314572725.1088159747.110918>"),
            decode(<<"\"#Ref<0.314572725.1088159747.110918>\"">>, #{codecs => [reference]})
        ),
        ?assertEqual(
            <<"#Ref<x">>,
            decode(<<"\"#Ref<x\"">>, #{codecs => [reference]})
        )
    ].

null_test(Config) when is_list(Config) ->
    [
        ?assertEqual(null, decode(<<"null">>)),
        ?assertEqual(foo, decode(<<"null">>, #{null => foo}))
    ].

binary_to_float_test(Config) when is_list(Config) ->
    [
        ?assertEqual(0.0001, decode(<<"0.0001">>)),
        ?assertEqual(foo, decode(<<"0.0001">>, #{binary_to_float => fun(_) -> foo end}))
    ].

binary_to_integer_test(Config) when is_list(Config) ->
    [
        ?assertEqual(0, decode(<<"0">>)),
        ?assertEqual(foo, decode(<<"0">>, #{binary_to_integer => fun(_) -> foo end}))
    ].

array_start_test(Config) when is_list(Config) ->
    [
        ?assertEqual([], decode(<<"[]">>)),
        ?assertEqual([foo], decode(<<"[]">>, #{array_start => fun(_) -> [foo] end}))
    ].

array_push_test(Config) when is_list(Config) ->
    ?assertEqual([foo], decode(<<"[null]">>, #{array_push => fun(_, _) -> [foo] end})).

array_finish_test(Config) when is_list(Config) ->
    [
        ?assertEqual([0, 1, 2], decode(<<"[0,1,2]">>)),
        ?assertEqual([2, 1, 0], decode(<<"[0,1,2]">>, #{array_finish => reversed})),
        ?assertEqual([], decode(<<"[0,1,2]">>, #{array_finish => fun(_, _) -> {[], []} end}))
    ].

object_push_test(Config) when is_list(Config) ->
    ?assertEqual(#{}, decode(<<"{\"foo\":\"bar\"}">>, #{object_push => fun(_, _, _) -> [] end})).

object_keys_test(Config) when is_list(Config) ->
    [
        ?assertEqual(#{<<"foo">> => <<"bar">>}, decode(<<"{\"foo\":\"bar\"}">>)),
        ?assertEqual(
            #{<<"foo">> => <<"bar">>}, decode(<<"{\"foo\":\"bar\"}">>, #{object_keys => copy})
        ),
        ?assertEqual(
            #{foo => <<"bar">>}, decode(<<"{\"foo\":\"bar\"}">>, #{object_keys => atom})
        ),
        ?assertEqual(
            #{foo => <<"bar">>}, decode(<<"{\"foo\":\"bar\"}">>, #{object_keys => existing_atom})
        ),
        ?assertEqual(
            #{bar => <<"bar">>},
            decode(<<"{\"foo\":\"bar\"}">>, #{object_keys => fun(_) -> bar end})
        )
    ].

object_finish_test(Config) when is_list(Config) ->
    [
        ?assertEqual(#{}, decode(<<"{}">>)),
        ?assertEqual(
            [{<<"foo">>, <<"bar">>}, {<<"bar">>, <<"baz">>}],
            decode(<<"{\"foo\":\"bar\",\"bar\":\"baz\"}">>, #{object_finish => proplist})
        ),
        ?assertEqual(
            [{<<"bar">>, <<"baz">>}, {<<"foo">>, <<"bar">>}],
            decode(<<"{\"foo\":\"bar\",\"bar\":\"baz\"}">>, #{object_finish => reversed_proplist})
        ),
        ?assertEqual([], decode(<<"{}">>, #{object_finish => fun(_, _) -> {[], []} end}))
    ].

%% --------------------------------------------------------------------
%% Test support
%% --------------------------------------------------------------------

decode(Bin) ->
    euneus:decode(Bin).

decode(Bin, Opts) ->
    euneus:decode(Bin, Opts).
