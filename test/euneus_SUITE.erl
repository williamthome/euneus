-module(euneus_SUITE).
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
        re:run(atom_to_binary(Fun), <<"^test_">>) =/= nomatch
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

test_encode(Config) when is_list(Config) ->
    [
        ?assertEqual(<<"[\"foo\"]">>, euneus:encode([foo])),
        ?assertEqual(<<"[\"foo\"]">>, euneus:encode([foo], #{}))
    ].

test_encode_iodata(Config) when is_list(Config) ->
    [
        ?assertEqual([$[, [$", <<"foo">>, $"], $]], euneus:encode_iodata([foo])),
        ?assertEqual([$[, [$", <<"foo">>, $"], $]], euneus:encode_iodata([foo], #{}))
    ].

test_decode(Config) when is_list(Config) ->
    [
        ?assertEqual([<<"foo">>], euneus:decode(<<"[\"foo\"]">>)),
        ?assertEqual([<<"foo">>], euneus:decode(<<"[\"foo\"]">>, #{}))
    ].

test_decode_iodata(Config) when is_list(Config) ->
    [
        ?assertEqual([<<"foo">>], euneus:decode_iodata([$[, [$", <<"foo">>, $"], $]])),
        ?assertEqual([<<"foo">>], euneus:decode_iodata([$[, [$", <<"foo">>, $"], $]], #{}))
    ].
