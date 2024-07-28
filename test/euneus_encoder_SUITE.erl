-module(euneus_encoder_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [Fun || {Fun, 1} <- ?MODULE:module_info(exports),
            re:run(atom_to_binary(Fun), <<"^test_">>) =/= nomatch].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

test_encode(Config) when is_list(Config) ->
    [?assertEqual(Expect, iolist_to_binary(euneus_encoder:encode(Term, Opts)))
     || {Expect, Term, Opts} <- [
        % Atoms
        {<<"true">>, true, #{}},
        {<<"false">>, false, #{}},
        {<<"null">>, null, #{}},
        {<<"\"foo\"">>, foo, #{}},
        {<<"\"bar\"">>, foo, #{atom => fun(_, _, _) -> <<"\"bar\"">> end}},
        % Drop nulls
        {<<"{\"foo\":\"foo\"}">>, #{foo => foo, bar => null}, #{drop_nulls => true}}
    ]].

