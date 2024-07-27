-module(euneus_decoder_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [Fun || {Fun, 1} <- ?MODULE:module_info(exports),
            re:run(atom_to_binary(Fun), <<"^test_">>) =/= nomatch].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

test_decode(Config) when is_list(Config) ->
    [
        ?assertEqual(null,
                     euneus_decoder:decode(<<"null">>)),
        ?assertEqual([null, 1],
                     euneus_decoder:decode(<<"[null, 1]">>)),
        ?assertEqual(#{<<"foo">> => null, <<"bar">> => 1},
                     euneus_decoder:decode(<<"{\"foo\": null, \"bar\": 1}">>))

    ].

