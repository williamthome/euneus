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
        re:run(atom_to_binary(Fun), <<".*_test$">>) =/= nomatch
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

encode_test(Config) when is_list(Config) ->
    [
        ?assertEqual(<<"[\"foo\"]">>, euneus:encode([foo])),
        ?assertEqual(<<"[\"foo\"]">>, euneus:encode([foo], #{}))
    ].

encode_to_iodata_test(Config) when is_list(Config) ->
    [
        ?assertEqual([$[, [$", <<"foo">>, $"], $]], euneus:encode_to_iodata([foo])),
        ?assertEqual([$[, [$", <<"foo">>, $"], $]], euneus:encode_to_iodata([foo], #{}))
    ].

decode_test(Config) when is_list(Config) ->
    [
        ?assertEqual([<<"foo">>], euneus:decode(<<"[\"foo\"]">>)),
        ?assertEqual([<<"foo">>], euneus:decode(<<"[\"foo\"]">>, #{}))
    ].

decode_iodata_test(Config) when is_list(Config) ->
    [
        ?assertEqual([<<"foo">>], euneus:decode_iodata([$[, [$", <<"foo">>, $"], $]])),
        ?assertEqual([<<"foo">>], euneus:decode_iodata([$[, [$", <<"foo">>, $"], $]], #{}))
    ].

decode_stream_test(Config) when is_list(Config) ->
    State = stream_continue_state(euneus:decode_stream_start(<<"{\"foo\":">>)),
    ?assertEqual(
        {end_of_input, #{<<"foo">> => 1}}, euneus:decode_stream_continue(<<"1}">>, State)
    ).

% Wrapper to suppress dialyzer errors.
% See notes in euneus_decoder_SUITE:stream_continue_state/1.
stream_continue_state({continue, State}) ->
    State.

minify_test(Config) when is_list(Config) ->
    ?assertEqual(
        <<"{\"foo\":\"bar\",\"0\":0,[null,true,false,0.001,\"foo\",{\"foo\":0.0001]}">>,
        euneus:minify(<<
            "{\"foo\"   :  \"bar\",\"0\"  :\n   0, [  null, \ntrue,   "
            "false, 0.001, \"foo\" \n, {\n    \"foo\"  \n : \n 0.0001 \n]}"
        >>)
    ).
