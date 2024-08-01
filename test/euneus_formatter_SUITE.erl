-module(euneus_formatter_SUITE).
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
%% -------------------------------------------------------------------

format_test(Config) when is_list(Config) ->
    [
        ?assertEqual(
            <<"{\r\t\t\"foo\":\"bar\"\r}">>,
            format(<<"\r\n\t{\"foo\":\"bar\"}">>, #{
                indent_type => tabs,
                indent_width => 2,
                spaced_values => false,
                crlf => r
            })
        ),
        ?assertEqual(
            <<"[\r\n\t\t\"foo\",\r\n\t\t\"bar\"\r\n]">>,
            format(<<"[\"foo\",\"bar\"]">>, #{
                indent_type => tabs,
                indent_width => 2,
                spaced_values => false,
                crlf => rn
            })
        ),
        ?assertEqual(
            <<
                "[\n  true,\n  false,\n  null,\n  {\n    "
                "\"foo\": \"b\\\"ar\",\n    \"bar\": 0\n  }\n]"
            >>,
            format(
                <<"[true, false, null, {\"foo\":\"b\\\"ar\", \"bar\": 0}]">>,
                #{
                    indent_type => spaces,
                    indent_width => 2,
                    spaced_values => true,
                    crlf => n
                }
            )
        )
    ].

%% --------------------------------------------------------------------
%% Test support
%% --------------------------------------------------------------------

format(JSON, Opts) ->
    euneus:format(JSON, Opts).
