%%%---------------------------------------------------------------------
%%% @copyright 2023 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc JSON generator tests.
%%%
%%% Copyright 2023 William Fank Thomé
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @end
%%%---------------------------------------------------------------------
-module(euneus_encoder_SUITE).

% -include_lib("common_test/include/ct.hrl").

%% Callback functions
-export([ suite/0
        , all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ nulls/1
        , encode_binary/1
        , encode_atom/1
        , encode_integer/1
        , encode_float/1
        , encode_list/1
        , encode_map/1
        , encode_tuple/1
        , escape/1
        , handle_error/1
        , codecs/1
        ]).

-define(DEFAULT_NULL_VALUE, null).

%%%=====================================================================
%%% Callback functions
%%%=====================================================================

%%----------------------------------------------------------------------
%% @doc Returns list of tuples to set default properties for the suite.
%%
%% @param Info List of key/value pairs.
%%
%% @end
%%----------------------------------------------------------------------
-spec suite() -> Info when
    Info :: [tuple()].

suite() ->
    [].

%%----------------------------------------------------------------------
%% @doc Initialization before the suite.
%%
%% @param Config A list of key/value pairs, holding the test case configuration.
%%
%% @end
%%----------------------------------------------------------------------
-spec init_per_suite(Config0) -> Config when
    Config0 :: [tuple()],
    Config :: [tuple()].

init_per_suite(Config) ->
    Config.

%%----------------------------------------------------------------------
%% @doc Cleanup after the suite.
%%
%% @param Config A list of key/value pairs, holding the test case configuration.
%%
%% @end
%%----------------------------------------------------------------------
-spec end_per_suite(Config) -> Result when
    Config :: [tuple()],
    Result :: term().

end_per_suite(_Config) ->
    ok.

%%----------------------------------------------------------------------
%% @doc Initialization before each test case.
%%
%% @param TestCase Name of the test case that is about to run.
%% @param Config A list of key/value pairs, holding the test case configuration.
%%
%% @end
%%----------------------------------------------------------------------
-spec init_per_testcase(TestCase, Config0) -> Config when
    TestCase :: atom(),
    Config0 :: [tuple()],
    Config :: [tuple()].

init_per_testcase(_TestCase, Config) ->
    Config.

%%----------------------------------------------------------------------
%% @doc Cleanup after each test case.
%%
%% @param TestCase Name of the test case that is finished.
%% @param Config A list of key/value pairs, holding the test case configuration.
%%
%% @end
%%----------------------------------------------------------------------
-spec end_per_testcase(TestCase, Config) -> Result when
    TestCase :: atom(),
    Config :: [tuple()],
    Result :: term().

end_per_testcase(_TestCase, _Config) ->
    ok.

%%----------------------------------------------------------------------
%% @doc Returns the list of groups and test cases that are to be executed.
%%
%% @param GroupName Name of a test case group.
%% @param TestCase Name of a test case.
%%
%% @end
%%----------------------------------------------------------------------
-spec all() -> GroupsAndTestCases when
    GroupsAndTestCases :: [Group | TestCase],
    Group :: {group, GroupName},
    GroupName :: atom(),
    TestCase :: atom().

all() ->
    [ nulls
    , encode_binary
    , encode_atom
    , encode_integer
    , encode_float
    , encode_list
    , encode_map
    , encode_tuple
    , escape
    , handle_error
    , codecs
    ].

%%%=====================================================================
%%% Test cases
%%%=====================================================================

nulls(Config) when is_list(Config) ->
    {ok, <<"null">>} = encode(?DEFAULT_NULL_VALUE, #{}),
    {ok, <<"null">>} = encode(nil, #{null_values => [nil]}).

encode_binary(Config) when is_list(Config) ->
    {ok, [$", <<"foo">>, $"]} = encode(<<"foo">>, #{}),
    {ok, <<"\"foo\"">>} = encode(<<"foo">>, #{
        binary => #{
            encode => fun(Bin, _Opts) ->
                <<$", Bin/binary, $">>
            end
        }
    }).

encode_atom(Config) when is_list(Config) ->
    {ok, <<"true">>} = encode(true, #{}),
    {ok, <<"false">>} = encode(false, #{}),
    {ok, [$", <<"foo">>, $"]} = encode(foo, #{}),
    {ok, <<"\"foo\"">>} = encode(foo, #{
        atom => #{
            encode => fun(Atom, _Opts) ->
                <<$", (atom_to_binary(Atom))/binary, $">>
            end
        }
    }).

encode_integer(Config) when is_list(Config) ->
    {ok, <<"0">>} = encode(0, #{}),
    {ok, <<"\"0\"">>} = encode(0, #{
        integer => #{
            encode => fun(Int, _Opts) ->
                <<$", (integer_to_binary(Int))/binary, $">>
            end
        }
    }).

encode_float(Config) when is_list(Config) ->
    {ok, <<"0.0">>} = encode(0.00, #{}),
    {ok, <<"\"1.000e-02\"">>} = encode(0.010, #{
        float => #{
            encode => fun(Float, _Opts) ->
                <<$", (float_to_binary(Float, [{scientific, 3}]))/binary, $">>
            end
        }
    }).

encode_list(Config) when is_list(Config) ->
    {ok, <<"[]">>} = encode([], #{}),
    {ok, [${, [$", <<"foo">>, $"], $:, [$", <<"bar">>, $"], $}]} =
        encode([{foo, bar}], #{
            list => #{
                encode => fun
                    ([{K, _} | _] = Proplist, Opts)
                      when is_binary(K); is_atom(K); is_integer(K) ->
                        Map = proplists:to_map(Proplist),
                        euneus_encoder:encode_map(Map, Opts);
                    (List, Opts) ->
                        euneus_encoder:encode_list(List, Opts)
                end
            }
    }).

encode_map(Config) when is_list(Config) ->
    {ok, <<"{}">>} = encode(#{}, #{}),
    {ok, [${, [$", <<"foo">>, $"], $:, [$", <<"bar">>, $"], $}]} =
        encode(#{}, #{
            map => #{
                encode => fun (_Map, Opts) ->
                    euneus_encoder:encode_map(#{foo => bar}, Opts)
                end
            }
    }).

encode_tuple(Config) when is_list(Config) ->
    {ok, <<"[\"foo\"]">>} = encode_to_bin({foo}, #{}),
    {ok, <<"[\"myrecord\",{\"key\":\"val\"}]">>} =
        encode_to_bin({myrecord, val}, #{
            tuple => #{
                encode => fun({myrecord, Val}, Opts) ->
                    euneus_encoder:encode_list([myrecord, #{key => Val}], Opts)
                end
            }
    }).

escape(Config) when is_list(Config) ->
    {ok, <<"\"foo\"">>} = encode_to_bin(foo, #{
        escape => json
    }),
    {ok, <<"\"<\\/script>\"">>} = encode_to_bin(<<"</script>">>, #{
        escape => html
    }),
    {ok, <<"\"\\u2028\\u2029\"">>} = encode_to_bin(
        unicode:characters_to_binary([8232,8233]), #{
        escape => javascript
    }),
    {ok, <<"\"\\u2603\"">>} = encode_to_bin(<<"☃"/utf8>>, #{
        escape => unicode
    }),
    {ok, <<"bar">>} = encode(foo, #{
        escape => fun (<<"foo">>, _Opts) ->
            <<"bar">>
        end
    }).

handle_error(Config) when is_list(Config) ->
    {error, bar} = encode(foo, #{
        escape => fun (<<"foo">>, _Opts) ->
            throw(foo)
        end,
        handle_error => fun (throw, foo, _Stacktrace) ->
            {error, bar}
        end
    }).

codecs(Config) when is_list(Config) ->
    Opts = euneus_encoder:parse_options_to_settings(#{}),
    {halt, [$", <<"test::foo">>, $"]} =
        euneus_test_codec:encode({test, foo}, foo, Opts),
    next =
        euneus_test_codec:encode({test, bar}, bar, Opts),
    {ok, <<"[\"test\",\"foo\"]">>} = encode_to_bin({test, foo}, #{}),
    {ok, <<"\"test::foo\"">>} =
        encode_to_bin({test, foo}, #{
            tuple => #{
                codecs => [{euneus_test_codec, foo}]
            }
    }).

%%%=====================================================================
%%% Support functions
%%%=====================================================================

encode(Input, Opts) ->
    euneus_encoder:encode(Input, Opts).

encode_to_bin(Input, Opts) ->
    case encode(Input, Opts) of
        {ok, JSON} ->
            {ok, iolist_to_binary(JSON)};
        {error, Reason} ->
            {error, Reason}
    end.
