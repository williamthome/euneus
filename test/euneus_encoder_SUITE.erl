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
        , binary_encoder/1
        , atom_encoder/1
        , integer_encoder/1
        , float_encoder/1
        , list_encoder/1
        , map_encoder/1
        , datetime_encoder/1
        , timestamp_encoder/1
        , unhandled_encoder/1
        , escaper/1
        , error_handler/1
        ]).

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
    , binary_encoder
    , atom_encoder
    , integer_encoder
    , float_encoder
    , list_encoder
    , map_encoder
    , datetime_encoder
    , timestamp_encoder
    , unhandled_encoder
    , escaper
    , error_handler
    ].

%%%=====================================================================
%%% Test cases
%%%=====================================================================

nulls(Config) when is_list(Config) ->
    {ok, <<"null">>} = encode(undefined, #{}),
    {ok, <<"null">>} = encode(nil, #{nulls => [nil]}).

binary_encoder(Config) when is_list(Config) ->
    {ok, [$", <<"foo">>, $"]} = encode(<<"foo">>, #{}),
    {ok, <<"\"foo\"">>} = encode(<<"foo">>, #{
        binary_encoder => fun(Bin, _Opts) ->
            <<$", Bin/binary, $">>
        end
    }).

atom_encoder(Config) when is_list(Config) ->
    {ok, <<"true">>} = encode(true, #{}),
    {ok, <<"false">>} = encode(false, #{}),
    {ok, [$", <<"foo">>, $"]} = encode(foo, #{}),
    {ok, <<"\"foo\"">>} = encode(foo, #{
        atom_encoder => fun(Atom, _Opts) ->
            <<$", (atom_to_binary(Atom))/binary, $">>
        end
    }).

integer_encoder(Config) when is_list(Config) ->
    {ok, <<"0">>} = encode(0, #{}),
    {ok, <<"\"0\"">>} = encode(0, #{
        integer_encoder => fun(Int, _Opts) ->
            <<$", (integer_to_binary(Int))/binary, $">>
        end
    }).

float_encoder(Config) when is_list(Config) ->
    {ok, <<"0.0">>} = encode(0.00, #{}),
    {ok, <<"\"1.000e-02\"">>} = encode(0.010, #{
        float_encoder => fun(Float, _Opts) ->
            <<$", (float_to_binary(Float, [{scientific, 3}]))/binary, $">>
        end
    }).

list_encoder(Config) when is_list(Config) ->
    {ok, <<"[]">>} = encode([], #{}),
    {ok, [${, [$", <<"foo">>, $"], $:, [$", <<"bar">>, $"], $}]} =
        encode([{foo, bar}], #{list_encoder => fun
            ([{K, _} | _] = Proplist, Opts)
              when is_binary(K); is_atom(K); is_integer(K) ->
                Map = proplists:to_map(Proplist),
                euneus_encoder:encode_map(Map, Opts);
            (List, Opts) ->
                euneus_encoder:encode_list(List, Opts)
        end
    }).

map_encoder(Config) when is_list(Config) ->
    {ok, <<"{}">>} = encode(#{}, #{}),
    {ok, [${, [$", <<"foo">>, $"], $:, [$", <<"bar">>, $"], $}]} =
        encode(#{}, #{map_encoder => fun (_Map, Opts) ->
            euneus_encoder:encode_map(#{foo => bar}, Opts)
        end
    }).

datetime_encoder(Config) when is_list(Config) ->
    {ok, [$", <<"1970-01-01T00:00:00Z">>, $"]} = encode({{1970,1,1},{0,0,0}}, #{}),
    {ok, [$", <<"2023-01-01T00:00:00Z">>, $"]} = encode({{1970,1,1},{0,0,0}}, #{
        datetime_encoder => fun (_DateTime, Opts) ->
            euneus_encoder:encode_datetime({{2023,1,1},{0,0,0}}, Opts)
        end
    }).

timestamp_encoder(Config) when is_list(Config) ->
    {ok, [$", <<"1970-01-01T00:00:00.000Z">>, $"]} = encode({0,0,0}, #{}),
    {ok, [$", <<"2023-01-01T00:00:00.000Z">>, $"]} = encode({0,0,0}, #{
        timestamp_encoder => fun (_DateTime, Opts) ->
            euneus_encoder:encode_timestamp({1672,531200,0}, Opts)
        end
    }).

unhandled_encoder(Config) when is_list(Config) ->
    {error, {unsupported_type, {foo}}} = encode({foo}, #{}),
    {ok, [$[, [$", <<"myrecord">>, $"], [$,,
        [${, [$", <<"key">>, $"], $:, [$", <<"val">>, $"], $}],
    $]]]} = encode({myrecord, val}, #{
        unhandled_encoder => fun({myrecord, Val}, Opts) ->
            Encode = maps:get(list_encoder, Opts),
            Encode([myrecord, #{key => Val}], Opts)
        end
    }).

escaper(Config) when is_list(Config) ->
    {ok, <<"\"foo\"">>} = encode_to_bin(foo, #{
        escaper => json
    }),
    {ok, <<"\"<\\/script>\"">>} = encode_to_bin(<<"</script>">>, #{
        escaper => html
    }),
    {ok, <<"\"\\u2028\\u2029\"">>} = encode_to_bin(
        unicode:characters_to_binary([8232,8233]), #{
        escaper => javascript
    }),
    {ok, <<"\"\\u2603\"">>} = encode_to_bin(<<"☃"/utf8>>, #{
        escaper => unicode
    }),
    {ok, <<"bar">>} = encode(foo, #{
        escaper => fun (<<"foo">>, _Opts) ->
            <<"bar">>
        end
    }).

error_handler(Config) when is_list(Config) ->
    {error, bar} = encode(foo, #{
        escaper => fun (<<"foo">>, _Opts) ->
            throw(foo)
        end,
        error_handler => fun (throw, foo, _Stacktrace) ->
            {error, bar}
        end
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
