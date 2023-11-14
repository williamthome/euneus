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
-export([ nulls/1, binary_encoder/1, atom_encoder/1 ]).

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
    [ nulls, binary_encoder, atom_encoder ].

%%%=====================================================================
%%% Test cases
%%%=====================================================================

nulls(Config) when is_list(Config) ->
    {ok, <<"null">>} = euneus_encoder:encode(undefined, #{}),
    {ok, <<"null">>} = euneus_encoder:encode(nil, #{nulls => [nil]}).

binary_encoder(Config) when is_list(Config) ->
    {ok, [$", <<"foo">>, $"]} = euneus_encoder:encode(<<"foo">>, #{}),
    {ok, <<"\"foo\"">>} = euneus_encoder:encode(<<"foo">>, #{
        binary_encoder => fun(Bin, _Opts) -> <<$", Bin/binary, $">> end
    }).

atom_encoder(Config) when is_list(Config) ->
    {ok, <<"true">>} = euneus_encoder:encode(true, #{}),
    {ok, <<"false">>} = euneus_encoder:encode(false, #{}),
    {ok, [$", <<"foo">>, $"]} = euneus_encoder:encode(foo, #{}),
    {ok, <<"\"foo\"">>} = euneus_encoder:encode(foo, #{
        atom_encoder => fun(Atom, _Opts) -> <<$", (atom_to_binary(Atom))/binary, $">> end
    }).
