%%%---------------------------------------------------------------------
%%% @copyright 2023 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc JSON parser tests.
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
-module(euneus_decoder_SUITE).

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
-export([ null_term/1
        , keys/1
        , values/1
        , arrays/1
        , objects/1
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
    [ null_term
    , keys
    , values
    , arrays
    , objects
    ].

%%%=====================================================================
%%% Test cases
%%%=====================================================================

null_term(Config) when is_list(Config) ->
    {ok, undefined} = decode(<<"null">>, #{}),
    {ok, nil} = decode(<<"null">>, #{null_term => nil}).

keys(Config) when is_list(Config) ->
    {ok, #{<<"foo">> := <<"bar">>}} = decode(<<"{\"foo\":\"bar\"}">>, #{}),
    {ok, #{<<"foo">> := <<"bar">>}} = decode(<<"{\"foo\":\"bar\"}">>, #{
        keys => copy
    }),
    {ok, #{foo := <<"bar">>}} = decode(<<"{\"foo\":\"bar\"}">>, #{
        keys => to_atom
    }),
    {ok, #{foo := <<"bar">>}} = decode(<<"{\"foo\":\"bar\"}">>, #{
        keys => to_existing_atom
    }),
    {ok, #{0 := <<"0">>}} = decode(<<"{\"0\":\"0\"}">>, #{
        keys => to_integer
    }),
    {ok, #{foo := <<"bar">>}} = decode(<<"{\"foo\":\"bar\"}">>, #{
        keys => fun(<<"foo">>, _Opts) ->
            foo
        end
    }).

values(Config) when is_list(Config) ->
    {ok, #{<<"foo">> := <<"bar">>}} = decode(<<"{\"foo\":\"bar\"}">>, #{}),
    {ok, #{<<"foo">> := <<"bar">>}} = decode(<<"{\"foo\":\"bar\"}">>, #{
        values => copy
    }),
    {ok, #{<<"foo">> := bar}} = decode(<<"{\"foo\":\"bar\"}">>, #{
        values => to_atom
    }),
    {ok, #{<<"foo">> := bar}} = decode(<<"{\"foo\":\"bar\"}">>, #{
        values => to_existing_atom
    }),
    {ok, #{<<"0">> := 0}} = decode(<<"{\"0\":\"0\"}">>, #{
        values => to_integer
    }),
    {ok, #{<<"foo">> := bar}} = decode(<<"{\"foo\":\"bar\"}">>, #{
        values => fun(<<"bar">>, _Opts) ->
            bar
        end
    }).

arrays(Config) when is_list(Config) ->
    {ok, [<<"foo">>,true,0,undefined]} = decode(<<"[\"foo\",true,0,null]">>, #{}),
    {ok, [foo]} = decode(<<"[]">>, #{
        arrays => fun([], _Opts) ->
            [foo]
        end
    }).

objects(Config) when is_list(Config) ->
    {ok, #{<<"foo">> := <<"bar">>, <<"0">> := undefined}} =
        decode(<<"{\"foo\":\"bar\",\"0\":null}">>, #{}),
    {ok, #{}} = decode(<<"{\"foo\":\"bar\",\"0\":null}">>, #{
        objects => fun(#{<<"foo">> := <<"bar">>, <<"0">> := undefined}, _Opts) ->
            #{}
        end
    }).

%%%=====================================================================
%%% Support functions
%%%=====================================================================

decode(Input, Opts) ->
    euneus_decoder:decode(Input, Opts).
