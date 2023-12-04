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
-export([ null_value/1
        , integer_base/1
        , string/1
        , array/1
        , object/1
        , object_return/1
        , object_keys/1
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
    [ null_value
    , string
    , integer_base
    , array
    , object
    , object_return
    , object_keys
    , handle_error
    , codecs
    ].

%%%=====================================================================
%%% Test cases
%%%=====================================================================

null_value(Config) when is_list(Config) ->
    {ok, ?DEFAULT_NULL_VALUE} = decode(<<"null">>, #{}),
    {ok, nil} = decode(<<"null">>, #{null_value => nil}).

integer_base(Config) when is_list(Config) ->
    {ok, 10} = decode(<<"10">>, #{}),
    {ok, 16} = decode(<<"10">>, #{integer_base => 16}).

string(Config) when is_list(Config) ->
    {ok, #{<<"foo">> := <<"bar">>}} = decode(<<"{\"foo\":\"bar\"}">>, #{}),
    {ok, #{<<"foo">> := <<"bar">>}} = decode(<<"{\"foo\":\"bar\"}">>, #{
        string => #{
            codecs => [copy]
        }
    }),
    {ok, #{<<"foo">> := bar}} = decode(<<"{\"foo\":\"bar\"}">>, #{
        string => #{
            codecs => [to_atom]
        }
    }),
    {ok, #{<<"foo">> := bar}} = decode(<<"{\"foo\":\"bar\"}">>, #{
        string => #{
            codecs => [to_existing_atom]
        }
    }),
    {ok, #{<<"0">> := 0}} = decode(<<"{\"0\":\"0\"}">>, #{
        string => #{
            codecs => [to_integer]
        }
    }),
    {ok, #{<<"foo">> := bar}} = decode(<<"{\"foo\":\"bar\"}">>, #{
        string => #{
            codecs => [
                fun(<<"bar">>, _Opts, _Settings) ->
                    {halt, bar}
                end
            ]
        }
    }).

array(Config) when is_list(Config) ->
    {ok, [<<"foo">>,true,0,?DEFAULT_NULL_VALUE]} = decode(<<"[\"foo\",true,0,null]">>, #{}),
    {ok, [1]} = decode(<<"[0]">>, #{
        array => #{
            codecs => [
                fun([0], _Opts, _Settings) ->
                    {halt, [1]}
                end
            ]
        }
    }).

object(Config) when is_list(Config) ->
    {ok, #{<<"foo">> := <<"bar">>, <<"0">> := 0}} =
        decode(<<"{\"foo\":\"bar\",\"0\":0}">>, #{}),
    {ok, #{}} = decode(<<"{\"foo\":\"bar\",\"0\":0}">>, #{
        object => #{
            codecs => [
                fun(#{<<"foo">> := <<"bar">>, <<"0">> := 0}, _Opts, _Settings) ->
                    {halt, #{}}
                end
            ]
        }
    }).

object_return(Config) when is_list(Config) ->
    {ok, #{<<"foo">> := <<"bar">>, <<"0">> := 0}} =
        decode(<<"{\"foo\":\"bar\",\"0\":0}">>, #{
            object => #{
                return => map
            }
        }),
    {ok, [{<<"0">>, 0}, {<<"foo">>, <<"bar">>}]} =
        decode(<<"{\"foo\":\"bar\",\"0\":0}">>, #{
            object => #{
                return => proplist
            }
        }),
    {ok, [{<<"foo">>, <<"bar">>}, {<<"0">>, 0}]} =
        decode(<<"{\"foo\":\"bar\",\"0\":0}">>, #{
            object => #{
                return => ordered_proplist
            }
        }).

object_keys(Config) when is_list(Config) ->
    {ok, #{<<"foo">> := <<"bar">>}} = decode(<<"{\"foo\":\"bar\"}">>, #{}),
    {ok, #{<<"foo">> := <<"bar">>}} = decode(<<"{\"foo\":\"bar\"}">>, #{
        object => #{
            keys => copy
        }
    }),
    {ok, #{foo := <<"bar">>}} = decode(<<"{\"foo\":\"bar\"}">>, #{
        object => #{
            keys => to_atom
        }
    }),
    {ok, #{foo := <<"bar">>}} = decode(<<"{\"foo\":\"bar\"}">>, #{
        object => #{
            keys => to_existing_atom
        }
    }),
    {ok, #{0 := <<"0">>}} = decode(<<"{\"0\":\"0\"}">>, #{
        object => #{
            keys => to_integer
        }
    }),
    {ok, [{foo, <<"bar">>}]} = decode(<<"{\"foo\":\"bar\"}">>, #{
        object => #{
            return => proplist,
            keys => to_atom
        }
    }).

handle_error(Config) when is_list(Config) ->
    {error, bar} = decode(<<"[0]">>, #{
        array => #{
            codecs => [fun([0], _Opts, _Settings) ->
                throw(foo)
            end]
        },
        handle_error => fun (throw, foo, _Stacktrace) ->
            {error, bar}
        end
    }).

codecs(Config) when is_list(Config) ->
    Opts = euneus_decoder:parse_options_to_settings(#{}),
    {halt, {test, foo}} =
        euneus_test_codec:decode(<<"test::foo">>, foo, Opts),
    next =
        euneus_test_codec:decode(<<"test::bar">>, bar, Opts),
    {ok, <<"test::foo">>} = decode(<<"\"test::foo\"">>, #{}),
    {ok, {test, foo}} =
        decode(<<"\"test::foo\"">>, #{
            string => #{
                codecs => [{euneus_test_codec, foo}]
            }
        }).

%%%=====================================================================
%%% Support functions
%%%=====================================================================

decode(Input, Opts) ->
    euneus_decoder:decode(Input, Opts).
