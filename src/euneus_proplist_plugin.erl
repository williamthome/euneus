%%%---------------------------------------------------------------------
%%% @copyright 2023 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Proplist plugin.
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
-module(euneus_proplist_plugin).

-behaviour(euneus_plugin).

%% euneus_plugin callbacks

-export([ encode/2, decode/2 ]).

%% Macros

-define(is_key(X), is_binary(X) orelse is_atom(X) orelse is_integer(X)).

%%%=====================================================================
%%% euneus_plugin callbacks
%%%=====================================================================

encode([{X, _} | _] = Proplist, Opts) when ?is_key(X) ->
    Map = proplists:to_map(Proplist),
    {halt, euneus_encoder:encode_map(Map, Opts)};
encode(_Term, _Opts) ->
    next.

decode(_Bin, _Opts) ->
    next.

%%%=====================================================================
%%% Eunit tests
%%%=====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    [ ?assertEqual(Expect, encode(Input, euneus_encoder:parse_opts(Opts)))
      || {Expect, Input, Opts} <- [
        {next, [], #{}},
        {{halt, [${,[$",<<"foo">>,$"],$:,[$",<<"bar">>,$"],$}]}, [{foo, bar}], #{}}
    ]].

decode_test() ->
    [ ?assertEqual(Expect, decode(Input, euneus_decoder:parse_opts(Opts)))
      || {Expect, Input, Opts} <- [
        {next, <<>>, #{}}
    ]].

encode_plugin_test() ->
    [ ?assertEqual(Expect, euneus:encode(Input, Opts))
      || {Expect, Input, Opts} <- [
        { {ok, [${,[$",<<"foo">>,$"],$:,[$",<<"bar">>,$"],$}]}
        , [{foo, bar}]
        , #{plugins => [?MODULE]} }
    ]].

decode_plugin_test() ->
    [ ?assertEqual(Expect, euneus:decode(Input, Opts))
      || {Expect, Input, Opts} <- [
        {{ok, []}, <<"[]">>, #{plugins => [?MODULE]}}
    ]].

-endif.
