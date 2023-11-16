%%%---------------------------------------------------------------------
%%% @copyright 2023 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Plugin behavior.
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
-module(euneus_plugin).

-compile({ inline_list_funcs, true }).

%% API functions

-export([ normalize_modules_list/1, to_module/1 ]).

%%%=====================================================================
%%% Callbacks
%%%=====================================================================

-callback encode(Input, Opts) -> Output when
    Input :: term(),
    Opts :: euneus_encoder:options(),
    Output :: {halt, iolist()} | next.

-callback decode(Input, Opts) -> Output when
    Input :: binary(),
    Opts :: euneus_decoder:options(),
    Output :: {halt, term()} | next.

%%%=====================================================================
%%% API functions
%%%=====================================================================

normalize_modules_list(Plugins) ->
    lists:map(fun(Plugin) ->
        case to_module(Plugin) of
            {ok, Module} ->
                Module;
            error ->
                Plugin
        end
    end, Plugins).

to_module(datetime) -> {ok, euneus_plugin_datetime_iso8601};
to_module(inet) -> {ok, euneus_plugin_inet};
to_module(pid) -> {ok, euneus_plugin_pid};
to_module(port) -> {ok, euneus_plugin_port};
to_module(proplist) -> {ok, euneus_plugin_proplist};
to_module(reference) -> {ok, euneus_plugin_reference};
to_module(timestamp) -> {ok, euneus_plugin_timestamp_iso8601};
to_module(_) -> error.

%%%=====================================================================
%%% Eunit tests
%%%=====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

to_module_test() ->
    [ ?assertMatch(Expect, to_module(Input))
      || {Expect, Input} <- [
        {{ok, euneus_plugin_datetime_iso8601}, datetime},
        {{ok, euneus_plugin_inet}, inet},
        {{ok, euneus_plugin_pid}, pid},
        {{ok, euneus_plugin_port}, port},
        {{ok, euneus_plugin_proplist}, proplist},
        {{ok, euneus_plugin_reference}, reference},
        {{ok, euneus_plugin_timestamp_iso8601}, timestamp},
        {error, foo}
    ]].

-endif.
