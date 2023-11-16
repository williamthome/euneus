%%%---------------------------------------------------------------------
%%% @copyright 2023 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Port plugin.
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
-module(euneus_plugin_port).

-behaviour(euneus_plugin).

%% euneus_plugin callbacks

-export([ encode/2, decode/2 ]).

%%%=====================================================================
%%% euneus_plugin callbacks
%%%=====================================================================

encode(Port, Opts) when is_port(Port) ->
    PortBin = iolist_to_binary(port_to_list(Port)),
    {halt, euneus_encoder:encode_binary(PortBin, Opts)};
encode(_Term, _Opts) ->
    next.

decode(Bin, _Opts) ->
    try
        Port = list_to_port(binary_to_list(Bin)),
        {halt, Port}
    catch
        error:badarg ->
            next
    end.

%%%=====================================================================
%%% Eunit tests
%%%=====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

port_bin() ->
    <<"#Port<0.1>">>.

port() ->
    list_to_port(binary_to_list(port_bin())).

encode_test() ->
    [ ?assertEqual(Expect, encode(Input, euneus_encoder:parse_opts(Opts)))
      || {Expect, Input, Opts} <- [
        {next, #{}, #{}},
        {{halt, [$", port_bin(), $"]}, port(), #{}}
    ]].

decode_test() ->
    [ ?assertEqual(Expect, decode(Input, euneus_decoder:parse_opts(Opts)))
      || {Expect, Input, Opts} <- [
        {next, <<"{}">>, #{}},
        {{halt, port()}, port_bin(), #{}}
    ]].

encode_plugin_test() ->
    [ ?assertEqual(Expect, euneus:encode(Input, Opts))
      || {Expect, Input, Opts} <- [
        {{ok, [$", port_bin(), $"]}, port(), #{plugins => [?MODULE]}}
    ]].

decode_plugin_test() ->
    [ ?assertEqual(Expect, euneus:decode(Input, Opts))
      || {Expect, Input, Opts} <- [
        {{ok, port()}, <<$", (port_bin())/binary, $">>, #{plugins => [?MODULE]}}
    ]].

-endif.
