%%%---------------------------------------------------------------------
%%% @copyright 2023 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Inet plugin.
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
-module(euneus_plugin_inet).

-behaviour(euneus_plugin).

%% euneus_plugin callbacks

-export([ encode/2, decode/2 ]).

%%%=====================================================================
%%% euneus_plugin callbacks
%%%=====================================================================

encode(Ip, Opts) ->
    case inet:ntoa(Ip) of
        {error, einval} ->
            next;
        IpStr ->
            IpBin = list_to_binary(IpStr),
            {halt, euneus_encoder:encode_binary(IpBin, Opts)}
    end.

decode(Bin, _Opts) ->
    Addr = binary_to_list(Bin),
    case inet:parse_address(Addr) of
        {ok, Ip} ->
            {halt, Ip};
        {error, einval} ->
            next
    end.

%%%=====================================================================
%%% Eunit tests
%%%=====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    [ ?assertEqual(Expect, encode(Input, euneus_encoder:parse_opts(Opts)))
      || {Expect, Input, Opts} <- [
        {next, {-1,0,0,0}, #{}},
        {next, {0,0,0,256}, #{}},
        {next, {-1,0,0,0,0,0,0,0}, #{}},
        {next, {0,0,0,0,0,0,0,65636}, #{}},
        % ipv4
        {{halt, [$", <<"0.0.0.0">>, $"]}, {0,0,0,0}, #{}},
        {{halt, [$", <<"255.255.255.255">>, $"]}, {255,255,255,255}, #{}},
        % ipv6
        {{halt, [$", <<"::">>, $"]}, {0,0,0,0,0,0,0,0}, #{}},
        {{halt, [$", <<"::1">>, $"]}, {0,0,0,0,0,0,0,1}, #{}},
        {{halt, [$", <<"::192.168.42.2">>, $"]}, {0,0,0,0,0,0,(192 bsl 8) bor 168,(42 bsl 8) bor 2}, #{}},
        {{halt, [$", <<"::ffff:192.168.42.2">>, $"]}, {0,0,0,0,0,16#FFFF,(192 bsl 8) bor 168,(42 bsl 8) bor 2}, #{}},
        {{halt, [$", <<"3ffe:b80:1f8d:2:204:acff:fe17:bf38">>, $"]}, {16#3ffe,16#b80,16#1f8d,16#2,16#204,16#acff,16#fe17,16#bf38}, #{}},
        {{halt, [$", <<"fe80::204:acff:fe17:bf38">>, $"]}, {16#fe80,0,0,0,16#204,16#acff,16#fe17,16#bf38}, #{}}
    ]].

decode_test() ->
    [ ?assertEqual(Expect, decode(Input, euneus_decoder:parse_opts(Opts)))
      || {Expect, Input, Opts} <- [
        {next, <<"-1.0.0.0">>, #{}},
        {next, <<"0.0.0.256">>, #{}},
        % ipv4
        {{halt, {0,0,0,0}}, <<"0.0.0.0">>, #{}},
        {{halt, {255,255,255,255}}, <<"255.255.255.255">>, #{}},
        % ipv6
        {{halt, {0,0,0,0,0,0,0,0}}, <<"::">>, #{}},
        {{halt, {0,0,0,0,0,0,0,1}}, <<"::1">>, #{}},
        {{halt, {0,0,0,0,0,0,(192 bsl 8) bor 168,(42 bsl 8) bor 2}}, <<"::192.168.42.2">>, #{}},
        {{halt, {0,0,0,0,0,16#FFFF,(192 bsl 8) bor 168,(42 bsl 8) bor 2}}, <<"::ffff:192.168.42.2">>, #{}},
        {{halt, {16#3ffe,16#b80,16#1f8d,16#2,16#204,16#acff,16#fe17,16#bf38}}, <<"3ffe:b80:1f8d:2:204:acff:fe17:bf38">>, #{}},
        {{halt, {16#fe80,0,0,0,16#204,16#acff,16#fe17,16#bf38}}, <<"fe80::204:acff:fe17:bf38">>, #{}}
    ]].

encode_plugin_test() ->
    [ ?assertEqual(Expect, euneus:encode(Input, Opts))
      || {Expect, Input, Opts} <- [
        {{ok, [$", <<"0.0.0.0">>, $"]}, {0,0,0,0}, #{plugins => [?MODULE]}}
    ]].

decode_plugin_test() ->
    [ ?assertEqual(Expect, euneus:decode(Input, Opts))
      || {Expect, Input, Opts} <- [
        {{ok, {0,0,0,0}}, <<"\"0.0.0.0\"">>, #{plugins => [?MODULE]}}
    ]].

-endif.
