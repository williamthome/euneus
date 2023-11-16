%%%---------------------------------------------------------------------
%%% @copyright 2023 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Timestamp ISO8601 plugin.
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
-module(euneus_plugin_timestamp_iso8601).

-behaviour(euneus_plugin).

-compile({ inline, chars_to_integer/2 }).
-compile({ inline, chars_to_integer/3 }).
-compile({ inline, chars_to_integer/4 }).

%% euneus_plugin callbacks

-export([ encode/2, decode/2 ]).

%% Macros

-define(min(X, Min), is_integer(X) andalso X >= Min).
-define(is_number(X), X >= $0, X =< $9).

%%%=====================================================================
%%% euneus_plugin callbacks
%%%=====================================================================

encode({MegaSecs, Secs, MicroSecs} = Timestamp, Opts)
  when ?min(MegaSecs, 0), ?min(Secs, 0), ?min(MicroSecs, 0) ->
    MilliSecs = MicroSecs div 1000,
    {{YYYY,MM,DD},{H,M,S}} = calendar:now_to_datetime(Timestamp),
    DateTime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
        [YYYY,MM,DD,H,M,S,MilliSecs])
    ),
    {halt, euneus_encoder:encode_binary(DateTime, Opts)};
encode(_Term, _Opts) ->
    next.

decode(<< Y4/integer, Y3/integer, Y2/integer, Y1/integer, $-/integer
        , M2/integer, M1/integer, $-/integer
        , D2/integer, D1/integer
        , $T/integer
        , H2/integer, H1/integer, $:/integer
        , Min2/integer, Min1/integer, $:/integer
        , S2/integer, S1/integer
        , $./integer
        , MSec3/integer, MSec2/integer, MSec1/integer
        , $Z/integer >>, _Opts)
  when ?is_number(Y4), ?is_number(Y3), ?is_number(Y2), ?is_number(Y1)
     , ?is_number(M2), ?is_number(M1)
     , ?is_number(D2), ?is_number(D1)
     , ?is_number(H2), ?is_number(H1)
     , ?is_number(Min2), ?is_number(Min1)
     , ?is_number(S2), ?is_number(S1)
     , ?is_number(MSec3), ?is_number(MSec2), ?is_number(MSec1) ->
    Date = { chars_to_integer(Y4, Y3, Y2, Y1)
           , chars_to_integer(M2, M1)
           , chars_to_integer(D2, D1) },
    Time = { chars_to_integer(H2, H1)
           , chars_to_integer(Min2, Min1)
           , chars_to_integer(S2, S1) },
    DateTime = {Date, Time},
    MilliSeconds = chars_to_integer(MSec3, MSec2, MSec1),
    GregSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
    Seconds = GregSeconds - 62167219200,
    Timestamp = { Seconds div 1000000
                , Seconds rem 1000000
                , MilliSeconds * 1000 },
    {halt, Timestamp};
decode(_Bin, _Opts) ->
    next.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

chars_to_integer(N2, N1) ->
    ((N2 - $0) * 10) + (N1 - $0).

chars_to_integer(N3, N2, N1) ->
    ((N3 - $0) * 100) + ((N2 - $0) * 10) + (N1 - $0).

chars_to_integer(N4, N3, N2, N1) ->
    ((N4 - $0) * 1000) + ((N3 - $0) * 100) + ((N2 - $0) * 10) + (N1 - $0).

%%%=====================================================================
%%% Eunit tests
%%%=====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    [ ?assertEqual(Expect, encode(Input, euneus_encoder:parse_opts(Opts)))
      || {Expect, Input, Opts} <- [
        {next, {{0,0,0},{0,0,0}}, #{}},
        {{halt, [$", <<"1970-01-01T00:00:00.000Z">>, $"]}, {0,0,0}, #{}}
    ]].

decode_test() ->
    [ ?assertEqual(Expect, decode(Input, euneus_decoder:parse_opts(Opts)))
      || {Expect, Input, Opts} <- [
        {next, <<"1970-01-01T00:00:00Z">>, #{}},
        {{halt, {0,0,0}}, <<"1970-01-01T00:00:00.000Z">>, #{}}
    ]].

encode_plugin_test() ->
    [ ?assertEqual(Expect, euneus:encode(Input, Opts))
      || {Expect, Input, Opts} <- [
        {{ok, [$", <<"1970-01-01T00:00:00.000Z">>, $"]}, {0,0,0}, #{plugins => [?MODULE]}}
    ]].

decode_plugin_test() ->
    [ ?assertEqual(Expect, euneus:decode(Input, Opts))
      || {Expect, Input, Opts} <- [
        {{ok, {0,0,0}}, <<"\"1970-01-01T00:00:00.000Z\"">>, #{plugins => [?MODULE]}}
    ]].

-endif.
