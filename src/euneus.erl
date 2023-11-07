%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Core module.

%% Copyright 2023 William Fank Thomé
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(euneus).

%% API functions
-export([ encode/1 ]).
-export([ encode/2 ]).
-export([ encode_to_binary/1 ]).
-export([ encode_to_binary/2 ]).
-export([ decode/1 ]).
-export([ decode/2 ]).

%%%=====================================================================
%%% API functions
%%%=====================================================================

-spec encode(Input) -> Return when
    Input :: euneus_encoder:input(),
    Return :: euneus_encoder:result().

encode(Input) ->
    euneus_smart_json_encoder:encode(Input).

-spec encode(Input, Opts) -> Return when
    Input :: euneus_encoder:input(),
    Opts :: euneus_encoder:options(),
    Return :: euneus_encoder:result().

encode(Input, Opts) ->
    euneus_encoder:encode(Input, Opts).

-spec encode_to_binary(Input) -> Return when
    Input :: euneus_encoder:input(),
    Return :: {ok, binary()} | {error, euneus_encoder:error_reason()}.

encode_to_binary(Input) ->
    case encode(Input) of
        {ok, Result} ->
            {ok, iolist_to_binary(Result)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec encode_to_binary(Input, Opts) -> Return when
    Input :: euneus_encoder:input(),
    Opts :: euneus_encoder:options(),
    Return :: {ok, binary()} | {error, euneus_encoder:error_reason()}.

encode_to_binary(Input, Opts) ->
    case encode(Input, Opts) of
        {ok, Result} ->
            {ok, iolist_to_binary(Result)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec decode(Input) -> Result when
    Input :: euneus_decoder:input(),
    Result :: euneus_decoder:result().

decode(Input) ->
    euneus_smart_decoder:decode(Input).

-spec decode(Input, Opts) -> Result when
    Input :: euneus_decoder:input(),
    Opts :: euneus_decoder:options(),
    Result :: euneus_decoder:result().

decode(Input, Opts) ->
    euneus_decoder:decode(Input, Opts).
