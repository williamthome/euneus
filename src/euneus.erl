%%%---------------------------------------------------------------------
%%% @copyright 2023-2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Core module.
%%%
%%% Copyright 2023-2024 William Fank Thomé
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
-module(euneus).

-export([ encode/1
        , encode/2
        , encode_to_iodata/1
        , encode_to_iodata/2
        , parse_encode_opts/1
        , decode/1
        , decode/2
        , parse_decode_opts/1
        , minify/1
        , minify_to_iodata/1
        , prettify/1
        , prettify_to_iodata/1
        , format/2
        , format_to_iodata/2
        , parse_format_opts/1
        ]).

%%%=====================================================================
%%% API functions
%%%=====================================================================

%%
%% Encode
%%

encode(Term) ->
    iolist_to_binary(euneus_encoder:encode(Term)).

encode(Term, Opts) ->
    iolist_to_binary(euneus_encoder:encode(Term, Opts)).

encode_to_iodata(Term) ->
    euneus_encoder:encode(Term).

encode_to_iodata(Term, Opts) ->
    euneus_encoder:encode(Term, Opts).

parse_encode_opts(Opts) ->
    euneus_encoder:parse_opts(Opts).

%%
%% Decode
%%

decode(IoData) ->
    euneus_decoder:decode(IoData).

decode(IoData, Opts) ->
    euneus_decoder:decode(IoData, Opts).

parse_decode_opts(Opts) ->
    euneus_decoder:parse_opts(Opts).

%%
%% Minify
%%

minify(JSON) ->
    iolist_to_binary(euneus_formatter:minify(JSON)).

minify_to_iodata(JSON) ->
    euneus_formatter:minify(JSON).

%%
%% Prettify
%%

prettify(JSON) ->
    iolist_to_binary(euneus_formatter:prettify(JSON)).

prettify_to_iodata(JSON) ->
    euneus_formatter:prettify(JSON).

%%
%% Format
%%

format(JSON, Opts) ->
    iolist_to_binary(euneus_formatter:format(JSON, Opts)).

format_to_iodata(JSON, Opts) ->
    euneus_formatter:format(JSON, Opts).

parse_format_opts(Opts) ->
    euneus_formatter:parse_opts(Opts).
