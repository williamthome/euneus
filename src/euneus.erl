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
-export([ parse_encode_opts/1 ]).
-export([ encode_parsed/2 ]).
-export([ encode_parsed_to_binary/2 ]).
-export([ encode_js/1 ]).
-export([ encode_js_to_binary/1 ]).
-export([ encode_html/1 ]).
-export([ encode_html_to_binary/1 ]).
-export([ encode_unicode/1 ]).
-export([ encode_unicode_to_binary/1 ]).
-export([ decode/1 ]).
-export([ decode/2 ]).

%% Types

-type encode_input() :: euneus_encoder:input().
-type encode_opts() :: euneus_encoder:options().
-type encode_result() :: euneus_encoder:result().
-type encode_to_bin_result() :: {ok, binary()}
                              | {error, euneus_encoder:error_reason()}.

-type decode_input() :: euneus_decoder:input().
-type decode_opts() :: euneus_decoder:options().
-type decode_result() :: euneus_decoder:result().

%%%=====================================================================
%%% API functions
%%%=====================================================================

%%----------------------------------------------------------------------
%% Encode
%%----------------------------------------------------------------------

%% encode/2

-spec encode(Input, Opts) -> Return when
    Input :: encode_input(),
    Opts :: map() | encode_opts(),
    Return :: encode_result().

encode(Input, Opts) ->
    euneus_encoder:encode(Input, Opts).

%% encode_to_binary/2

-spec encode_to_binary(Input, Opts) -> Return when
    Input :: encode_input(),
    Opts :: map() | encode_opts(),
    Return :: encode_to_bin_result().

encode_to_binary(Input, Opts) ->
    do_encode_to_bin(encode(Input, Opts)).

%% parse_encode_opts/1

-spec parse_encode_opts(Opts) -> Result when
    Opts :: map(),
    Result :: encode_opts().

parse_encode_opts(Opts) ->
    euneus_encoder:parse_opts(Opts).

%% encode_parsed/2

-spec encode_parsed(Input, ParsedOpts) -> Result when
    Input :: encode_input(),
    ParsedOpts :: encode_opts(),
    Result :: encode_result().

encode_parsed(Input, ParsedOpts) ->
    euneus_encoder:encode_parsed(Input, ParsedOpts).

%% encode_parsed_to_binary/2

-spec encode_parsed_to_binary(Input, ParsedOpts) -> Result when
    Input :: encode_input(),
    ParsedOpts :: encode_opts(),
    Result :: encode_to_bin_result().

encode_parsed_to_binary(Input, ParsedOpts) ->
    do_encode_to_bin(encode_parsed(Input, ParsedOpts)).

%%----------------------------------------------------------------------
%% Encode JSON
%%----------------------------------------------------------------------

-spec encode(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_result().

encode(Input) ->
    euneus_smart_json_encoder:encode(Input).

-spec encode_to_binary(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_to_bin_result().

encode_to_binary(Input) ->
    do_encode_to_bin(encode(Input)).

%%----------------------------------------------------------------------
%% Encode JS
%%----------------------------------------------------------------------

-spec encode_js(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_result().

encode_js(Input) ->
    euneus_smart_js_encoder:encode(Input).

-spec encode_js_to_binary(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_to_bin_result().

encode_js_to_binary(Input) ->
    do_encode_to_bin(encode_js(Input)).

%%----------------------------------------------------------------------
%% Encode HTML
%%----------------------------------------------------------------------

-spec encode_html(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_result().

encode_html(Input) ->
    euneus_smart_html_encoder:encode(Input).

-spec encode_html_to_binary(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_to_bin_result().

encode_html_to_binary(Input) ->
    do_encode_to_bin(encode_html(Input)).

%%----------------------------------------------------------------------
%% Encode Unicode
%%----------------------------------------------------------------------

-spec encode_unicode(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_result().

encode_unicode(Input) ->
    euneus_smart_unicode_encoder:encode(Input).

-spec encode_unicode_to_binary(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_to_bin_result().

encode_unicode_to_binary(Input) ->
    do_encode_to_bin(encode_unicode(Input)).

%%----------------------------------------------------------------------
%% Decode
%%----------------------------------------------------------------------

-spec decode(Input) -> Result when
    Input :: decode_input(),
    Result :: decode_result().

decode(Input) ->
    euneus_smart_decoder:decode(Input).

-spec decode(Input, Opts) -> Result when
    Input :: decode_input(),
    Opts :: decode_opts(),
    Result :: decode_result().

decode(Input, Opts) ->
    euneus_decoder:decode(Input, Opts).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

do_encode_to_bin({ok, Result}) ->
    {ok, iolist_to_binary(Result)};
do_encode_to_bin({error, Reason}) ->
    {error, Reason}.
