%%%---------------------------------------------------------------------
%%% @copyright 2023 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Core module.
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
-export([ parse_decode_opts/1 ]).
-export([ decode_parsed/2 ]).

%% Types

-export_type([ encode_input/0 ]).
-export_type([ encode_opts/0 ]).
-export_type([ encode_result/0 ]).
-export_type([ encode_to_bin_result/0 ]).
-export_type([ decode_input/0 ]).
-export_type([ decode_opts/0 ]).
-export_type([ decode_result/0 ]).

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

%%%---------------------------------------------------------------------
%%% Encode
%%%---------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @doc Generates a JSON from Erlang term.
%%
%% @equiv euneus_encoder:encode/2
%%
%% @see euneus_encoder:encode/2
%%
%% @end
%%----------------------------------------------------------------------
-spec encode(Input, Opts) -> Return when
    Input :: encode_input(),
    Opts :: map() | encode_opts(),
    Return :: encode_result().

encode(Input, Opts) ->
    euneus_encoder:encode(Input, Opts).

%%----------------------------------------------------------------------
%% @doc Generates a JSON as {@link erlang:binary()} from Erlang term.
%%
%% @returns {@link erlang:binary()}.
%%
%% @see euneus_encoder:encode/2
%%
%% @end
%%----------------------------------------------------------------------
-spec encode_to_binary(Input, Opts) -> Return when
    Input :: encode_input(),
    Opts :: map() | encode_opts(),
    Return :: encode_to_bin_result().

encode_to_binary(Input, Opts) ->
    do_encode_to_bin(encode(Input, Opts)).

%%----------------------------------------------------------------------
%% @doc Parses {@link erlang:map()} to {@link euneus_encoder:options()}.
%%
%% @equiv euneus_encoder:parse_opts/1
%%
%% @see euneus_encoder:parse_opts/1
%% @see euneus_encoder:encode_parsed/2
%%
%% @end
%%----------------------------------------------------------------------
-spec parse_encode_opts(Opts) -> Result when
    Opts :: map(),
    Result :: encode_opts().

parse_encode_opts(Opts) ->
    euneus_encoder:parse_opts(Opts).

%%----------------------------------------------------------------------
%% @doc Generates a JSON from Erlang term.
%%
%% @equiv euneus_encoder:encode_parsed/2
%%
%% @see euneus_encoder:encode_parsed/2
%%
%% @end
%%----------------------------------------------------------------------
-spec encode_parsed(Input, ParsedOpts) -> Result when
    Input :: encode_input(),
    ParsedOpts :: encode_opts(),
    Result :: encode_result().

encode_parsed(Input, ParsedOpts) ->
    euneus_encoder:encode_parsed(Input, ParsedOpts).

%%----------------------------------------------------------------------
%% @doc Generates a JSON as {@link erlang:binary()} from Erlang term.
%%
%% @returns {@link erlang:binary()}.
%%
%% @see euneus_encoder:encode_parsed/2
%%
%% @end
%%----------------------------------------------------------------------
-spec encode_parsed_to_binary(Input, ParsedOpts) -> Result when
    Input :: encode_input(),
    ParsedOpts :: encode_opts(),
    Result :: encode_to_bin_result().

encode_parsed_to_binary(Input, ParsedOpts) ->
    do_encode_to_bin(encode_parsed(Input, ParsedOpts)).

%%%---------------------------------------------------------------------
%%% Encode JSON
%%%---------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @doc Generates a JSON from Erlang term.
%%
%% Uses JSON encoder.
%%
%% @equiv euneus_smart_json_encoder:encode/1
%%
%% @see euneus_smart_json_encoder:encode/1
%%
%% @end
%%----------------------------------------------------------------------
-spec encode(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_result().

encode(Input) ->
    euneus_smart_json_encoder:encode(Input).

%%----------------------------------------------------------------------
%% @doc Generates a JSON as {@link erlang:binary()} from Erlang term.
%%
%% Uses JSON encoder.
%%
%% @returns {@link erlang:binary()}.
%%
%% @see euneus_smart_json_encoder:encode/1
%%
%% @end
%%----------------------------------------------------------------------
-spec encode_to_binary(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_to_bin_result().

encode_to_binary(Input) ->
    do_encode_to_bin(encode(Input)).

%%%---------------------------------------------------------------------
%%% Encode JS
%%%---------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @doc Generates a JSON from Erlang term.
%%
%% Uses Javascript encoder.
%%
%% @equiv euneus_smart_js_encoder:encode/1
%%
%% @see euneus_smart_js_encoder:encode/1
%%
%% @end
%%----------------------------------------------------------------------
-spec encode_js(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_result().

encode_js(Input) ->
    euneus_smart_js_encoder:encode(Input).

%%----------------------------------------------------------------------
%% @doc Generates a JSON as {@link erlang:binary()} from Erlang term.
%%
%% Uses Javascript encoder.
%%
%% @returns {@link erlang:binary()}.
%%
%% @see euneus_smart_js_encoder:encode/1
%%
%% @end
%%----------------------------------------------------------------------
-spec encode_js_to_binary(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_to_bin_result().

encode_js_to_binary(Input) ->
    do_encode_to_bin(encode_js(Input)).

%%%---------------------------------------------------------------------
%%% Encode HTML
%%%---------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @doc Generates a JSON from Erlang term.
%%
%% Uses HTML encoder.
%%
%% @equiv euneus_smart_html_encoder:encode/1
%%
%% @see euneus_smart_html_encoder:encode/1
%%
%% @end
%%----------------------------------------------------------------------
-spec encode_html(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_result().

encode_html(Input) ->
    euneus_smart_html_encoder:encode(Input).

%%----------------------------------------------------------------------
%% @doc Generates a JSON as {@link erlang:binary()} from Erlang term.
%%
%% Uses HTML encoder.
%%
%% @returns {@link erlang:binary()}.
%%
%% @see euneus_smart_html_encoder:encode/1
%%
%% @end
%%----------------------------------------------------------------------
-spec encode_html_to_binary(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_to_bin_result().

encode_html_to_binary(Input) ->
    do_encode_to_bin(encode_html(Input)).

%%%---------------------------------------------------------------------
%%% Encode Unicode
%%%---------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @doc Generates a JSON from Erlang term.
%%
%% Uses Unicode encoder.
%%
%% @equiv euneus_smart_unicode_encoder:encode/1
%%
%% @see euneus_smart_unicode_encoder:encode/1
%%
%% @end
%%----------------------------------------------------------------------
-spec encode_unicode(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_result().

encode_unicode(Input) ->
    euneus_smart_unicode_encoder:encode(Input).

%%----------------------------------------------------------------------
%% @doc Generates a JSON as {@link erlang:binary()} from Erlang term.
%%
%% Uses Unicode encoder.
%%
%% @returns {@link erlang:binary()}.
%%
%% @see euneus_smart_unicode_encoder:encode/1
%%
%% @end
%%----------------------------------------------------------------------
-spec encode_unicode_to_binary(Input) -> Return when
    Input :: encode_input(),
    Return :: encode_to_bin_result().

encode_unicode_to_binary(Input) ->
    do_encode_to_bin(encode_unicode(Input)).

%%%---------------------------------------------------------------------
%%% Decode
%%%---------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @doc Parses JSON to Erlang term.
%%
%% @equiv euneus_smart_decoder:decode/1
%%
%% @see euneus_smart_decoder:decode/1
%%
%% @end
%%----------------------------------------------------------------------
-spec decode(Input) -> Result when
    Input :: decode_input(),
    Result :: decode_result().

decode(Input) ->
    euneus_smart_decoder:decode(Input).

%%----------------------------------------------------------------------
%% @doc Parses JSON to Erlang term.
%%
%% @equiv euneus_decoder:decode/2
%%
%% @see euneus_decoder:decode/2
%%
%% @end
%%----------------------------------------------------------------------
-spec decode(Input, Opts) -> Result when
    Input :: decode_input(),
    Opts :: map() | decode_opts(),
    Result :: decode_result().

decode(Input, Opts) ->
    euneus_decoder:decode(Input, Opts).

%%----------------------------------------------------------------------
%% @doc Parses {@link erlang:map()} to {@link euneus_decoder:options()}.
%%
%% @equiv euneus_decoder:parse_opts/1
%%
%% @see euneus_decoder:parse_opts/1
%% @see euneus_decoder:decode_parsed/2
%%
%% @end
%%----------------------------------------------------------------------
-spec parse_decode_opts(Opts) -> Result when
    Opts :: map(),
    Result :: decode_opts().

parse_decode_opts(Opts) ->
    euneus_decoder:parse_opts(Opts).

%%----------------------------------------------------------------------
%% @doc Parses JSON to Erlang term.
%%
%% @equiv euneus_decoder:decode_parsed/2
%%
%% @see euneus_decoder:decode_parsed/2
%%
%% @end
%%----------------------------------------------------------------------
-spec decode_parsed(Input, ParsedOpts) -> Result when
    Input :: decode_input(),
    ParsedOpts :: decode_opts(),
    Result :: decode_result().

decode_parsed(Input, ParsedOpts) ->
    euneus_decoder:decode_parsed(Input, ParsedOpts).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

do_encode_to_bin({ok, Result}) ->
    {ok, iolist_to_binary(Result)};
do_encode_to_bin({error, Reason}) ->
    {error, Reason}.
