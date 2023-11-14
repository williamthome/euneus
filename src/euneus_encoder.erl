%%%---------------------------------------------------------------------
%%% @copyright 2023 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc JSON generator.
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
-module(euneus_encoder).

-compile({ inline, encode_binary/2 }).
-compile({ inline, encode_atom/2 }).
-compile({ inline, encode_integer/2 }).
-compile({ inline, encode_float/2 }).
-compile({ inline, encode_list/2 }).
-compile({ inline, encode_map/2 }).
-compile({ inline, encode_datetime/2 }).
-compile({ inline, encode_timestamp/2 }).
-compile({ inline, encode_unhandled/2 }).
-compile({ inline, escape_json/4 }).
-compile({ inline, escape_json_chunk/5 }).
-compile({ inline, escape_html/4 }).
-compile({ inline, escape_html_chunk/5 }).
-compile({ inline, escape_js/4 }).
-compile({ inline, escape_js_chunk/5 }).
-compile({ inline, escape_unicode/4 }).
-compile({ inline, escape_unicode_chunk/5 }).
-compile({ inline, handle_error/3 }).
-compile({ inline, maps_get/3 }).
-compile({ inline, maps_to_list/1 }).

% By default, encode_unhandled/2 will raise unsupported_type exception,
% so it is a function without a local return. Note that parse_opts/1
% is included because unhandled_encoder option has no local return.
-dialyzer({ no_return, parse_opts/1 }).
-dialyzer({ no_return, encode_unhandled/2 }).
-dialyzer( no_improper_lists ).

%% API functions

-export([ encode/2 ]).
-export([ parse_opts/1 ]).
-export([ encode_parsed/2 ]).
-export([ encode_binary/2 ]).
-export([ encode_atom/2 ]).
-export([ encode_integer/2 ]).
-export([ encode_float/2 ]).
-export([ encode_list/2 ]).
-export([ encode_map/2 ]).
-export([ encode_datetime/2 ]).
-export([ encode_timestamp/2 ]).
-export([ encode_unhandled/2 ]).
-export([ escape/2 ]).
-export([ escape_byte/1 ]).
-export([ escape_json/1 ]).
-export([ escape_html/1 ]).
-export([ escape_js/1 ]).
-export([ escape_unicode/1 ]).
-export([ throw_unsupported_type_error/1 ]).
-export([ handle_error/3 ]).

%% Types

-export_type([ input/0 ]).
-export_type([ options/0 ]).
-export_type([ result/0 ]).
-export_type([ encoder/1 ]).
-export_type([ escaper/1 ]).
-export_type([ error_handler/0 ]).
-export_type([ error_reason/0 ]).

-type input() :: term().
-type options() :: #{ nulls => list()
                    , binary_encoder => encoder(binary())
                    , atom_encoder => encoder(atom())
                    , integer_encoder => encoder(integer())
                    , float_encoder => encoder(float())
                    , list_encoder => encoder(list())
                    , map_encoder => encoder(map())
                    , datetime_encoder => encoder(calendar:datetime())
                    , timestamp_encoder => encoder(erlang:timestamp())
                    , unhandled_encoder => encoder(term())
                    , escaper => json
                               | html
                               | javascript
                               | unicode
                               | escaper(binary())
                    , error_handler => error_handler()
                    }.
-type result() :: {ok, iolist()} | {error, error_reason()}.
-type encoder(Input) :: fun((Input, options()) -> iolist()).
-type escaper(Input) :: fun((Input, options()) -> iolist()).
-type error_class() :: error | exit | throw.
-type unsupported_type_error() :: {unsupported_type, Unsupported :: term()}.
-type invalid_byte_error() :: {invalid_byte, Byte :: byte(), Input :: binary()}.
-type error_reason() :: unsupported_type_error() | invalid_byte_error().
-type error_stacktrace() :: erlang:stacktrace().
-type error_handler() :: fun(( error_class()
                             , error_reason()
                             , error_stacktrace() ) -> error_stacktrace()).

%% Macros

-define(min(X, Min), is_integer(X) andalso X >= Min).
-define(range(X, Min, Max), is_integer(X) andalso X >= Min andalso X =< Max).

-define(NON_PRINTABLE_LAST, 31).
-define(ONE_BYTE_LAST, 127).
-define(TWO_BYTE_LAST, 2_047).
-define(THREE_BYTE_LAST, 65_535).

%%%=====================================================================
%%% API functions
%%%=====================================================================

%%----------------------------------------------------------------------
%% @doc Generates a JSON from Erlang term.
%%
%% @param Term :: {@link euneus_encoder:input()}.
%% @param Opts :: {@link erlang:map()}.
%%
%% @returns {@link euneus_encoder:result()}.
%%
%% @end
%%----------------------------------------------------------------------
-spec encode(input(), map()) -> result().

encode(Term, Opts) ->
    encode_parsed(Term, parse_opts(Opts)).

%%----------------------------------------------------------------------
%% @doc Parses {@link erlang:map()} to {@link euneus_encoder:options()}.
%%
%% The parsed map can be expanded in compile time or stored to be
%% reused, avoiding parsing the options in every encoding.
%%
%% @end
%%
%% NOTE: The explicit call of functions wrapped in another function is
%% required for the inline optimization.
%%
%% @see euneus_encoder:parse_opts/1
%% @see euneus_encoder:encode_parsed/2
%%
%% @end
%%----------------------------------------------------------------------
-spec parse_opts(map()) -> options().

parse_opts(Opts) ->
    #{
        nulls => maps_get(nulls, Opts, [undefined]),
        binary_encoder => maps_get(binary_encoder, Opts, fun (X, O) ->
            escape(X, O)
        end),
        atom_encoder => maps_get(atom_encoder, Opts, fun (X, O) ->
            encode_atom(X, O)
        end),
        integer_encoder => maps_get(integer_encoder, Opts, fun (X, O) ->
            encode_integer(X, O)
        end),
        float_encoder => maps_get(float_encoder, Opts, fun (X, O) ->
            encode_float(X, O)
        end),
        list_encoder => maps_get(list_encoder, Opts, fun (X, O) ->
            encode_list(X, O)
        end),
        map_encoder => maps_get(map_encoder, Opts, fun (X, O) ->
            encode_map(X, O)
        end),
        datetime_encoder => maps_get(datetime_encoder, Opts, fun (X, O) ->
            encode_datetime(X, O)
        end),
        timestamp_encoder => maps_get(timestamp_encoder, Opts, fun (X, O) ->
            encode_timestamp(X, O)
        end),
        unhandled_encoder => maps_get(unhandled_encoder, Opts, fun (X, O) ->
            encode_unhandled(X, O)
        end),
        escaper =>
            case maps_get(escaper, Opts, json) of
                json ->
                    fun(X, _O) -> [$", escape_json(X, [], X, 0), $"] end;
                html ->
                    fun(X, _O) -> [$", escape_html(X, [], X, 0), $"] end;
                javascript ->
                    fun(X, _O) -> [$", escape_js(X, [], X, 0), $"] end;
                unicode ->
                    fun(X, _O) -> [$", escape_unicode(X, [], X, 0), $"] end;
                Fun when is_function(Fun, 2) ->
                    Fun
            end,
        error_handler => maps_get(error_handler, Opts, fun(C, R, S) ->
            handle_error(C, R, S)
        end)
    }.

%%----------------------------------------------------------------------
%% @doc Generates a JSON from Erlang term.
%%
%% @param Term :: {@link euneus_encoder:input()}.
%% @param Opts :: {@link euneus_encoder:options()}.
%%
%% @returns {@link euneus_encoder:result()}.
%%
%% @see euneus_encoder:parse_opts/1
%%
%% @end
%%----------------------------------------------------------------------
-spec encode_parsed(input(), options()) -> result().

encode_parsed(Term, Opts) ->
    try
        {ok, value(Term, Opts)}
    catch
        Class:Reason:Stacktrace ->
            Handle = maps:get(error_handler, Opts),
            Handle(Class, Reason, Stacktrace)
    end.

encode_binary(Bin, Opts) ->
    escape(Bin, Opts).

encode_atom(true, _Opts) ->
    <<"true">>;
encode_atom(false, _Opts) ->
    <<"false">>;
encode_atom(Atom, #{nulls := Nulls} = Opts) ->
    case lists:member(Atom, Nulls) of
        true ->
            <<"null">>;
        false ->
            escape(atom_to_binary(Atom, utf8), Opts)
    end.

encode_integer(Int, _Opts) ->
    integer_to_binary(Int).

encode_float(Float, _Opts) ->
    float_to_binary(Float, [short]).

encode_list([H | T], Opts) ->
    [$[, value(H, Opts), do_encode_list_loop(T, Opts)];
encode_list([], _Opts) ->
    <<"[]">>.

do_encode_list_loop([], _Opts) ->
    [$]];
do_encode_list_loop([H | T], Opts) ->
    [$,, value(H, Opts) | do_encode_list_loop(T, Opts)].

encode_map(Map, Opts) ->
    do_encode_map(maps_to_list(Map), Opts).

do_encode_map([{K, V} | T], Opts) ->
    [${, key(K, Opts), $:, value(V, Opts) | do_encode_map_loop(T, Opts)];
do_encode_map([], _) ->
    <<"{}">>.

do_encode_map_loop([], _Opts) ->
    [$}];
do_encode_map_loop([{K, V} | T], Opts) ->
    [$,, key(K, Opts), $:, value(V, Opts) | do_encode_map_loop(T, Opts)].

encode_datetime({{YYYY,MM,DD},{H,M,S}}, Opts) ->
    DateTime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [YYYY,MM,DD,H,M,S])
    ),
    escape(DateTime, Opts).

encode_timestamp({_,_,MicroSecs} = Timestamp, Opts) ->
    MilliSecs = MicroSecs div 1000,
    {{YYYY,MM,DD},{H,M,S}} = calendar:now_to_datetime(Timestamp),
    DateTime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
        [YYYY,MM,DD,H,M,S,MilliSecs])
    ),
    escape(DateTime, Opts).

encode_unhandled(Term, _Opts) ->
    throw_unsupported_type_error(Term).

escape(Bin, #{escaper := Escape} = Opts) ->
    Escape(Bin, Opts).

escape_json(Bin) ->
    escape_json(Bin, [], Bin, 0).

escape_json(Data, Acc, Input, Pos) ->
    case Data of
        <<$"/integer, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\\"">>],
            escape_json(Rest, Acc1, Input, Pos+1);
        <<$\\/integer, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\\\">>],
            escape_json(Rest, Acc1, Input, Pos+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?NON_PRINTABLE_LAST ->
            Acc1 = [Acc | escape_byte(Byte)],
            escape_json(Rest, Acc1, Input, Pos+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?ONE_BYTE_LAST ->
            escape_json_chunk(Rest, Acc, Input, Pos, 1);
        <<Char/utf8, Rest/bitstring>> when Char =< ?TWO_BYTE_LAST ->
            escape_json_chunk(Rest, Acc, Input, Pos, 2);
        <<Char/utf8, Rest/bitstring>> when Char =< ?THREE_BYTE_LAST ->
            escape_json_chunk(Rest, Acc, Input, Pos, 3);
        <<_Char/utf8, Rest/bitstring>> ->
            escape_json_chunk(Rest, Acc, Input, Pos, 4);
        <<>> ->
            Acc;
        <<Byte/integer, _Rest/bitstring>> ->
            throw_invalid_byte_error(Byte, Input)
    end.

escape_json_chunk(Data, Acc, Input, Pos, Len) ->
    case Data of
        <<$"/integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\\"">>]],
            escape_json(Rest, Acc1, Input, Pos+Len+1);
        <<$\\/integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\\\">>]],
            escape_json(Rest, Acc1, Input, Pos+Len+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?NON_PRINTABLE_LAST ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, escape_byte(Byte)]],
            escape_json(Rest, Acc1, Input, Pos+Len+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?ONE_BYTE_LAST ->
            escape_json_chunk(Rest, Acc, Input, Pos, Len+1);
        <<>> ->
            case Acc =:= [] of
                true ->
                    binary_part(Input, Pos, Len);
                false ->
                    [Acc, binary_part(Input, Pos, Len)]
            end;
        <<Char/utf8, Rest/bitstring>> when Char =< ?TWO_BYTE_LAST ->
            escape_json_chunk(Rest, Acc, Input, Pos, Len+2);
        <<Char/utf8, Rest/bitstring>> when Char =< ?THREE_BYTE_LAST ->
            escape_json_chunk(Rest, Acc, Input, Pos, Len+3);
        <<_Char/utf8, Rest/bitstring>> ->
            escape_json_chunk(Rest, Acc, Input, Pos, Len+4);
        <<Byte/integer, _Rest/bitstring>> ->
            throw_invalid_byte_error(Byte, Input)
    end.

escape_html(Bin) ->
    escape_html(Bin, [], Bin, 0).

escape_html(Data, Acc, Input, Pos) ->
    case Data of
        <<$"/integer, Rest/bitstring>> ->
          Acc1 = [Acc, <<"\\\"">>],
          escape_html(Rest, Acc1, Input, Pos+1);
        <<$\\/integer, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\\\">>],
            escape_html(Rest, Acc1, Input, Pos+1);
        <<$//integer, Rest/bitstring>> ->
          Acc1 = [Acc | <<"\\/">>],
          escape_html(Rest, Acc1, Input, Pos+1);
        <<Byte/integer, Rest/bitstring>> when Byte < 33 ->
            Acc1 = [Acc, escape_byte(Byte)],
            escape_html(Rest, Acc1, Input, Pos+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?ONE_BYTE_LAST ->
            escape_html_chunk(Rest, Acc, Input, Pos, 1);
        <<Char/utf8, Rest/bitstring>> when Char =< ?TWO_BYTE_LAST ->
            escape_html_chunk(Rest, Acc, Input, Pos, 2);
        <<8232/utf8, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\u2028">>],
            escape_html(Rest, Acc1, Input, Pos+3);
        <<8233/utf8, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\u2029">>],
            escape_html(Rest, Acc1, Input, Pos+3);
        <<Char/utf8, Rest/bitstring>> when Char =< ?THREE_BYTE_LAST ->
            escape_html_chunk(Rest, Acc, Input, Pos, 3);
        <<_Char/utf8, Rest/bitstring>> ->
            escape_html_chunk(Rest, Acc, Input, Pos, 4);
        <<>> ->
            Acc;
        <<Byte/integer, _Rest/bitstring>> ->
            throw_invalid_byte_error(Byte, Input)
    end.

escape_html_chunk(Data, Acc, Input, Pos, Len) ->
    case Data of
        <<$"/integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc2 = [Acc | [Part, <<"\\\"">>]],
            escape_html(Rest, Acc2, Input, Pos+Len+1);
        <<$\\/integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc2 = [Acc | [Part, <<"\\\\">>]],
            escape_html(Rest, Acc2, Input, Pos+Len+1);
        <<$//integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc2 = [Acc | [Part, <<"\\/">>]],
            escape_html(Rest, Acc2, Input, Pos+Len+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?NON_PRINTABLE_LAST ->
            Part = binary_part(Input, Pos, Len),
            Acc2 = [Acc, Part, escape_byte(Byte)],
            escape_html(Rest, Acc2, Input, Pos+Len+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?ONE_BYTE_LAST ->
            escape_html_chunk(Rest, Acc, Input, Pos, Len+1);
        <<>> ->
            case Acc =:= [] of
                true ->
                    binary_part(Input, Pos, Len);
                false ->
                    [Acc, binary_part(Input, Pos, Len)]
            end;
        <<Char/utf8, Rest/bitstring>> when Char =< ?TWO_BYTE_LAST ->
            escape_html_chunk(Rest, Acc, Input, Pos, Len+2);
        <<8232/utf8, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc2 = [Acc | [Part, <<"\\u2028">>]],
            escape_html(Rest, Acc2, Input, Pos+Len+3);
        <<8233/utf8, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc2 = [Acc | [Part, <<"\\u2029">>]],
            escape_html(Rest, Acc2, Input, Pos+Len+3);
        <<Char/utf8, Rest/bitstring>> when Char =< ?THREE_BYTE_LAST ->
            escape_html_chunk(Rest, Acc, Input, Pos, Len+3);
        <<_Char/utf8, Rest/bitstring>> ->
            escape_html_chunk(Rest, Acc, Input, Pos, Len+4);
        <<Byte/integer, _Rest/bitstring>> ->
            throw_invalid_byte_error(Byte, Input)
    end.

escape_js(Bin) ->
    escape_js(Bin, [], Bin, 0).

escape_js(Data, Acc, Input, Pos) ->
    case Data of
        <<$"/integer, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\\"">>],
            escape_js(Rest, Acc1, Input, Pos+1);
        <<$\\/integer, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\\\">>],
            escape_js(Rest, Acc1, Input, Pos+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?NON_PRINTABLE_LAST ->
            Acc1 = [Acc | escape_byte(Byte)],
            escape_js(Rest, Acc1, Input, Pos+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?ONE_BYTE_LAST ->
            escape_js_chunk(Rest, Acc, Input, Pos, 1);
        <<Char/utf8, Rest/bitstring>> when Char =< ?TWO_BYTE_LAST ->
            escape_js_chunk(Rest, Acc, Input, Pos, 2);
        <<8232/utf8, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\u2028">>],
            escape_js(Rest, Acc1, Input, Pos+3);
        <<8233/utf8, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\u2029">>],
            escape_js(Rest, Acc1, Input, Pos+3);
        <<Char/utf8, Rest/bitstring>> when Char =< ?THREE_BYTE_LAST ->
            escape_js_chunk(Rest, Acc, Input, Pos, 3);
        <<_Char/utf8, Rest/bitstring>> ->
            escape_js_chunk(Rest, Acc, Input, Pos, 4);
        <<>> ->
            Acc;
        <<Byte/integer, _Rest/bitstring>> ->
            throw_invalid_byte_error(Byte, Input)
    end.

escape_js_chunk(Data, Acc, Input, Pos, Len) ->
    case Data of
        <<$"/integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\\"">>]],
            escape_js(Rest, Acc1, Input, Pos+Len+1);
        <<$\\/integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\\\">>]],
            escape_js(Rest, Acc1, Input, Pos+Len+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?NON_PRINTABLE_LAST ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, escape_byte(Byte)]],
            escape_js(Rest, Acc1, Input, Pos+Len+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?ONE_BYTE_LAST ->
            escape_js_chunk(Rest, Acc, Input, Pos, Len+1);
        <<>> ->
            case Acc =:= [] of
                true ->
                    binary_part(Input, Pos, Len);
                false ->
                    [Acc, binary_part(Input, Pos, Len)]
            end;
        <<Char/utf8, Rest/bitstring>> when Char =< ?TWO_BYTE_LAST ->
            escape_js_chunk(Rest, Acc, Input, Pos, Len+2);
        <<8232/utf8, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\u2028">>]],
            escape_js(Rest, Acc1, Input, Pos+Len+3);
        <<8233/utf8, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\u2029">>]],
            escape_js(Rest, Acc1, Input, Pos+Len+3);
        <<Char/utf8, Rest/bitstring>> when Char =< ?THREE_BYTE_LAST ->
            escape_js_chunk(Rest, Acc, Input, Pos, Len+3);
        <<_Char/utf8, Rest/bitstring>> ->
            escape_js_chunk(Rest, Acc, Input, Pos, Len+4);
        <<Byte/integer, _Rest/bitstring>> ->
            throw_invalid_byte_error(Byte, Input)
    end.

escape_unicode(Bin) ->
    escape_unicode(Bin, [], Bin, 0).

escape_unicode(Data, Acc, Input, Pos) ->
    case Data of
        <<$"/integer, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\\"">>],
            escape_unicode(Rest, Acc1, Input, Pos+1);
        <<$\\/integer, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\\\">>],
            escape_unicode(Rest, Acc1, Input, Pos+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?NON_PRINTABLE_LAST ->
            Acc1 = [Acc | escape_byte(Byte)],
            escape_unicode(Rest, Acc1, Input, Pos+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?ONE_BYTE_LAST ->
            escape_unicode_chunk(Rest, Acc, Input, Pos, 1);
        <<Char/utf8, Rest/bitstring>> when Char < 256 ->
            Acc1 = [Acc | [<<"\\u00">>, integer_to_binary(Char, 16)]],
            escape_unicode(Rest, Acc1, Input, Pos+2);
        <<Char/utf8, Rest/bitstring>> when Char =< ?TWO_BYTE_LAST ->
            Acc1 = [Acc | [<<"\\u0">>, integer_to_binary(Char, 16)]],
            escape_unicode(Rest, Acc1, Input, Pos+2);
        <<Char/utf8, Rest/bitstring>> when Char < 4096 ->
            Acc1 = [Acc | [<<"\\u0">>, integer_to_binary(Char, 16)]],
            escape_unicode(Rest, Acc1, Input, Pos+3);
        <<Char/utf8, Rest/bitstring>> when Char =< ?THREE_BYTE_LAST ->
            Acc1 = [Acc | [<<"\\u">>, integer_to_binary(Char, 16)]],
            escape_unicode(Rest, Acc1, Input, Pos+3);
        <<Char0/utf8, Rest/bitstring>> ->
            Char = Char0 - 65536,
            Acc1 = [ Acc
                   | [ <<"\\uD">>
                     , integer_to_binary(2048 bor (Char bsr 10), 16)
                     , <<"\\uD">>
                     , integer_to_binary(3072 bor Char band 1023, 16) ]
                   ],
            escape_unicode(Rest, Acc1, Input, Pos+4);
        <<>> ->
            Acc;
        <<Byte/integer, _Rest/bitstring>> ->
            throw_invalid_byte_error(Byte, Input)
    end.

escape_unicode_chunk(Data, Acc, Input, Pos, Len) ->
    case Data of
        <<$"/integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\\"">>]],
            escape_unicode(Rest, Acc1, Input, Pos+Len+1);
        <<$\\/integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\\\">>]],
            escape_unicode(Rest, Acc1, Input, Pos+Len+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?NON_PRINTABLE_LAST ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, escape_byte(Byte)]],
            escape_unicode(Rest, Acc1, Input, Pos+Len+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?ONE_BYTE_LAST ->
            escape_unicode_chunk(Rest, Acc, Input, Pos, Len+1);
        <<>> ->
            case Acc =:= [] of
                true ->
                    binary_part(Input, Pos, Len);
                false ->
                    [Acc | binary_part(Input, Pos, Len)]
            end;
        <<Char/utf8, Rest/bitstring>> when Char < 256 ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\u00">>, integer_to_binary(Char, 16)]],
            escape_unicode(Rest, Acc1, Input, Pos+Len+2);
        <<Char/utf8, Rest/bitstring>> when Char =< ?TWO_BYTE_LAST ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\u0">>, integer_to_binary(Char, 16)]],
            escape_unicode(Rest, Acc1, Input, Pos+Len+2);
        <<Char/utf8, Rest/bitstring>> when Char < 4096 ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\u0">>, integer_to_binary(Char, 16)]],
            escape_unicode(Rest, Acc1, Input, Pos+Len+3);
        <<Char/utf8, Rest/bitstring>> when Char =< ?THREE_BYTE_LAST ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\u">>, integer_to_binary(Char, 16)]],
            escape_unicode(Rest, Acc1, Input, Pos+Len+3);
        <<Char0/utf8, Rest/bitstring>> ->
            Char = Char0 - 65536,
            Part = binary_part(Input, Pos, Len),
            Acc1 = [ Acc
                   | [ Part
                     , <<"\\uD">>
                     , integer_to_binary(2048 bor (Char bsr 10), 16)
                     , <<"\\uD">>
                     , integer_to_binary(3072 bor Char band 1023, 16) ]
                   ],
            escape_unicode(Rest, Acc1, Input, Pos+Len+4);
        <<Byte/integer, _Rest/bitstring>> ->
            throw_invalid_byte_error(Byte, Input)
    end.

escape_byte(0) -> <<"\\u0000">>;
escape_byte(1) -> <<"\\u0001">>;
escape_byte(2) -> <<"\\u0002">>;
escape_byte(3) -> <<"\\u0003">>;
escape_byte(4) -> <<"\\u0004">>;
escape_byte(5) -> <<"\\u0005">>;
escape_byte(6) -> <<"\\u0006">>;
escape_byte(7) -> <<"\\u0007">>;
escape_byte($\b) -> <<"\\b">>;
escape_byte($\t) -> <<"\\t">>;
escape_byte($\n) -> <<"\\n">>;
escape_byte($\v) -> <<"\\u000B">>;
escape_byte($\f) -> <<"\\f">>;
escape_byte($\r) -> <<"\\r">>;
escape_byte(14) -> <<"\\u000E">>;
escape_byte(15) -> <<"\\u000F">>;
escape_byte(16) -> <<"\\u0010">>;
escape_byte(17) -> <<"\\u0011">>;
escape_byte(18) -> <<"\\u0012">>;
escape_byte(19) -> <<"\\u0013">>;
escape_byte(20) -> <<"\\u0014">>;
escape_byte(21) -> <<"\\u0015">>;
escape_byte(22) -> <<"\\u0016">>;
escape_byte(23) -> <<"\\u0017">>;
escape_byte(24) -> <<"\\u0018">>;
escape_byte(25) -> <<"\\u0019">>;
escape_byte(26) -> <<"\\u001A">>;
escape_byte($\e) -> <<"\\u001B">>;
escape_byte(28) -> <<"\\u001C">>;
escape_byte(29) -> <<"\\u001D">>;
escape_byte(30) -> <<"\\u001E">>;
escape_byte(31) -> <<"\\u001F">>;
escape_byte($\") -> <<"\\\"">>;
escape_byte($/) -> <<"\\/">>;
escape_byte($\\) -> <<"\\\\">>;
escape_byte(Byte) -> throw_invalid_byte_error(Byte, Byte).

throw_unsupported_type_error(Term) ->
    throw({unsupported_type, Term}).

throw_invalid_byte_error(Byte, Input) ->
    throw({invalid_byte, Byte, Input}).

handle_error(throw, Reason, _Stacktrace) ->
    case Reason of
        {unsupported_type, Unsupported} ->
            {error, {unsupported_type, Unsupported}};
        {invalid_byte, Byte0, Input} ->
            Byte = <<"0x"/utf8, (integer_to_binary(Byte0, 16))/binary>>,
            {error, {invalid_byte, Byte, Input}};
        _ ->
            {error, Reason}
    end;
handle_error(Class, Reason, Stacktrace) ->
    erlang:raise(Class, Reason, Stacktrace).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

key(Atom, #{binary_encoder := Encode} = Opts) when is_atom(Atom) ->
    Encode(atom_to_binary(Atom, utf8), Opts);
key(Bin, #{binary_encoder := Encode} = Opts) when is_binary(Bin) ->
    Encode(Bin, Opts);
key(Int, #{binary_encoder := Encode} = Opts) when is_integer(Int) ->
    Encode(integer_to_binary(Int), Opts);
key(String, #{binary_encoder := Encode} = Opts) when is_list(String) ->
    Encode(list_to_binary(String), Opts).

value(Bin, #{binary_encoder := Encode} = Opts) when is_binary(Bin) ->
    Encode(Bin, Opts);
value(Atom, #{atom_encoder := Encode} = Opts) when is_atom(Atom) ->
    Encode(Atom, Opts);
value(Int, #{integer_encoder := Encode} = Opts) when is_integer(Int) ->
    Encode(Int, Opts);
value(Float, #{float_encoder := Encode} = Opts) when is_float(Float) ->
    Encode(Float, Opts);
value(List, #{list_encoder := Encode} = Opts) when is_list(List) ->
    Encode(List, Opts);
value(Map, #{map_encoder := Encode} = Opts) when is_map(Map) ->
    Encode(Map, Opts);
value({{YYYY,MM,DD},{H,M,S}} = DateTime, #{datetime_encoder := Encode} = Opts)
  when ?min(YYYY, 0), ?range(MM, 1, 12), ?range(DD, 1, 31)
     , ?range(H, 0, 23), ?range(M, 0, 59), ?range(S, 0, 59) ->
    Encode(DateTime, Opts);
value({MegaSecs,Secs,MicroSecs} = Timestamp, #{timestamp_encoder := Encode} = Opts)
  when ?min(MegaSecs, 0), ?min(Secs, 0), ?min(MicroSecs, 0) ->
    Encode(Timestamp, Opts);
value(Term, #{unhandled_encoder := Encode} = Opts) ->
    Encode(Term, Opts).

%%%=====================================================================
%%% Support functions
%%%=====================================================================

maps_get(Key, Map, Default) ->
    case Map of
        #{Key := Value} -> Value;
        #{} -> Default
    end.

maps_to_list(Map) ->
    do_maps_to_list(erts_internal:map_next(0, Map, [])).

do_maps_to_list([Iter, Map | Acc]) when is_integer(Iter) ->
    do_maps_to_list(erts_internal:map_next(Iter, Map, Acc));
do_maps_to_list(Acc) ->
    Acc.

%%%=====================================================================
%%% Eunit tests
%%%=====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    [ ?assertEqual(Expect, euneus:encode_to_binary(Input, Opts))
      || {Expect, Input, Opts} <- [
        {{ok, <<"true">>}, true, #{}},
        {{ok, <<"\"foo\"">>}, foo, #{}},
        {{ok, <<"\"foo\"">>}, <<"foo">>, #{}},
        {{ok, <<"0">>}, 0, #{}},
        {{ok, <<"123.456789">>}, 123.45678900, #{}},
        {{ok, <<"[true,0,null]">>}, [true,0,undefined], #{}},
        {{ok, <<"{\"foo\":\"bar\"}">>}, #{foo => bar}, #{}},
        {{ok, <<"{\"0\":0}">>}, #{0 => 0}, #{}},
        {{ok, <<"\"1970-01-01T00:00:00Z\"">>}, {{1970,1,1},{0,0,0}}, #{}},
        {{ok, <<"\"1970-01-01T00:00:00.000Z\"">>}, {0,0,0}, #{}}
    ]].

-endif.
