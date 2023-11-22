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

-compile({ inline, parse_options_to_settings/1 }).
-compile({ inline, get_codecs/2 }).
-compile({ inline, get_encode_fun/2 }).
-compile({ inline, key/3 }).
-compile({ inline, key_to_bin/1 }).
-compile({ inline, normalize_binary_codecs/1 }).
-compile({ inline, normalize_atom_codecs/1 }).
-compile({ inline, normalize_integer_codecs/1 }).
-compile({ inline, normalize_float_codecs/1 }).
-compile({ inline, normalize_list_codecs/1 }).
-compile({ inline, normalize_map_codecs/1 }).
-compile({ inline, normalize_tuple_codecs/1 }).
-compile({ inline, normalize_pid_codecs/1 }).
-compile({ inline, normalize_port_codecs/1 }).
-compile({ inline, normalize_reference_codecs/1 }).
-compile({ inline, codecs_loop/3 }).
-compile({ inline, proplist_list_codec/3 }).
-compile({ inline, skip_values_map_codec/3 }).
-compile({ inline, datetime_tuple_codec/3 }).
-compile({ inline, timestamp_tuple_codec/3 }).
-compile({ inline, ipv4_tuple_codec/3 }).
-compile({ inline, ipv6_tuple_codec/3 }).
-compile({ inline, encode_binary/2 }).
-compile({ inline, encode_atom/2 }).
-compile({ inline, encode_integer/2 }).
-compile({ inline, encode_float/2 }).
-compile({ inline, encode_list/2 }).
-compile({ inline, encode_map/2 }).
-compile({ inline, encode_tuple/2 }).
-compile({ inline, encode_pid/2 }).
-compile({ inline, encode_port/2 }).
-compile({ inline, encode_reference/2 }).
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
-compile({ inline_list_funcs, true }).

% By default, encode_tuple/2 will raise unsupported_type exception,
% so it is a function without a local return. Note that parse_opts/1
% is included because unhandled_encoder option has no local return.
-dialyzer({ no_return, parse_options_to_settings/1 }).
-dialyzer({ no_return, encode_tuple/2 }).
-dialyzer( no_improper_lists ).

%% API functions

-export([ encode/2 ]).
-export([ parse_options_to_settings/1 ]).
-export([ get_null_values_setting/1 ]).
-export([ get_binary_codecs_setting/1 ]).
-export([ get_atom_codecs_setting/1 ]).
-export([ get_integer_codecs_setting/1 ]).
-export([ get_float_codecs_setting/1 ]).
-export([ get_list_codecs_setting/1 ]).
-export([ get_map_codecs_setting/1 ]).
-export([ get_tuple_codecs_setting/1 ]).
-export([ get_pid_codecs_setting/1 ]).
-export([ get_port_codecs_setting/1 ]).
-export([ get_reference_codecs_setting/1 ]).
-export([ get_encode_binary_setting/1 ]).
-export([ get_encode_atom_setting/1 ]).
-export([ get_encode_integer_setting/1 ]).
-export([ get_encode_float_setting/1 ]).
-export([ get_encode_list_setting/1 ]).
-export([ get_encode_map_setting/1 ]).
-export([ get_encode_tuple_setting/1 ]).
-export([ get_encode_pid_setting/1 ]).
-export([ get_encode_port_setting/1 ]).
-export([ get_encode_reference_setting/1 ]).
-export([ get_escape_setting/1 ]).
-export([ get_handle_error_setting/1 ]).
-export([ encode_parsed/2 ]).
-export([ encode_binary/2 ]).
-export([ encode_atom/2 ]).
-export([ encode_integer/2 ]).
-export([ encode_float/2 ]).
-export([ encode_list/2 ]).
-export([ encode_map/2 ]).
-export([ encode_tuple/2 ]).
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
-export_type([ settings/0 ]).
-export_type([ result/0 ]).
-export_type([ encode_fun/1 ]).
-export_type([ escape_fun/1 ]).
-export_type([ handle_error_fun/0 ]).
-export_type([ error_reason/0 ]).

-record(settings, { null_values       :: [null()]
                  , integer_base      :: integer_base()
                  , float_to_bin_opts :: float_to_binary_options()
                  , binary_codecs     :: [{codec_fun(binary()), codec_options()}]
                  , atom_codecs       :: [{codec_fun(atom()), codec_options()}]
                  , integer_codecs    :: [{codec_fun(integer()), codec_options()}]
                  , float_codecs      :: [{codec_fun(float()), codec_options()}]
                  , list_codecs       :: [{codec_fun(list()), codec_options()}]
                  , map_codecs        :: [{codec_fun(map()), codec_options()}]
                  , tuple_codecs      :: [{codec_fun(tuple()), codec_options()}]
                  , pid_codecs        :: [{codec_fun(pid()), codec_options()}]
                  , port_codecs       :: [{codec_fun(port()), codec_options()}]
                  , reference_codecs  :: [{codec_fun(reference()), codec_options()}]
                  , encode_binary     :: encode_fun(binary())
                  , encode_atom       :: encode_fun(atom())
                  , encode_integer    :: encode_fun(integer())
                  , encode_float      :: encode_fun(float())
                  , encode_list       :: encode_fun(list())
                  , encode_map        :: encode_fun(map())
                  , encode_tuple      :: encode_fun(tuple())
                  , encode_pid        :: encode_fun(pid())
                  , encode_port       :: encode_fun(port())
                  , encode_reference  :: encode_fun(reference())
                  , escape            :: escape_fun(binary())
                  , handle_error      :: handle_error_fun()
                  }).

-type null() :: atom().
-type integer_base() :: 2..36.
-type float_to_binary_options() :: [ {decimals, Decimals :: 0..253}
                                   | {scientific, Decimals :: 0..249}
                                   | compact
                                   | short ].
-type codec_options()    :: term().
-type codec_result(Type) :: next
                          | {next, Type}
                          | {encode, Type}
                          | {halt, iolist()}
                          | {new_type, term()}.
-type codec_fun(Type)    :: fun(( Type
                                , codec_options()
                                , #settings{} ) -> codec_result(Type)).
-type codec_behavior()   :: module().
-type codec(Type)        :: codec_fun(Type)
                          | {codec_fun(Type), codec_options()}
                          | codec_behavior()
                          | {codec_behavior(), codec_options()}.

-type input() :: term().
-type options() :: #{
    null_values => [null()],
    binary => #{
        encode => encode_fun(binary()),
        codecs => [codec(binary())]
    },
    atom => #{
        encode => encode_fun(atom()),
        codecs => [codec(atom())]
    },
    integer => #{
        encode => encode_fun(integer()),
        codecs => [codec(integer())],
        base => integer_base()
    },
    float => #{
        encode => encode_fun(float()),
        codecs => [codec(float())],
        to_binary_options => float_to_binary_options()
    },
    list => #{
        encode => encode_fun(list()),
        codecs => [proplist | codec(list())]
    },
    map => #{
        encode => encode_fun(map()),
        codecs => [codec(map())],
        skip_values => [term()]
    },
    tuple => #{
        encode => encode_fun(tuple()),
        codecs => [ datetime
                  | timestamp
                  | ipv4
                  | ipv6
                  % TODO: {record, Opts}
                  | codec(tuple()) ]
    },
    pid => #{
        encode => encode_fun(pid()),
        codecs => [codec(pid())]
    },
    port => #{
        encode => encode_fun(port()),
        codecs => [codec(port())]
    },
    reference => #{
        encode => encode_fun(reference()),
        codecs => [codec(reference())]
    },
    escape => json
            | html
            | javascript
            | unicode
            | escape_fun(binary()),
    handle_error => handle_error_fun()
}.
-type settings() :: #settings{}.
-type result() :: {ok, iolist()} | {error, error_reason()}.
-type encode_fun(Input) :: fun((Input, settings()) -> iolist()).
-type escape_fun(Input) :: fun((Input, settings()) -> iolist()).
-type error_class() :: error | exit | throw.
-type unsupported_type_error() :: {unsupported_type, Unsupported :: term()}.
-type invalid_byte_error() :: {invalid_byte, Byte :: byte(), Input :: binary()}.
-type error_reason() :: unsupported_type_error() | invalid_byte_error().
-type error_stacktrace() :: erlang:stacktrace().
-type handle_error_fun() :: fun(( error_class()
                                , error_reason()
                                , error_stacktrace() ) -> result()).

%% Macros

-define(min(X, Min), is_integer(X) andalso X >= Min).
-define(range(X, Min, Max), is_integer(X) andalso X >= Min andalso X =< Max).
-define(is_proplist_key(X), is_binary(X) orelse is_atom(X) orelse is_integer(X)).
-define(is_codec_fun(X), is_function(X, 3)).

-define(NON_PRINTABLE_LAST, 31).
-define(ONE_BYTE_LAST, 127).
-define(TWO_BYTE_LAST, 2_047).
-define(THREE_BYTE_LAST, 65_535).

-define(DEFAULT_CODEC_OPTS, []).
-define(DEFAULT_NULL_VALUES, [null, undefined]).
-define(DEFAULT_SKIP_VALUES, [undefined]).
-define(DEFAULT_INTEGER_BASE, 10).
-define(DEFAULT_FLOAT_TO_BIN_OPTS, [short]).

%%%=====================================================================
%%% API functions
%%%=====================================================================

%%----------------------------------------------------------------------
%% @doc Generates a JSON from Erlang term.
%%
%% @param Term :: {@link euneus_encoder:input()}.
%% @param Opts :: {@link euneus_encoder:options()}.
%%
%% @returns {@link euneus_encoder:result()}.
%%
%% @end
%%----------------------------------------------------------------------
-spec encode(input(), options()) -> result().

encode(Term, Opts) ->
    encode_parsed(Term, parse_options_to_settings(Opts)).

%%----------------------------------------------------------------------
%% @doc Parses {@link euneus_encoder:options()} to {@link euneus_encoder:settings()}.
%%
%% The parsed map can be expanded in compile time or stored to be
%% reused, avoiding parsing the options in every encoding.
%%
%% @end
%%
%% NOTE: The explicit call of functions wrapped in another function is
%% required for the inline optimization.
%%
%% @param Opts :: {@link euneus_encoder:options()}.
%%
%% @returns {@link euneus_encoder:settings()}.
%%
%% @end
%%----------------------------------------------------------------------
-spec parse_options_to_settings(options()) -> settings().

parse_options_to_settings(Opts) ->
    BinOpts = maps_get(binary, Opts, #{}),
    AtomOpts = maps_get(atom, Opts, #{}),
    IntOpts = maps_get(integer, Opts, #{}),
    FloatOpts = maps_get(float, Opts, #{}),
    ListOpts = maps_get(list, Opts, #{}),
    MapOpts = maps_get(map, Opts, #{}),
    TupleOpts = maps_get(tuple, Opts, #{}),
    PidOpts = maps_get(pid, Opts, #{}),
    PortOpts = maps_get(port, Opts, #{}),
    RefOpts = maps_get(reference, Opts, #{}),
    #settings{
        null_values = maps_get(null_values, Opts, ?DEFAULT_NULL_VALUES),
        integer_base = maps_get(integer_base, Opts, ?DEFAULT_INTEGER_BASE),
        float_to_bin_opts = maps_get(to_binary_options, FloatOpts, ?DEFAULT_FLOAT_TO_BIN_OPTS),
        binary_codecs = normalize_binary_codecs(maps_get(codecs, BinOpts, [])),
        atom_codecs = normalize_atom_codecs(maps_get(codecs, AtomOpts, [])),
        integer_codecs = normalize_integer_codecs(maps_get(codecs, IntOpts, [])),
        float_codecs = normalize_float_codecs(maps_get(codecs, FloatOpts, [])),
        list_codecs = normalize_list_codecs(maps_get(codecs, ListOpts, [])),
        map_codecs = case maps_get(skip_values, MapOpts, ?DEFAULT_SKIP_VALUES) of
            [] ->
                normalize_map_codecs(maps_get(codecs, MapOpts, []));
            ValuesToSkip ->
                normalize_map_codecs([ {skip_values, ValuesToSkip}
                                     | maps_get(codecs, MapOpts, []) ])
        end,
        tuple_codecs = normalize_tuple_codecs(maps_get(codecs, TupleOpts, [])),
        pid_codecs = normalize_pid_codecs(maps_get(codecs, PidOpts, [])),
        port_codecs = normalize_port_codecs(maps_get(codecs, PortOpts, [])),
        reference_codecs = normalize_reference_codecs(maps_get(codecs, RefOpts, [])),
        encode_binary = maps_get(encode, BinOpts, fun (X, O) ->
            escape(X, O)
        end),
        encode_atom = maps_get(encode, AtomOpts, fun (X, O) ->
            encode_atom(X, O)
        end),
        encode_integer = maps_get(encode, IntOpts, fun (X, O) ->
            encode_integer(X, O)
        end),
        encode_float = maps_get(encode, FloatOpts, fun (X, O) ->
            encode_float(X, O)
        end),
        encode_list = maps_get(encode, ListOpts, fun (X, O) ->
            encode_list(X, O)
        end),
        encode_map = maps_get(encode, MapOpts, fun (X, O) ->
            encode_map(X, O)
        end),
        encode_tuple = maps_get(encode, TupleOpts, fun (X, O) ->
            encode_tuple(X, O)
        end),
        encode_pid = maps_get(encode, PidOpts, fun (X, O) ->
            encode_pid(X, O)
        end),
        encode_port = maps_get(encode, PortOpts, fun (X, O) ->
            encode_port(X, O)
        end),
        encode_reference = maps_get(encode, RefOpts, fun (X, O) ->
            encode_reference(X, O)
        end),
        escape =
            case maps_get(escape, Opts, json) of
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
        handle_error = maps_get(handle_error, Opts, fun(C, R, S) ->
            handle_error(C, R, S)
        end)
    }.

% NOTE: The explicit call of functions wrapped in another function is
%       required for the inline optimization.

normalize_binary_codecs(Codecs) ->
    lists:map(fun
        ({Fun, Opts}) when ?is_codec_fun(Fun) ->
            {Fun, Opts};
        (Fun) when ?is_codec_fun(Fun) ->
            {Fun, ?DEFAULT_CODEC_OPTS};
        ({Mod, Opts}) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(Opts),
            {fun Mod:encode/3, ParsedOpts};
        (Mod) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(?DEFAULT_CODEC_OPTS),
            {fun Mod:encode/3, ParsedOpts}
    end, Codecs).

normalize_atom_codecs(Codecs) ->
    lists:map(fun
        ({Fun, Opts}) when ?is_codec_fun(Fun) ->
            {Fun, Opts};
        (Fun) when ?is_codec_fun(Fun) ->
            {Fun, ?DEFAULT_CODEC_OPTS};
        ({Mod, Opts}) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(Opts),
            {fun Mod:encode/3, ParsedOpts};
        (Mod) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(?DEFAULT_CODEC_OPTS),
            {fun Mod:encode/3, ParsedOpts}
    end, Codecs).

normalize_integer_codecs(Codecs) ->
    lists:map(fun
        ({Fun, Opts}) when ?is_codec_fun(Fun) ->
            {Fun, Opts};
        (Fun) when ?is_codec_fun(Fun) ->
            {Fun, ?DEFAULT_CODEC_OPTS};
        ({Mod, Opts}) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(Opts),
            {fun Mod:encode/3, ParsedOpts};
        (Mod) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(?DEFAULT_CODEC_OPTS),
            {fun Mod:encode/3, ParsedOpts}
    end, Codecs).

normalize_float_codecs(Codecs) ->
    lists:map(fun
        ({Fun, Opts}) when ?is_codec_fun(Fun) ->
            {Fun, Opts};
        (Fun) when ?is_codec_fun(Fun) ->
            {Fun, ?DEFAULT_CODEC_OPTS};
        ({Mod, Opts}) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(Opts),
            {fun Mod:encode/3, ParsedOpts};
        (Mod) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(?DEFAULT_CODEC_OPTS),
            {fun Mod:encode/3, ParsedOpts}
    end, Codecs).

normalize_list_codecs(Codecs) ->
    lists:map(fun
        (proplist) ->
            {fun(List, Opts, Settings) ->
                proplist_list_codec(List, Opts, Settings)
            end, ?DEFAULT_CODEC_OPTS};
        ({Fun, Opts}) when ?is_codec_fun(Fun) ->
            {Fun, Opts};
        (Fun) when ?is_codec_fun(Fun) ->
            {Fun, ?DEFAULT_CODEC_OPTS};
        ({Mod, Opts}) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(Opts),
            {fun Mod:encode/3, ParsedOpts};
        (Mod) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(?DEFAULT_CODEC_OPTS),
            {fun Mod:encode/3, ParsedOpts}
    end, Codecs).

normalize_map_codecs(Codecs) ->
    lists:map(fun
        ({skip_values, ValuesToSkip}) when is_list(ValuesToSkip) ->
            {fun(Map, Opts, Settings) ->
                skip_values_map_codec(Map, Opts, Settings)
            end, ValuesToSkip};
        ({Fun, Opts}) when ?is_codec_fun(Fun) ->
            {Fun, Opts};
        (Fun) when ?is_codec_fun(Fun) ->
            {Fun, ?DEFAULT_CODEC_OPTS};
        ({Mod, Opts}) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(Opts),
            {fun Mod:encode/3, ParsedOpts};
        (Mod) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(?DEFAULT_CODEC_OPTS),
            {fun Mod:encode/3, ParsedOpts}
    end, Codecs).

normalize_tuple_codecs(Codecs) ->
    lists:map(fun
        (datetime) ->
            {fun(Tuple, Opts, Settings) ->
                 datetime_tuple_codec(Tuple, Opts, Settings)
            end, ?DEFAULT_CODEC_OPTS};
        (timestamp) ->
            {fun(Tuple, Opts, Settings) ->
                timestamp_tuple_codec(Tuple, Opts, Settings)
            end, ?DEFAULT_CODEC_OPTS};
        (ipv4) ->
            {fun(Tuple, Opts, Settings) ->
                ipv4_tuple_codec(Tuple, Opts, Settings)
            end, ?DEFAULT_CODEC_OPTS};
        (ipv6) ->
            {fun(Tuple, Opts, Settings) ->
                ipv6_tuple_codec(Tuple, Opts, Settings)
            end, ?DEFAULT_CODEC_OPTS};
        ({Fun, Opts}) when ?is_codec_fun(Fun) ->
            {Fun, Opts};
        (Fun) when ?is_codec_fun(Fun) ->
            {Fun, ?DEFAULT_CODEC_OPTS};
        ({Mod, Opts}) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(Opts),
            {fun Mod:encode/3, ParsedOpts};
        (Mod) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(?DEFAULT_CODEC_OPTS),
            {fun Mod:encode/3, ParsedOpts}
    end, Codecs).

normalize_pid_codecs(Codecs) ->
    lists:map(fun
        ({Fun, Opts}) when ?is_codec_fun(Fun) ->
            {Fun, Opts};
        (Fun) when ?is_codec_fun(Fun) ->
            {Fun, ?DEFAULT_CODEC_OPTS};
        ({Mod, Opts}) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(Opts),
            {fun Mod:encode/3, ParsedOpts};
        (Mod) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(?DEFAULT_CODEC_OPTS),
            {fun Mod:encode/3, ParsedOpts}
    end, Codecs).

normalize_port_codecs(Codecs) ->
    lists:map(fun
        ({Fun, Opts}) when ?is_codec_fun(Fun) ->
            {Fun, Opts};
        (Fun) when ?is_codec_fun(Fun) ->
            {Fun, ?DEFAULT_CODEC_OPTS};
        ({Mod, Opts}) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(Opts),
            {fun Mod:encode/3, ParsedOpts};
        (Mod) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(?DEFAULT_CODEC_OPTS),
            {fun Mod:encode/3, ParsedOpts}
    end, Codecs).

normalize_reference_codecs(Codecs) ->
    lists:map(fun
        ({Fun, Opts}) when ?is_codec_fun(Fun) ->
            {Fun, Opts};
        (Fun) when ?is_codec_fun(Fun) ->
            {Fun, ?DEFAULT_CODEC_OPTS};
        ({Mod, Opts}) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(Opts),
            {fun Mod:encode/3, ParsedOpts};
        (Mod) when is_atom(Mod) ->
            {ok, ParsedOpts} = Mod:init(?DEFAULT_CODEC_OPTS),
            {fun Mod:encode/3, ParsedOpts}
    end, Codecs).

%%%---------------------------------------------------------------------
%%% Options
%%%---------------------------------------------------------------------

get_null_values_setting(#settings{null_values = Nulls}) ->
    Nulls.

get_binary_codecs_setting(#settings{binary_codecs = Codecs}) ->
    Codecs.

get_atom_codecs_setting(#settings{atom_codecs = Codecs}) ->
    Codecs.

get_integer_codecs_setting(#settings{integer_codecs = Codecs}) ->
    Codecs.

get_float_codecs_setting(#settings{float_codecs = Codecs}) ->
    Codecs.

get_list_codecs_setting(#settings{list_codecs = Codecs}) ->
    Codecs.

get_map_codecs_setting(#settings{map_codecs = Codecs}) ->
    Codecs.

get_tuple_codecs_setting(#settings{tuple_codecs = Codecs}) ->
    Codecs.

get_pid_codecs_setting(#settings{pid_codecs = Codecs}) ->
    Codecs.

get_port_codecs_setting(#settings{port_codecs = Codecs}) ->
    Codecs.

get_reference_codecs_setting(#settings{reference_codecs = Codecs}) ->
    Codecs.

get_encode_binary_setting(#settings{encode_binary = EncodeFun}) ->
    EncodeFun.

get_encode_atom_setting(#settings{encode_atom = EncodeFun}) ->
    EncodeFun.

get_encode_integer_setting(#settings{encode_integer = EncodeFun}) ->
    EncodeFun.

get_encode_float_setting(#settings{encode_float = EncodeFun}) ->
    EncodeFun.

get_encode_list_setting(#settings{encode_list = EncodeFun}) ->
    EncodeFun.

get_encode_map_setting(#settings{encode_map = EncodeFun}) ->
    EncodeFun.

get_encode_tuple_setting(#settings{encode_tuple = EncodeFun}) ->
    EncodeFun.

get_encode_pid_setting(#settings{encode_pid = EncodeFun}) ->
    EncodeFun.

get_encode_port_setting(#settings{encode_port = EncodeFun}) ->
    EncodeFun.

get_encode_reference_setting(#settings{encode_reference = EncodeFun}) ->
    EncodeFun.

get_escape_setting(#settings{escape = EscapeFun}) ->
    EscapeFun.

get_handle_error_setting(#settings{handle_error = HandleErrorFun}) ->
    HandleErrorFun.

%%----------------------------------------------------------------------
%% @doc Generates a JSON from Erlang term.
%%
%% @param Term :: {@link euneus_encoder:input()}.
%% @param Opts :: {@link euneus_encoder:settings()}.
%%
%% @returns {@link euneus_encoder:result()}.
%%
%% @see euneus_encoder:parse_opts/1
%%
%% @end
%%----------------------------------------------------------------------
-spec encode_parsed(input(), settings()) -> result().

encode_parsed(Term, Opts) ->
    try
        {ok, value(Term, Opts)}
    catch
        Class:Reason:Stacktrace ->
            HandleError = Opts#settings.handle_error,
            HandleError(Class, Reason, Stacktrace)
    end.

encode_binary(Bin, Settings) ->
    escape(Bin, Settings).

encode_atom(true, _Settings) ->
    <<"true">>;
encode_atom(false, _Settings) ->
    <<"false">>;
encode_atom(Atom, Settings) ->
    case lists:member(Atom, Settings#settings.null_values) of
        true ->
            <<"null">>;
        false ->
            escape(atom_to_binary(Atom, utf8), Settings)
    end.

encode_integer(Int, Settings) ->
    integer_to_binary(Int, Settings#settings.integer_base).

encode_float(Float, Settings) ->
    float_to_binary(Float, Settings#settings.float_to_bin_opts).

encode_list([H | T], Settings) ->
    [$[, value(H, Settings), do_encode_list_loop(T, Settings)];
encode_list([], _Settings) ->
    <<"[]">>.

do_encode_list_loop([], _Settings) ->
    [$]];
do_encode_list_loop([H | T], Settings) ->
    [$,, value(H, Settings) | do_encode_list_loop(T, Settings)].

encode_map(Map, Settings) ->
    KeyEncode = Settings#settings.encode_binary,
    do_encode_map(maps_to_list(Map), KeyEncode, Settings).

do_encode_map([{K, V} | T], KeyEncode, Settings) ->
    [${, key(KeyEncode, K, Settings), $:, value(V, Settings) | do_encode_map_loop(T, KeyEncode, Settings)];
do_encode_map([], _, _) ->
    <<"{}">>.

do_encode_map_loop([], _, _Settings) ->
    [$}];
do_encode_map_loop([{K, V} | T], KeyEncode, Settings) ->
    [$,, key(KeyEncode, K, Settings), $:, value(V, Settings) | do_encode_map_loop(T, KeyEncode, Settings)].

encode_tuple(Tuple, Settings) ->
    value(tuple_to_list(Tuple), Settings).

encode_pid(Pid, Settings) ->
    escape(list_to_binary(pid_to_list(Pid)), Settings).

encode_port(Port, Settings) ->
    escape(list_to_binary(port_to_list(Port)), Settings).

encode_reference(Ref, Settings) ->
    escape(list_to_binary(ref_to_list(Ref)), Settings).

escape(Bin, #settings{escape = Escape} = Settings) ->
    Escape(Bin, Settings).

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

key(Encode, Key, Settings) ->
    Encode(key_to_bin(Key), Settings).

key_to_bin(Key) when is_binary(Key) ->
    Key;
key_to_bin(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
key_to_bin(Key) when is_integer(Key) ->
    integer_to_binary(Key);
key_to_bin(Key) when is_list(Key) ->
    list_to_binary(Key).

value(Term, Settings) ->
    case get_codecs(Term, Settings) of
        [] ->
            Encode = get_encode_fun(Term, Settings),
            Encode(Term, Settings);
        Codecs ->
            case codecs_loop(Codecs, Term, Settings) of
                {encode, X} ->
                    Encode = get_encode_fun(Term, Settings),
                    Encode(X, Settings);
                {halt, X} ->
                    X;
                {new_type, X} ->
                    value(X, Settings)
            end
    end.

get_codecs(Term, Settings) when is_binary(Term) ->
    Settings#settings.binary_codecs;
get_codecs(Term, Settings) when is_atom(Term) ->
    Settings#settings.atom_codecs;
get_codecs(Term, Settings) when is_integer(Term) ->
    Settings#settings.integer_codecs;
get_codecs(Term, Settings) when is_float(Term) ->
    Settings#settings.float_codecs;
get_codecs(Term, Settings) when is_list(Term) ->
    Settings#settings.list_codecs;
get_codecs(Term, Settings) when is_map(Term) ->
    Settings#settings.map_codecs;
get_codecs(Term, Settings) when is_tuple(Term) ->
    Settings#settings.tuple_codecs;
get_codecs(Term, Settings) when is_pid(Term) ->
    Settings#settings.pid_codecs;
get_codecs(Term, Settings) when is_port(Term) ->
    Settings#settings.port_codecs;
get_codecs(Term, Settings) when is_reference(Term) ->
    Settings#settings.reference_codecs.

get_encode_fun(Term, Settings) when is_binary(Term) ->
    Settings#settings.encode_binary;
get_encode_fun(Term, Settings) when is_atom(Term) ->
    Settings#settings.encode_atom;
get_encode_fun(Term, Settings) when is_integer(Term) ->
    Settings#settings.encode_integer;
get_encode_fun(Term, Settings) when is_float(Term) ->
    Settings#settings.encode_float;
get_encode_fun(Term, Settings) when is_list(Term) ->
    Settings#settings.encode_list;
get_encode_fun(Term, Settings) when is_map(Term) ->
    Settings#settings.encode_map;
get_encode_fun(Term, Settings) when is_tuple(Term) ->
    Settings#settings.encode_tuple;
get_encode_fun(Term, Settings) when is_pid(Term) ->
    Settings#settings.encode_pid;
get_encode_fun(Term, Settings) when is_port(Term) ->
    Settings#settings.encode_port;
get_encode_fun(Term, Settings) when is_reference(Term) ->
    Settings#settings.encode_reference.

codecs_loop([], Term, _Settings) ->
    {encode, Term};
codecs_loop([{Fun, Opts} | Funs], Term, Settings) ->
    case Fun(Term, Opts, Settings) of
        next ->
            codecs_loop(Funs, Term, Settings);
        {next, X} ->
            codecs_loop(Funs, X, Settings);
        {encode, X} ->
            {encode, X};
        {halt, X} ->
            {halt, X};
        {new_type, X} ->
            {new_type, X}
    end.

skip_values_map_codec(Map, ValuesToSkip, _Settings) ->
    {next, maps:filter(fun(_, Value) ->
        not lists:member(Value, ValuesToSkip)
    end, Map)}.

% TODO: Check how to deal with proplists that starts with a boolean or
%       only have boolean values, for example:
%       ```
%       1> proplists:to_map([foo, {bar, baz}]).
%       #{foo => true,bar => baz}
%
%       2> proplists:to_map([foo, bar]).
%       #{foo => true,bar => true}
%       ```
proplist_list_codec([{Key, _} | _] = Proplist, _, _) when ?is_proplist_key(Key) ->
    {new_type, proplists:to_map(Proplist)};
proplist_list_codec(_, _, _) ->
    next.

datetime_tuple_codec({{YYYY,MM,DD},{H,M,S}}, _Opts, Settings)
  when ?min(YYYY, 0), ?range(MM, 1, 12), ?range(DD, 1, 31)
     , ?range(H, 0, 23), ?range(M, 0, 59), ?range(S, 0, 59) ->
    DateTime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [YYYY,MM,DD,H,M,S])
    ),
    {halt, escape(DateTime, Settings)};
datetime_tuple_codec(_, _, _) ->
    next.

timestamp_tuple_codec({MegaSecs, Secs, MicroSecs} = Timestamp, _Opts, Settings)
  when ?min(MegaSecs, 0), ?min(Secs, 0), ?min(MicroSecs, 0) ->
    MilliSecs = MicroSecs div 1000,
    {{YYYY,MM,DD},{H,M,S}} = calendar:now_to_datetime(Timestamp),
    DateTime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
        [YYYY,MM,DD,H,M,S,MilliSecs])
    ),
    {halt, escape(DateTime, Settings)};
timestamp_tuple_codec(_, _, _) ->
    next.

ipv4_tuple_codec({_A,_B,_C,_D} = Tuple, _Opts, _Settings) ->
    case inet_parse:ntoa(Tuple) of
        {error, einval} ->
            next;
        Ipv4 ->
            {new_type, list_to_binary(Ipv4)}
    end;
ipv4_tuple_codec(_, _, _) ->
    next.

ipv6_tuple_codec({_A,_B,_C,_D,_E,_F,_G,_H} = Tuple, _Opts, _Settings) ->
    case inet_parse:ntoa(Tuple) of
        {error, einval} ->
            next;
        Ipv6 ->
            {new_type, list_to_binary(Ipv6)}
    end;
ipv6_tuple_codec(_, _, _) ->
    next.

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

encode_to_bin(Input, Opts) ->
    case encode(Input, Opts) of
        {ok, IOList} ->
            {ok, iolist_to_binary(IOList)};
        {error, Reason} ->
            {error, Reason}
    end.

encode_test() ->
    [ ?assertEqual(Expect, encode_to_bin(Input, Opts))
      || {Expect, Input, Opts} <- [
        {{ok, <<"true">>}, true, #{}},
        {{ok, <<"\"foo\"">>}, foo, #{}},
        {{ok, <<"\"foo\"">>}, <<"foo">>, #{}},
        {{ok, <<"0">>}, 0, #{}},
        {{ok, <<"123.456789">>}, 123.45678900, #{}},
        {{ok, <<"[true,0,null]">>}, [true,0,null], #{}},
        {{ok, <<"{\"foo\":\"bar\"}">>}, #{foo => bar}, #{}},
        {{ok, <<"{\"0\":0}">>}, #{0 => 0}, #{}}
    ]].

proplist_list_codec_test() ->
    [ ?assertEqual(Expect, encode_to_bin(Input, Opts))
        || {Expect, Input, Opts} <- [
        { {ok, <<"{\"foo\":\"bar\"}">>}, [{foo, bar}], #{
            list => #{codecs => [proplist]}
        }}
    ]].

skip_values_map_codec_test() ->
    [ ?assertEqual(Expect, encode_to_bin(Input, Opts))
        || {Expect, Input, Opts} <- [
        { {ok, <<"{\"a\":1}">>}
        , #{a => 1, b => null}
        , #{map => #{codecs => [{skip_values, ?DEFAULT_NULL_VALUES}]}}
        },
        { {ok, <<"{\"a\":1}">>}
        , #{a => 1, b => undefined, c => foo}
        , #{ null_values => [undefined, foo]
           , map => #{codecs => [{skip_values, [undefined, foo]}]}}
        },
        { {ok, <<"{\"a\":1}">>}
        , [{a, 1}, {b, undefined}, {c, foo}]
        , #{ null_values => [undefined, foo]
           , list => #{codecs => [proplist]}
           , map => #{codecs => [{skip_values, [undefined, foo]}]}}
        }
    ]].

datetime_tuple_codec_test() ->
    [ ?assertEqual(Expect, encode_to_bin(Input, Opts))
        || {Expect, Input, Opts} <- [
        { {ok, <<"\"1970-01-01T00:00:00Z\"">>}
        , {{1970,1,1},{0,0,0}}
        , #{tuple => #{codecs => [datetime]}}
        }
    ]].

timestamp_tuple_codec_test() ->
    [ ?assertEqual(Expect, encode_to_bin(Input, Opts))
        || {Expect, Input, Opts} <- [
        { {ok, <<"\"1970-01-01T00:00:00.000Z\"">>}
        , {0,0,0}
        , #{tuple => #{codecs => [timestamp]}}
        }
    ]].

ipv4_tuple_codec_test() ->
    [ ?assertEqual(Expect, encode_to_bin(Input, Opts))
      || {Expect, Input, Opts} <- [
        {{ok, <<"\"0.0.0.0\"">>}, {0,0,0,0}, #{tuple => #{codecs => [ipv4]}}}
    ]].

ipv6_tuple_codec_test() ->
    [ ?assertEqual(Expect, encode_to_bin(Input, Opts))
      || {Expect, Input, Opts} <- [
        {{ok, <<"\"::\"">>}, {0,0,0,0,0,0,0,0}, #{
            tuple => #{codecs => [ipv6]}
        }},
        {{ok, <<"\"::1\"">>}, {0,0,0,0,0,0,0,1}, #{
            tuple => #{codecs => [ipv6]}
        }},
        { {ok, <<"\"::192.168.42.2\"">>}
        , {0,0,0,0,0,0,(192 bsl 8) bor 168,(42 bsl 8) bor 2}
        , #{tuple => #{codecs => [ipv6]}}
        },
        { {ok, <<"\"::ffff:192.168.42.2\"">>}
        , {0,0,0,0,0,16#FFFF,(192 bsl 8) bor 168,(42 bsl 8) bor 2}
        , #{tuple => #{codecs => [ipv6]}}
        },
        { {ok, <<"\"3ffe:b80:1f8d:2:204:acff:fe17:bf38\"">>}
        , {16#3ffe,16#b80,16#1f8d,16#2,16#204,16#acff,16#fe17,16#bf38}
        , #{tuple => #{codecs => [ipv6]}}
        },
        { {ok, <<"\"fe80::204:acff:fe17:bf38\"">>}
        , {16#fe80,0,0,0,16#204,16#acff,16#fe17,16#bf38}
        , #{tuple => #{codecs => [ipv6]}}
        }
    ]].

-endif.
