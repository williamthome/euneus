%%%---------------------------------------------------------------------
%%% @copyright 2023 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc JSON formatter.
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
-module(euneus_formatter).

-compile({ inline, pp_object/5 }).
-compile({ inline, pp_array/5 }).
-compile({ inline, pp_newline/3 }).

%% API functions

-export([ minify/1 ]).
-export([ prettify/1 ]).
-export([ format/2 ]).
-export([ parse_opts/1 ]).
-export([ format_parsed/2 ]).

%% Types

-export_type([ input/0 ]).
-export_type([ options/0 ]).
-export_type([ parsed_options/0 ]).
-export_type([ result/0 ]).

-record(opts, { spaces :: binary()
              , indent :: binary()
              , crlf :: binary()
              }).

-type input() :: binary() | iolist().
-type options() :: #{ spaces => binary() | non_neg_integer()
                    , indent => binary() | non_neg_integer()
                    , crlf => binary() | cr | lf | crlf
                    }.
-type parsed_options() :: #opts{}.
-type result() :: iolist().

%%%=====================================================================
%%% API functions
%%%=====================================================================

%%----------------------------------------------------------------------
%% @doc Remove extra spaces and line feeds from JSON.
%%
%% @param JSON :: {@link euneus_formatter:input()}.
%%
%% @returns {@link euneus_formatter:result()}.
%%
%% @see euneus_formatter:format_parsed/2
%%
%% @end
%%----------------------------------------------------------------------
-spec minify(input()) -> result().

minify(JSON) ->
    Opts = #opts{
        spaces = <<>>,
        indent = <<>>,
        crlf = <<>>
    },
    format_parsed(JSON, Opts).

%%----------------------------------------------------------------------
%% @doc Format JSON for printing.
%%
%% @param JSON :: {@link euneus_formatter:input()}.
%%
%% @returns {@link euneus_formatter:result()}.
%%
%% @see euneus_formatter:format_parsed/2
%%
%% @end
%%----------------------------------------------------------------------
-spec prettify(input()) -> result().

prettify(JSON) ->
    Opts = #opts{
        spaces = <<$\s>>,
        indent = <<$\s, $\s>>,
        crlf = <<$\n>>
    },
    format_parsed(JSON, Opts).

%%----------------------------------------------------------------------
%% @doc Format JSON.
%%
%% @param JSON :: {@link euneus_formatter:input()}.
%% @param Opts :: {@link euneus_formatter:options()}.
%%
%% @returns {@link euneus_formatter:result()}.
%%
%% @end
%%----------------------------------------------------------------------
-spec format(input(), options()) -> result().

format(JSON, Opts) ->
    format_parsed(JSON, parse_opts(Opts)).

%%----------------------------------------------------------------------
%% @doc Parses {@link euneus_formatter:options()} to {@link euneus_formatter:parsed_options()}.
%%
%% The parsed map can be expanded in compile time or stored to be
%% reused, avoiding parsing the options in every encoding.
%%
%% @param Opts :: {@link euneus_formatter:options()}.
%%
%% @returns {@link euneus_formatter:parsed_options()}.
%%
%% @end
%%----------------------------------------------------------------------
-spec parse_opts(options()) -> parsed_options().

parse_opts(Opts) ->
    #opts{
        spaces = case maps:get(spaces, Opts, <<>>) of
            N when is_integer(N), N >= 0 ->
                binary:copy(<<$\s>>, N);
            Spaces when is_binary(Spaces) ->
                Spaces
        end,
        indent = case maps:get(indent, Opts, <<>>) of
            N when is_integer(N), N >= 0 ->
                binary:copy(<<$\t>>, N);
            Indent when is_binary(Indent) ->
                Indent
        end,
        crlf = case maps:get(crlf, Opts, <<>>) of
            cr ->
                <<$\r>>;
            lf ->
                <<$\n>>;
            crlf ->
                <<$\r, $\n>>;
            Crlf when is_binary(Crlf) ->
                Crlf
        end
    }.

%%----------------------------------------------------------------------
%% @doc Format JSON.
%%
%% @param JSON :: {@link euneus_formatter:input()}.
%% @param Opts :: {@link euneus_formatter:parsed_options()}.
%%
%% @returns {@link euneus_formatter:result()}.
%%
%% @see euneus_formatter:parse_opts/1
%%
%% @end
%%----------------------------------------------------------------------
-spec format_parsed(input(), parsed_options()) -> result().

format_parsed(JSON, Opts) when is_binary(JSON) ->
    pp_value(JSON, Opts, 0, 0, []);
format_parsed(JSON, Opts) when is_list(JSON) ->
    format_parsed(iolist_to_binary(JSON), Opts).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

pp_value(Data, Opts, Prev, Depth, Buffer) ->
    case Data of
        <<$\s, Rest/binary>> ->
            pp_value(Rest, Opts, Prev, Depth, Buffer);
        <<$\t, Rest/binary>> ->
            pp_value(Rest, Opts, Prev, Depth, Buffer);
        <<$\r, Rest/binary>> ->
            pp_value(Rest, Opts, Prev, Depth, Buffer);
        <<$\n, Rest/binary>> ->
            pp_value(Rest, Opts, Prev, Depth, Buffer);
        <<$", Rest/bitstring>> ->
            case Prev =:= ${ orelse Prev =:= $[ of
                true ->
                    Newline = pp_newline(Opts#opts.crlf, Opts#opts.indent, Depth),
                    pp_string(Rest, Opts, Prev, Depth, <<$">>, [Newline | Buffer]);
                false ->
                    pp_string(Rest, Opts, Prev, Depth, <<$">>, Buffer)
            end;
        <<$,, Rest/bitstring>> ->
            Newline = pp_newline(Opts#opts.crlf, Opts#opts.indent, Depth),
            pp_value(Rest, Opts, $,, Depth, [Newline, $, | Buffer]);
        <<${, Rest/bitstring>> ->
            pp_object(Rest, Opts, Prev, Depth, Buffer);
        <<$}, Rest/bitstring>> ->
            case Prev =:= ${ of
                true ->
                    pp_value(Rest, Opts, $}, Depth - 1, [$} | Buffer]);
                false ->
                    Depth1 = Depth - 1,
                    Newline = pp_newline(Opts#opts.crlf, Opts#opts.indent, Depth1),
                    pp_value(Rest, Opts, $}, Depth1, [$}, Newline | Buffer])
            end;
        <<$[, Rest/bitstring>> ->
            pp_array(Rest, Opts, Prev, Depth, Buffer);
        <<$], Rest/bitstring>> ->
            case Prev =:= $[ of
                true ->
                    pp_value(Rest, Opts, $], Depth - 1, [$] | Buffer]);
                false ->
                    Depth1 = Depth - 1,
                    Newline = pp_newline(Opts#opts.crlf, Opts#opts.indent, Depth1),
                    pp_value(Rest, Opts, $], Depth1, [$], Newline | Buffer])
            end;
        <<Rest/bitstring>> ->
            pp_number(Rest, Opts, Prev, Depth, <<>>, Buffer)
    end.

pp_string(Data, Opts, Prev, Depth, String, Buffer) ->
    case Data of
        <<$\\, $", Rest/binary>> ->
            pp_string(Rest, Opts, Prev, Depth, <<String/bitstring, $\\, $">>, Buffer);
        <<$", Rest/binary>> ->
            pp_value(Rest, Opts, Prev, Depth, [<<String/bitstring, $">> | Buffer]);
        <<H, Rest/bitstring>> ->
            pp_string(Rest, Opts, Prev, Depth, <<String/bitstring, H>>, Buffer)
    end.

pp_number(Data, Opts, Prev, Depth, Value, Buffer) ->
    case Data of
        <<$\s, Rest/binary>> ->
            pp_value(Rest, Opts, Prev, Depth, [Value | Buffer]);
        <<$\t, Rest/binary>> ->
            pp_value(Rest, Opts, Prev, Depth, [Value | Buffer]);
        <<$\r, Rest/binary>> ->
            pp_value(Rest, Opts, Prev, Depth, [Value | Buffer]);
        <<$\n, Rest/binary>> ->
            pp_value(Rest, Opts, Prev, Depth, [Value | Buffer]);
        <<$:, Rest/binary>> ->
            pp_value(Rest, Opts, $:, Depth, [Value, Opts#opts.spaces, $: | Buffer]);
        <<$,, Rest/binary>> ->
            Newline = pp_newline(Opts#opts.crlf, Opts#opts.indent, Depth),
            pp_value(Rest, Opts, $,, Depth, [Newline, $,, Value | Buffer]);
        <<$], Rest/bitstring>> ->
            case Prev =:= $[ of
                true ->
                    pp_value(Rest, Opts, $], Depth - 1, [$], Value | Buffer]);
                false ->
                    Depth1 = Depth - 1,
                    Newline = pp_newline(Opts#opts.crlf, Opts#opts.indent, Depth1),
                    pp_value(Rest, Opts, $], Depth1, [$], Newline, Value | Buffer])
            end;
        <<$}, Rest/bitstring>> ->
            case Prev =:= ${ of
                true ->
                    pp_value(Rest, Opts, $}, Depth - 1, [$}, Value | Buffer]);
                false ->
                    Depth1 = Depth - 1,
                    Newline = pp_newline(Opts#opts.crlf, Opts#opts.indent, Depth1),
                    pp_value(Rest, Opts, $}, Depth1, [$}, Newline, Value | Buffer])
            end;
        <<H, Rest/bitstring>> ->
            pp_number(Rest, Opts, Prev, Depth, <<Value/bitstring, H>>, Buffer);
        <<>> ->
            lists:reverse([Value | Buffer])
    end.

pp_object(Data, Opts, Prev, Depth, Buffer) ->
    case Buffer =:= [] of
        true ->
            pp_value(Data, Opts, ${, Depth + 1, [${ | Buffer]);
        false ->
            case Prev =:= $: orelse Prev =:= $, of
                true ->
                    pp_value(Data, Opts, ${, Depth + 1, [${ | Buffer]);
                false ->
                    Newline = pp_newline(Opts#opts.crlf, Opts#opts.indent, Depth),
                    pp_value(Data, Opts, ${, Depth + 1, [${, Newline | Buffer])
            end
    end.

pp_array(Data, Opts, Prev, Depth, Buffer) ->
    case Buffer =:= [] of
        true ->
            pp_value(Data, Opts, $[, Depth + 1, [$[ | Buffer]);
        false ->
            case Prev =:= $: orelse Prev =:= $, of
                true ->
                    pp_value(Data, Opts, $[, Depth + 1, [$[ | Buffer]);
                false ->
                    Newline = pp_newline(Opts#opts.crlf, Opts#opts.indent, Depth),
                    pp_value(Data, Opts, $[, Depth + 1, [$[, Newline | Buffer])
            end
    end.

pp_newline(Crlf, Indent, Depth) ->
    [Crlf, binary:copy(Indent, Depth)].

%%%=====================================================================
%%% Eunit tests
%%%=====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

newline_test() ->
    [ ?assertEqual(Expect, iolist_to_binary(pp_newline(Crlf, Indent, Depth)))
        || {Expect, Crlf, Indent, Depth} <- [
        {<<>>, <<>>, <<>>, 0},
        {<<>>, <<>>, <<>>, 1},
        {<<$\n>>, <<$\n>>, <<$\s>>, 0},
        {<<$\n,$\s,$\s>>, <<$\n>>, <<$\s>>, 2}
    ]].

format_test() ->
    [ ?assertEqual(Expect, iolist_to_binary(format(JSON, Opts)))
        || {Expect, JSON, Opts} <- [
        {<<"\"foo\"">>, <<"\"foo\"">>, #{}},
        {<<"\"foo\"">>, <<" \s \n \"foo\" \r \t ">>, #{}},
        {<<"\"f\\\"o\\\"o\"">>, <<"\"f\\\"o\\\"o\"">>, #{}},
        {<<"1">>, <<"1">>, #{}},
        {<<"1.010">>, <<"1.010">>, #{}},
        {<<"true">>, <<"true">>, #{}},
        {<<"false">>, <<"false">>, #{}},
        {<<"null">>, <<"null">>, #{}},
        {<<"{}">>, <<"{}">>, #{}},
        {<<"[]">>, <<"[]">>, #{}},
        {<<"{\"foo\":\"bar\",\"0\":0}">>, <<"{\"foo\":\"bar\",\"0\":0}">>, #{}},
        {<<"[\"foo\",0]">>, <<"[\"foo\",0]">>, #{}}
    ]].

minify_test() ->
    [ ?assertEqual(Expect, iolist_to_binary(minify(JSON)))
        || {Expect, JSON} <- [
            {<<"{\"foo\":\"bar\",\"0\":0}">>, <<"{\"foo\" :  \"bar\",\"0\"  :   0}">>},
            {<<"[\"foo\",0]">>, <<" [    \"foo\"  ,   0  ]">>}
    ]].

prettify_test() ->
    [ ?assertEqual(Expect, iolist_to_binary(prettify(JSON)))
        || {Expect, JSON} <- [
        {<<"{}">>, <<"{}">>},
        {<<"[]">>, <<"[]">>},
        { <<"{\n  \"foo\": \"bar\",\n  \"baz\": {\n    \"foo\": \"bar\",\n    \"0\": [\n      \"foo\",\n      0\n    ]\n  }\n}">>
        , <<"{\"foo\":\"bar\",\"baz\":{\"foo\":\"bar\",\"0\":[\"foo\",0]}}">> }
    ]].

-endif.
