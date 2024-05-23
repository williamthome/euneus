%%%---------------------------------------------------------------------
%%% @copyright 2023-2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc JSON formatter.
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
-module(euneus_formatter).

-export([ minify/1
        , prettify/1
        , format/2
        , parse_opts/1
        ]).

-record(state, { spaces :: binary()
               , indent :: binary()
               , crlf :: binary()
               }).

-type options() :: #{ spaces => binary() | non_neg_integer()
                    , indent => binary() | non_neg_integer()
                    , crlf => binary() | cr | lf | crlf
                    }.

%%%=====================================================================
%%% API functions
%%%=====================================================================

-spec minify(iodata()) -> iodata().

minify(JSON) ->
    Opts = #state{
        spaces = <<>>,
        indent = <<>>,
        crlf = <<>>
    },
    format(JSON, Opts).

-spec prettify(iodata()) -> iodata().

prettify(JSON) ->
    Opts = #state{
        spaces = <<$\s>>,
        indent = <<$\s, $\s>>,
        crlf = <<$\n>>
    },
    format(JSON, Opts).

-spec format(iodata(), #state{} | options()) -> iodata().

format(JSON, #state{} = State) when is_binary(JSON) ->
    pp_value(JSON, State, 0, 0, []);
format(JSON, Opts) when is_binary(JSON) ->
    format(JSON, parse_opts(Opts));
format(JSON, Opts) when is_list(JSON) ->
    format(iolist_to_binary(JSON), Opts).

-spec parse_opts(options()) -> #state{}.

parse_opts(Opts) when is_map(Opts) ->
    #state{
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

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

pp_value(Data, State, Prev, Depth, Buffer) ->
    case Data of
        <<$\s, Rest/binary>> ->
            pp_value(Rest, State, Prev, Depth, Buffer);
        <<$\t, Rest/binary>> ->
            pp_value(Rest, State, Prev, Depth, Buffer);
        <<$\r, Rest/binary>> ->
            pp_value(Rest, State, Prev, Depth, Buffer);
        <<$\n, Rest/binary>> ->
            pp_value(Rest, State, Prev, Depth, Buffer);
        <<$", Rest/bitstring>> ->
            case Prev =:= ${ orelse Prev =:= $[ of
                true ->
                    Newline = pp_newline(State#state.crlf, State#state.indent, Depth),
                    pp_string(Rest, State, Prev, Depth, <<$">>, [Newline | Buffer]);
                false ->
                    pp_string(Rest, State, Prev, Depth, <<$">>, Buffer)
            end;
        <<$,, Rest/bitstring>> ->
            Newline = pp_newline(State#state.crlf, State#state.indent, Depth),
            pp_value(Rest, State, $,, Depth, [Newline, $, | Buffer]);
        <<${, Rest/bitstring>> ->
            pp_object(Rest, State, Prev, Depth, Buffer);
        <<$}, Rest/bitstring>> ->
            case Prev =:= ${ of
                true ->
                    pp_value(Rest, State, $}, Depth - 1, [$} | Buffer]);
                false ->
                    Depth1 = Depth - 1,
                    Newline = pp_newline(State#state.crlf, State#state.indent, Depth1),
                    pp_value(Rest, State, $}, Depth1, [$}, Newline | Buffer])
            end;
        <<$[, Rest/bitstring>> ->
            pp_array(Rest, State, Prev, Depth, Buffer);
        <<$], Rest/bitstring>> ->
            case Prev =:= $[ of
                true ->
                    pp_value(Rest, State, $], Depth - 1, [$] | Buffer]);
                false ->
                    Depth1 = Depth - 1,
                    Newline = pp_newline(State#state.crlf, State#state.indent, Depth1),
                    pp_value(Rest, State, $], Depth1, [$], Newline | Buffer])
            end;
        <<Rest/bitstring>> ->
            pp_number(Rest, State, Prev, Depth, <<>>, Buffer)
    end.

pp_string(Data, State, Prev, Depth, String, Buffer) ->
    case Data of
        <<$\\, $", Rest/binary>> ->
            pp_string(Rest, State, Prev, Depth, <<String/bitstring, $\\, $">>, Buffer);
        <<$", Rest/binary>> ->
            pp_value(Rest, State, Prev, Depth, [<<String/bitstring, $">> | Buffer]);
        <<H, Rest/bitstring>> ->
            pp_string(Rest, State, Prev, Depth, <<String/bitstring, H>>, Buffer)
    end.

pp_number(Data, State, Prev, Depth, Value, Buffer) ->
    case Data of
        <<$\s, Rest/binary>> ->
            pp_value(Rest, State, Prev, Depth, [Value | Buffer]);
        <<$\t, Rest/binary>> ->
            pp_value(Rest, State, Prev, Depth, [Value | Buffer]);
        <<$\r, Rest/binary>> ->
            pp_value(Rest, State, Prev, Depth, [Value | Buffer]);
        <<$\n, Rest/binary>> ->
            pp_value(Rest, State, Prev, Depth, [Value | Buffer]);
        <<$:, Rest/binary>> ->
            pp_value(Rest, State, $:, Depth, [Value, State#state.spaces, $: | Buffer]);
        <<$,, Rest/binary>> ->
            Newline = pp_newline(State#state.crlf, State#state.indent, Depth),
            pp_value(Rest, State, $,, Depth, [Newline, $,, Value | Buffer]);
        <<$], Rest/bitstring>> ->
            case Prev =:= $[ of
                true ->
                    pp_value(Rest, State, $], Depth - 1, [$], Value | Buffer]);
                false ->
                    Depth1 = Depth - 1,
                    Newline = pp_newline(State#state.crlf, State#state.indent, Depth1),
                    pp_value(Rest, State, $], Depth1, [$], Newline, Value | Buffer])
            end;
        <<$}, Rest/bitstring>> ->
            case Prev =:= ${ of
                true ->
                    pp_value(Rest, State, $}, Depth - 1, [$}, Value | Buffer]);
                false ->
                    Depth1 = Depth - 1,
                    Newline = pp_newline(State#state.crlf, State#state.indent, Depth1),
                    pp_value(Rest, State, $}, Depth1, [$}, Newline, Value | Buffer])
            end;
        <<H, Rest/bitstring>> ->
            pp_number(Rest, State, Prev, Depth, <<Value/bitstring, H>>, Buffer);
        <<>> ->
            lists:reverse([Value | Buffer])
    end.

pp_object(Data, State, Prev, Depth, Buffer) ->
    case Buffer =:= [] of
        true ->
            pp_value(Data, State, ${, Depth + 1, [${ | Buffer]);
        false ->
            case Prev =:= $: orelse Prev =:= $, of
                true ->
                    pp_value(Data, State, ${, Depth + 1, [${ | Buffer]);
                false ->
                    Newline = pp_newline(State#state.crlf, State#state.indent, Depth),
                    pp_value(Data, State, ${, Depth + 1, [${, Newline | Buffer])
            end
    end.

pp_array(Data, State, Prev, Depth, Buffer) ->
    case Buffer =:= [] of
        true ->
            pp_value(Data, State, $[, Depth + 1, [$[ | Buffer]);
        false ->
            case Prev =:= $: orelse Prev =:= $, of
                true ->
                    pp_value(Data, State, $[, Depth + 1, [$[ | Buffer]);
                false ->
                    Newline = pp_newline(State#state.crlf, State#state.indent, Depth),
                    pp_value(Data, State, $[, Depth + 1, [$[, Newline | Buffer])
            end
    end.

pp_newline(Crlf, Indent, Depth) ->
    [Crlf, binary:copy(Indent, Depth)].

%%%=====================================================================
%%% Tests
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
