%%%---------------------------------------------------------------------
%%% @copyright 2023 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc JSON parser without any options for better performance.
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
-module(euneus_decoder_smart).

% NOTE: Inlining escapeu_last/5 reduces the memory consumption,
%       but huge increases the deviation and reduces the IPS.
%       Tested using the "UTF-8 escaped" data.
% -compile({ inline, escapeu_last/5 }).
-compile({ inline, string/5 }).
-compile({ inline, string/6 }).
-compile({ inline, number/5 }).
-compile({ inline, number_exp_cont/6 }).
-compile({ inline, number_exp_sign/5 }).
-compile({ inline, number_exp_sign/6 }).
-compile({ inline, number_exp_copy/5 }).
-compile({ inline, number_zero/5 }).
-compile({ inline, escapeu_1/7 }).
-compile({ inline, escapeu_2/7 }).
-compile({ inline, chars_to_integer/2 }).
-compile({ inline, chars_to_integer/3 }).
-compile({ inline, chars_to_integer/4 }).
-compile({ inline, try_parse_integer/6 }).
-compile({ inline, try_parse_float/6 }).
-compile({ inline, object/5 }).
-compile({ inline, array/5 }).
-compile({ inline, empty_array/4 }).
-compile({ inline, escape/5 }).
-compile({ inline, continue/5 }).

-dialyzer({ no_return, throw_token/5 }).
-dialyzer( no_improper_lists ).

%% API functions

-export([ decode/1 ]).

%% Types

-type input() :: euneus_decoder:input().
-type result() :: euneus_decoder:result().

%% Macros

% NOTE: We use integers instead of atoms to take advantage of
%       the jump table optimization.
-define(terminate, 0).
-define(array, 1).
-define(key, 2).
-define(object, 3).

-define(is_number(X), X >= $0, X =< $9).

-define(NON_PRINTABLE_LAST, 31).
-define(ONE_BYTE_LAST, 127).
-define(TWO_BYTE_LAST, 2_047).
-define(THREE_BYTE_LAST, 65_535).

%%%=====================================================================
%%% API functions
%%%=====================================================================

-spec decode(input()) -> result().

decode(Data) when is_binary(Data) ->
    try
        {ok, value(Data, Data, 0, [?terminate])}
    catch
        throw:{eof, _Input, _Pos, _Buffer} ->
            {error, unexpected_end_of_input};
        throw:{{byte, Byte}, _Rest, _Input, Pos, _Buffer} ->
            Hex = <<"0x"/utf8,(integer_to_binary(Byte, 16))/binary>>,
            {error, {unexpected_byte, Hex, Pos}};
        throw:{{token, Token}, _Rest, _Input, Pos, _Buffer} ->
            {error, {unexpected_sequence, Token, Pos}};
        throw:{invalid_option, Key} ->
            {error, {invalid_option, Key}};
        throw:Reason ->
            {error, Reason};
        Class:Reason:Stacktrace ->
            erlang:raise(Class, Reason, Stacktrace)
    end;
decode(Data) when is_list(Data) ->
    decode(iolist_to_binary(Data)).

key(Data, Input, Pos, Buffer) ->
    case Data of
        <<$\s/integer, Rest/bitstring>> ->
            key(Rest, Input, Pos + 1, Buffer);
        <<$\t/integer, Rest/bitstring>> ->
            key(Rest, Input, Pos + 1, Buffer);
        <<$\r/integer, Rest/bitstring>> ->
            key(Rest, Input, Pos + 1, Buffer);
        <<$\n/integer, Rest/bitstring>> ->
            key(Rest, Input, Pos + 1, Buffer);
        <<$"/integer, Rest/bitstring>> ->
            string(Rest, Input, Pos + 1, [?key | Buffer], 0);
        <<$}/integer, Rest/bitstring>> ->
            case Buffer of
                [[] | Buffer1] ->
                    continue(Rest, Input, Pos + 1, Buffer1, #{});
                [_|_] ->
                    throw_byte(Data, Input, Pos, Buffer)
            end;
        <<_/integer, _/bitstring>> ->
            throw_byte(Data, Input, Pos, Buffer);
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

key(Data, Input, Pos, Buffer, Value) ->
    case Data of
        <<$\s/integer, Rest/bitstring>> ->
            key(Rest, Input, Pos + 1, Buffer, Value);
        <<$\t/integer, Rest/bitstring>> ->
            key(Rest, Input, Pos + 1, Buffer, Value);
        <<$\r/integer, Rest/bitstring>> ->
            key(Rest, Input, Pos + 1, Buffer, Value);
        <<$\n/integer, Rest/bitstring>> ->
            key(Rest, Input, Pos + 1, Buffer, Value);
        <<$:/integer, Rest/bitstring>> ->
            value(Rest, Input, Pos + 1, [?object, Value | Buffer]);
        <<_/integer, _/bitstring>> ->
            throw_byte(Data, Input, Pos, Buffer);
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

value(Data, Input, Pos, Buffer) ->
    case Data of
        <<$\s/integer, Rest/bitstring>> ->
            value(Rest, Input, Pos + 1, Buffer);
        <<$\t/integer, Rest/bitstring>> ->
            value(Rest, Input, Pos + 1, Buffer);
        <<$\r/integer, Rest/bitstring>> ->
            value(Rest, Input, Pos + 1, Buffer);
        <<$\n/integer, Rest/bitstring>> ->
            value(Rest, Input, Pos + 1, Buffer);
        <<$"/integer, Rest/bitstring>> ->
            case Rest of
                << Y4/integer, Y3/integer, Y2/integer, Y1/integer, $-/integer
                 , M2/integer, M1/integer, $-/integer
                 , D2/integer, D1/integer
                 , $T/integer
                 , H2/integer, H1/integer, $:/integer
                 , Min2/integer, Min1/integer, $:/integer
                 , S2/integer, S1/integer
                 , Rest1/bitstring >>
                 when ?is_number(Y4), ?is_number(Y3), ?is_number(Y2), ?is_number(Y1)
                    , ?is_number(M2), ?is_number(M1)
                    , ?is_number(D2), ?is_number(D1)
                    , ?is_number(H2), ?is_number(H1)
                    , ?is_number(Min2), ?is_number(Min1)
                    , ?is_number(S2), ?is_number(S1) ->
                    case Rest1 of
                        << $Z/integer
                         , $"
                         , Rest2/bitstring >> ->
                            Date = { chars_to_integer(Y4, Y3, Y2, Y1)
                                   , chars_to_integer(M2, M1)
                                   , chars_to_integer(D2, D1) },
                            Time = { chars_to_integer(H2, H1)
                                   , chars_to_integer(Min2, Min1)
                                   , chars_to_integer(S2, S1) },
                            Value = {Date, Time},
                            continue(Rest2, Input, Pos + 22, Buffer, Value);
                        << $./integer
                         , MSec3/integer, MSec2/integer, MSec1/integer
                         , $Z/integer
                         , $"
                         , Rest2/bitstring >>
                         when ?is_number(MSec3)
                            , ?is_number(MSec2)
                            , ?is_number(MSec1) ->
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
                            Value = { Seconds div 1000000
                                    , Seconds rem 1000000
                                    , MilliSeconds * 1000 },
                            continue(Rest2, Input, Pos + 26, Buffer, Value)
                    end;
                <<_/bitstring>> ->
                    string(Rest, Input, Pos + 1, Buffer, 0)
            end;
        <<$0/integer, Rest/bitstring>> ->
            number_zero(Rest, Input, Pos, Buffer, 1);
        <<$1/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 1);
        <<$2/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 1);
        <<$3/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 1);
        <<$4/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 1);
        <<$5/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 1);
        <<$6/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 1);
        <<$7/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 1);
        <<$8/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 1);
        <<$9/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 1);
        <<$-/integer, Rest/bitstring>> ->
            number_minus(Rest, Input, Pos, Buffer);
        <<$[/integer, Rest/bitstring>> ->
            value(Rest, Input, Pos + 1, [?array, [] | Buffer]);
        <<$]/integer, Rest/bitstring>> ->
            empty_array(Rest, Input, Pos + 1, Buffer);
        <<${/integer, Rest/bitstring>> ->
            key(Rest, Input, Pos + 1, [[] | Buffer]);
        <<"false", Rest/bitstring>> ->
            continue(Rest, Input, Pos + 5, Buffer, false);
        <<"null", Rest/bitstring>> ->
            continue(Rest, Input, Pos + 4, Buffer, undefined);
        <<"true", Rest/bitstring>> ->
            continue(Rest, Input, Pos + 4, Buffer, true);
        <<_/integer, Rest/bitstring>> ->
            throw_byte(Rest, Input, Pos, Buffer);
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

chars_to_integer(N2, N1) ->
    ((N2 - $0) * 10) + (N1 - $0).

chars_to_integer(N3, N2, N1) ->
    ((N3 - $0) * 100) + ((N2 - $0) * 10) + (N1 - $0).

chars_to_integer(N4, N3, N2, N1) ->
    ((N4 - $0) * 1000) + ((N3 - $0) * 100) + ((N2 - $0) * 10) + (N1 - $0).

string(Data, Input, Pos, Buffer, Len) ->
    case Data of
        <<$"/integer, Rest/bitstring>> ->
            String = binary_part(Input, Pos, Len),
            continue(Rest, Input, Pos + Len + 1, Buffer, String);
        <<$\\/integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            escape(Rest, Input, Pos + Len, Buffer, Part);
        <<X/integer, _/bitstring>> when X =< ?NON_PRINTABLE_LAST ->
            throw_byte(Data, Input, Pos, Buffer);
        <<X/integer, Rest/bitstring>> when X =< ?ONE_BYTE_LAST ->
            string(Rest, Input, Pos, Buffer, Len + 1);
        <<Char/utf8, Rest/bitstring>> when Char =< ?TWO_BYTE_LAST ->
            string(Rest, Input, Pos, Buffer, Len + 2);
        <<Char/utf8, Rest/bitstring>> when Char =< ?THREE_BYTE_LAST ->
            string(Rest, Input, Pos, Buffer, Len + 3);
        <<_Char/utf8, Rest/bitstring>> ->
            string(Rest, Input, Pos, Buffer, Len + 4);
        <<_/integer, _/bitstring>> ->
            throw_byte(Data, Input, Pos, Buffer);
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

string(Data, Input, Pos, Buffer, Acc, Len) ->
    case Data of
        <<$"/integer, Rest/bitstring>> ->
            Last = binary_part(Input, Pos, Len),
            String = iolist_to_binary([Acc | Last]),
            continue(Rest, Input, Pos + Len + 1, Buffer, String);
        <<$\\/integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            escape(Rest, Input, Pos + Len, Buffer, [Acc | Part]);
        <<X/integer, _/bitstring>> when X =< ?NON_PRINTABLE_LAST ->
            throw_byte(Data, Input, Pos, Buffer);
        <<X/integer, Rest/bitstring>> when X =< ?ONE_BYTE_LAST ->
            string(Rest, Input, Pos, Buffer, Acc, Len + 1);
        <<Char/utf8, Rest/bitstring>> when Char =< ?TWO_BYTE_LAST ->
            string(Rest, Input, Pos, Buffer, Acc, Len + 2);
        <<Char/utf8, Rest/bitstring>> when Char =< ?THREE_BYTE_LAST ->
            string(Rest, Input, Pos, Buffer, Acc, Len + 3);
        <<_Char/utf8, Rest/bitstring>> ->
            string(Rest, Input, Pos, Buffer, Acc, Len + 4);
        <<_/integer, _/bitstring>> ->
            throw_byte(Data, Input, Pos, Buffer);
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

number(Data, Input, Pos, Buffer, Len) ->
    case Data of
        <<$0/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, Len + 1);
        <<$1/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, Len + 1);
        <<$2/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, Len + 1);
        <<$3/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, Len + 1);
        <<$4/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, Len + 1);
        <<$5/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, Len + 1);
        <<$6/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, Len + 1);
        <<$7/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, Len + 1);
        <<$8/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, Len + 1);
        <<$9/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, Len + 1);
        <<$./integer, Rest/bitstring>> ->
            number_frac(Rest, Input, Pos, Buffer, Len + 1);
        <<$e/integer, Rest/bitstring>> ->
            Prefix = binary_part(Input, Pos, Len),
            number_exp_copy(Rest, Input, Pos + Len + 1, Buffer, Prefix);
        <<$E/integer, Rest/bitstring>> ->
            Prefix = binary_part(Input, Pos, Len),
            number_exp_copy(Rest, Input, Pos + Len + 1, Buffer, Prefix);
        <<Rest/bitstring>> ->
            Token = binary_part(Input, Pos, Len),
            Int = try_parse_integer(Token, Token, Rest, Input, Pos, Buffer),
            continue(Rest, Input, Pos + Len, Buffer, Int)
    end.

number_exp(Data, Input, Pos, Buffer, Len) ->
    case Data of
        <<$0/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$1/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$2/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$3/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$4/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$5/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$6/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$7/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$8/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$9/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$-/integer, Rest/bitstring>> ->
            number_exp_sign(Rest, Input, Pos, Buffer, Len + 1);
        <<$+/integer, Rest/bitstring>> ->
            number_exp_sign(Rest, Input, Pos, Buffer, Len + 1);
        <<_/integer, _/bitstring>> ->
            throw_byte(Data, Input, Pos + Len, Buffer);
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

number_exp_cont(Data, Input, Pos, Buffer, Len) ->
    case Data of
        <<$0/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$1/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$2/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$3/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$4/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$5/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$6/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$7/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$8/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$9/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<Rest/bitstring>> ->
            Token = binary_part(Input, Pos, Len),
            Float = try_parse_float(Token, Token, Rest, Input, Pos, Buffer),
            continue(Rest, Input, Pos + Len, Buffer, Float)
    end.

number_exp_cont(Data, Input, Pos, Buffer, Prefix, Len) ->
    case Data of
        <<$0/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$1/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$2/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$3/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$4/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$5/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$6/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$7/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$8/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$9/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<Rest/bitstring>> ->
            Suffix = binary_part(Input, Pos, Len),
            String = <<Prefix/binary,".0e",Suffix/binary>>,
            PrefixSize = byte_size(Prefix),
            InitialPos = Pos - PrefixSize - 1,
            FinalPos = Pos + Len,
            Token = binary_part(Input, InitialPos, PrefixSize + Len + 1),
            Float = try_parse_float(String, Token, Rest, Input, InitialPos, Buffer),
            continue(Rest, Input, FinalPos, Buffer, Float)
    end.

number_exp_copy(Data, Input, Pos, Buffer, Prefix) ->
    case Data of
        <<$0/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, 1);
        <<$1/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, 1);
        <<$2/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, 1);
        <<$3/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, 1);
        <<$4/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, 1);
        <<$5/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, 1);
        <<$6/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, 1);
        <<$7/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, 1);
        <<$8/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, 1);
        <<$9/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, 1);
        <<$-/integer, Rest/bitstring>> ->
            number_exp_sign(Rest, Input, Pos, Buffer, Prefix, 1);
        <<$+/integer, Rest/bitstring>> ->
            number_exp_sign(Rest, Input, Pos, Buffer, Prefix, 1);
        <<_/integer, _/bitstring>> ->
            throw_byte(Data, Input, Pos, Buffer);
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

number_exp_sign(Data, Input, Pos, Buffer, Len) ->
    case Data of
        <<$0/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$1/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$2/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$3/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$4/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$5/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$6/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$7/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$8/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$9/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<_/integer, _/bitstring>> ->
            throw_byte(Data, Input, Pos + Len, Buffer);
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

number_exp_sign(Data, Input, Pos, Buffer, Prefix, Len) ->
    case Data of
        <<$0/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$1/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$2/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$3/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$4/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$5/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$6/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$7/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$8/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<$9/integer, Rest/bitstring>> ->
            number_exp_cont(Rest, Input, Pos, Buffer, Prefix, Len + 1);
        <<_/integer, _/bitstring>> ->
            throw_byte(Data, Input, Pos + Len, Buffer);
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

number_frac(Data, Input, Pos, Buffer, Len) ->
    case Data of
        <<$0/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$1/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$2/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$3/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$4/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$5/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$6/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$7/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$8/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$9/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<_/integer, _/bitstring>> ->
            throw_byte(Data, Input, Pos + Len, Buffer);
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

number_frac_cont(Data, Input, Pos, Buffer, Len) ->
    case Data of
        <<$0/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$1/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$2/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$3/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$4/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$5/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$6/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$7/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$8/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$9/integer, Rest/bitstring>> ->
            number_frac_cont(Rest, Input, Pos, Buffer, Len + 1);
        <<$e/integer, Rest/bitstring>> ->
            number_exp(Rest, Input, Pos, Buffer, Len + 1);
        <<$E/integer, Rest/bitstring>> ->
            number_exp(Rest, Input, Pos, Buffer, Len + 1);
        <<Rest/bitstring>> ->
            Token = binary_part(Input, Pos, Len),
            Float = try_parse_float(Token, Token, Rest, Input, Pos, Buffer),
            continue(Rest, Input, Pos + Len, Buffer, Float)
    end.

number_minus(Data, Input, Pos, Buffer) ->
    case Data of
        <<$0/integer, Rest/bitstring>> ->
            number_zero(Rest, Input, Pos, Buffer, 2);
        <<$1/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 2);
        <<$2/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 2);
        <<$3/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 2);
        <<$4/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 2);
        <<$5/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 2);
        <<$6/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 2);
        <<$7/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 2);
        <<$8/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 2);
        <<$9/integer, Rest/bitstring>> ->
            number(Rest, Input, Pos, Buffer, 2);
        <<_/integer, _/bitstring>> ->
            throw_byte(Data, Input, Pos + 1, Buffer);
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

number_zero(Data, Input, Pos, Buffer, Len) ->
    case Data of
        <<$./integer, Rest/bitstring>> ->
            number_frac(Rest, Input, Pos, Buffer, Len + 1);
        <<$e/integer, Rest/bitstring>> ->
            number_exp_copy(Rest, Input, Pos + Len + 1, Buffer, <<"0">>);
        <<$E/integer, Rest/bitstring>> ->
            number_exp_copy(Rest, Input, Pos + Len + 1, Buffer, <<"0">>);
        <<Rest/bitstring>> ->
            continue(Rest, Input, Pos + Len, Buffer, 0)
    end.

try_parse_integer(Bin, Token, Rest, Input, Pos, Buffer) ->
    try
        binary_to_integer(Bin)
    catch
        error:badarg ->
            throw_token(Token, Rest, Input, Pos, Buffer)
    end.

try_parse_float(Bin, Token, Rest, Input, Pos, Buffer) ->
    try
        binary_to_float(Bin)
    catch
        error:badarg ->
            throw_token(Token, Rest, Input, Pos, Buffer)
    end.

object(Data, Input, Pos, Buffer, Value) ->
    case Data of
        <<$\s/integer, Rest/bitstring>> ->
            object(Rest, Input, Pos + 1, Buffer, Value);
        <<$\t/integer, Rest/bitstring>> ->
            object(Rest, Input, Pos + 1, Buffer, Value);
        <<$\r/integer, Rest/bitstring>> ->
            object(Rest, Input, Pos + 1, Buffer, Value);
        <<$\n/integer, Rest/bitstring>> ->
            object(Rest, Input, Pos + 1, Buffer, Value);
        <<$,/integer, Rest/bitstring>> ->
            [Key, Acc | Buffer2] = Buffer,
            Acc2 = [{Key, Value} | Acc],
            key(Rest, Input, Pos + 1, [Acc2 | Buffer2]);
        <<$}/integer, Rest/bitstring>> ->
            [Key, Acc2 | Buffer2] = Buffer,
            Final = [{Key, Value} | Acc2],
            Map = maps:from_list(Final),
            continue(Rest, Input, Pos + 1, Buffer2, Map);
        <<_/integer, _/bitstring>> ->
            throw_byte(Data, Input, Pos, Buffer);
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

array(Data, Input, Pos, Buffer, Value) ->
    case Data of
        <<$\t/integer, Rest/bitstring>> ->
            array(Rest, Input, Pos + 1, Buffer, Value);
        <<$\n/integer, Rest/bitstring>> ->
            array(Rest, Input, Pos + 1, Buffer, Value);
        <<$\r/integer, Rest/bitstring>> ->
            array(Rest, Input, Pos + 1, Buffer, Value);
        <<$\s/integer, Rest/bitstring>> ->
            array(Rest, Input, Pos + 1, Buffer, Value);
        <<$,/integer, Rest/bitstring>> ->
            [Acc | Buffer2] = Buffer,
            value(Rest, Input, Pos + 1, [?array, [Value | Acc] | Buffer2]);
        <<$]/integer, Rest/bitstring>> ->
            [Acc | Buffer2] = Buffer,
            List = lists:reverse(Acc, [Value]),
            continue(Rest, Input, Pos + 1, Buffer2, List);
        <<_/integer, _/bitstring>> ->
            throw_byte(Data, Input, Pos, Buffer);
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

empty_array(Rest, Input, Pos, Buffer) ->
    case Buffer of
        [?array, [] | Buffer1] ->
            continue(Rest, Input, Pos, Buffer1, []);
        [_|_] ->
            throw_byte(Rest, Input, Pos - 1, Buffer)
    end.

continue(Rest, Input, Pos, [Continue | Buffer], Value) ->
    case Continue of
        ?terminate ->
            terminate(Rest, Input, Pos, Buffer, Value);
        ?array ->
            array(Rest, Input, Pos, Buffer, Value);
        ?key ->
            key(Rest, Input, Pos, Buffer, Value);
        ?object ->
            object(Rest, Input, Pos, Buffer, Value)
    end.

terminate(Data, Input, Pos, Buffer, Value) ->
    case Data of
        <<$\s/integer, Rest/bitstring>> ->
            terminate(Rest, Input, Pos + 1, Buffer, Value);
        <<$\t/integer, Rest/bitstring>> ->
            terminate(Rest, Input, Pos + 1, Buffer, Value);
        <<$\r/integer, Rest/bitstring>> ->
            terminate(Rest, Input, Pos + 1, Buffer, Value);
        <<$\n/integer, Rest/bitstring>> ->
            terminate(Rest, Input, Pos + 1, Buffer, Value);
        <<>> ->
            Value;
        <<_/bitstring>> ->
            throw_byte(Data, Input, Pos, Buffer)
    end.

escape(Data, Input, Pos, Buffer, Acc) ->
    case Data of
        <<$\"/integer, Rest/bitstring>> ->
            string(Rest, Input, Pos + 2, Buffer, [Acc, $\"], 0);
        <<$//integer, Rest/bitstring>> ->
            string(Rest, Input, Pos + 2, Buffer, [Acc, $/], 0);
        <<$\\/integer, Rest/bitstring>> ->
            string(Rest, Input, Pos + 2, Buffer, [Acc, $\\], 0);
        <<$b/integer, Rest/bitstring>> ->
            string(Rest, Input, Pos + 2, Buffer, [Acc, $\b], 0);
        <<$f/integer, Rest/bitstring>> ->
            string(Rest, Input, Pos + 2, Buffer, [Acc, $\f], 0);
        <<$n/integer, Rest/bitstring>> ->
            string(Rest, Input, Pos + 2, Buffer, [Acc, $\n], 0);
        <<$r/integer, Rest/bitstring>> ->
            string(Rest, Input, Pos + 2, Buffer, [Acc, $\r], 0);
        <<$t/integer, Rest/bitstring>> ->
            string(Rest, Input, Pos + 2, Buffer, [Acc, $\t], 0);
        <<$u/integer, Rest/bitstring>> ->
            escapeu(Rest, Input, Pos, Buffer, Acc);
        <<_/integer, _/bitstring>> ->
            throw_byte(Data, Input, Pos + 1, Buffer);
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

escapeu(Data, Input, Pos, Buffer, Acc) ->
    case Data of
        <<Int1:16/integer,Int2:16/integer, Rest/bitstring>> ->
            Last = escapeu_last(Int2, Rest, Input, Pos, Buffer),
            case Int1 of
                12336 ->
                    string(Rest, Input, Pos + 6, Buffer,
                           begin
                               _@1 = Acc,
                               _@2 = 0,
                               _@3 = Last,
                               case _@3 =< 127 of
                                   false ->
                                       _@4 = 6 bsl 5 + (_@2 bsl 2) + (_@3 bsr 6),
                                       _@5 = 2 bsl 6 + _@3 band 63,
                                       [_@1, _@4, _@5];
                                   true ->
                                       [_@1, _@3]
                               end
                           end,
                           0);
                12337 -> escapeu_1(Rest, Input, Pos, Buffer, Acc, Last, 1);
                12338 -> escapeu_1(Rest, Input, Pos, Buffer, Acc, Last, 2);
                12339 -> escapeu_1(Rest, Input, Pos, Buffer, Acc, Last, 3);
                12340 -> escapeu_1(Rest, Input, Pos, Buffer, Acc, Last, 4);
                12341 -> escapeu_1(Rest, Input, Pos, Buffer, Acc, Last, 5);
                12342 -> escapeu_1(Rest, Input, Pos, Buffer, Acc, Last, 6);
                12343 -> escapeu_1(Rest, Input, Pos, Buffer, Acc, Last, 7);
                12344 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 8);
                12345 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 9);
                12353 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 10);
                12354 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 11);
                12355 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 12);
                12356 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 13);
                12357 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 14);
                12358 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 15);
                12385 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 10);
                12386 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 11);
                12387 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 12);
                12388 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 13);
                12389 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 14);
                12390 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 15);
                12592 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 16);
                12593 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 17);
                12594 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 18);
                12595 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 19);
                12596 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 20);
                12597 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 21);
                12598 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 22);
                12599 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 23);
                12600 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 24);
                12601 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 25);
                12609 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 26);
                12610 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 27);
                12611 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 28);
                12612 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 29);
                12613 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 30);
                12614 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 31);
                12641 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 26);
                12642 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 27);
                12643 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 28);
                12644 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 29);
                12645 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 30);
                12646 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 31);
                12848 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 32);
                12849 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 33);
                12850 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 34);
                12851 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 35);
                12852 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 36);
                12853 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 37);
                12854 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 38);
                12855 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 39);
                12856 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 40);
                12857 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 41);
                12865 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 42);
                12866 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 43);
                12867 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 44);
                12868 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 45);
                12869 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 46);
                12870 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 47);
                12897 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 42);
                12898 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 43);
                12899 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 44);
                12900 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 45);
                12901 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 46);
                12902 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 47);
                13104 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 48);
                13105 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 49);
                13106 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 50);
                13107 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 51);
                13108 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 52);
                13109 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 53);
                13110 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 54);
                13111 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 55);
                13112 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 56);
                13113 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 57);
                13121 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 58);
                13122 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 59);
                13123 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 60);
                13124 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 61);
                13125 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 62);
                13126 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 63);
                13153 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 58);
                13154 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 59);
                13155 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 60);
                13156 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 61);
                13157 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 62);
                13158 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 63);
                13360 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 64);
                13361 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 65);
                13362 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 66);
                13363 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 67);
                13364 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 68);
                13365 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 69);
                13366 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 70);
                13367 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 71);
                13368 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 72);
                13369 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 73);
                13377 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 74);
                13378 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 75);
                13379 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 76);
                13380 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 77);
                13381 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 78);
                13382 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 79);
                13409 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 74);
                13410 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 75);
                13411 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 76);
                13412 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 77);
                13413 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 78);
                13414 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 79);
                13616 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 80);
                13617 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 81);
                13618 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 82);
                13619 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 83);
                13620 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 84);
                13621 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 85);
                13622 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 86);
                13623 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 87);
                13624 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 88);
                13625 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 89);
                13633 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 90);
                13634 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 91);
                13635 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 92);
                13636 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 93);
                13637 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 94);
                13638 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 95);
                13665 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 90);
                13666 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 91);
                13667 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 92);
                13668 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 93);
                13669 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 94);
                13670 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 95);
                13872 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 96);
                13873 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 97);
                13874 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 98);
                13875 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 99);
                13876 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 100);
                13877 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 101);
                13878 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 102);
                13879 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 103);
                13880 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 104);
                13881 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 105);
                13889 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 106);
                13890 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 107);
                13891 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 108);
                13892 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 109);
                13893 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 110);
                13894 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 111);
                13921 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 106);
                13922 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 107);
                13923 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 108);
                13924 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 109);
                13925 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 110);
                13926 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 111);
                14128 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 112);
                14129 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 113);
                14130 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 114);
                14131 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 115);
                14132 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 116);
                14133 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 117);
                14134 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 118);
                14135 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 119);
                14136 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 120);
                14137 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 121);
                14145 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 122);
                14146 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 123);
                14147 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 124);
                14148 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 125);
                14149 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 126);
                14150 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 127);
                14177 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 122);
                14178 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 123);
                14179 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 124);
                14180 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 125);
                14181 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 126);
                14182 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 127);
                14384 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 128);
                14385 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 129);
                14386 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 130);
                14387 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 131);
                14388 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 132);
                14389 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 133);
                14390 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 134);
                14391 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 135);
                14392 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 136);
                14393 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 137);
                14401 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 138);
                14402 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 139);
                14403 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 140);
                14404 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 141);
                14405 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 142);
                14406 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 143);
                14433 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 138);
                14434 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 139);
                14435 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 140);
                14436 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 141);
                14437 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 142);
                14438 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 143);
                14640 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 144);
                14641 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 145);
                14642 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 146);
                14643 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 147);
                14644 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 148);
                14645 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 149);
                14646 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 150);
                14647 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 151);
                14648 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 152);
                14649 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 153);
                14657 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 154);
                14658 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 155);
                14659 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 156);
                14660 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 157);
                14661 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 158);
                14662 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 159);
                14689 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 154);
                14690 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 155);
                14691 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 156);
                14692 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 157);
                14693 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 158);
                14694 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 159);
                16688 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 160);
                16689 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 161);
                16690 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 162);
                16691 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 163);
                16692 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 164);
                16693 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 165);
                16694 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 166);
                16695 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 167);
                16696 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 168);
                16697 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 169);
                16705 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 170);
                16706 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 171);
                16707 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 172);
                16708 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 173);
                16709 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 174);
                16710 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 175);
                16737 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 170);
                16738 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 171);
                16739 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 172);
                16740 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 173);
                16741 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 174);
                16742 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 175);
                16944 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 176);
                16945 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 177);
                16946 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 178);
                16947 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 179);
                16948 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 180);
                16949 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 181);
                16950 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 182);
                16951 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 183);
                16952 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 184);
                16953 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 185);
                16961 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 186);
                16962 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 187);
                16963 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 188);
                16964 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 189);
                16965 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 190);
                16966 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 191);
                16993 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 186);
                16994 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 187);
                16995 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 188);
                16996 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 189);
                16997 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 190);
                16998 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 191);
                17200 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 192);
                17201 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 193);
                17202 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 194);
                17203 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 195);
                17204 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 196);
                17205 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 197);
                17206 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 198);
                17207 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 199);
                17208 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 200);
                17209 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 201);
                17217 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 202);
                17218 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 203);
                17219 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 204);
                17220 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 205);
                17221 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 206);
                17222 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 207);
                17249 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 202);
                17250 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 203);
                17251 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 204);
                17252 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 205);
                17253 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 206);
                17254 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 207);
                17456 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 208);
                17457 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 209);
                17458 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 210);
                17459 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 211);
                17460 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 212);
                17461 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 213);
                17462 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 214);
                17463 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 215);
                17464 ->
                    escape_surrogate(Rest, Input, Pos, Buffer, Acc,
                                     begin
                                         _@1757 = 216,
                                         _@1758 = Last,
                                         65536
                                         +
                                         (_@1757 band 3 bsl 8 + _@1758 bsl 10)
                                     end);
                17465 ->
                    escape_surrogate(Rest, Input, Pos, Buffer, Acc,
                                     begin
                                         _@1759 = 217,
                                         _@1760 = Last,
                                         65536
                                         +
                                         (_@1759 band 3 bsl 8 + _@1760 bsl 10)
                                     end);
                17473 ->
                    escape_surrogate(Rest, Input, Pos, Buffer, Acc,
                                     begin
                                         _@1761 = 218,
                                         _@1762 = Last,
                                         65536
                                         +
                                         (_@1761 band 3 bsl 8 + _@1762 bsl 10)
                                     end);
                17474 ->
                    escape_surrogate(Rest, Input, Pos, Buffer, Acc,
                                     begin
                                         _@1763 = 219,
                                         _@1764 = Last,
                                         65536
                                         +
                                         (_@1763 band 3 bsl 8 + _@1764 bsl 10)
                                     end);
                17505 ->
                    escape_surrogate(Rest, Input, Pos, Buffer, Acc,
                                     begin
                                         _@1765 = 218,
                                         _@1766 = Last,
                                         65536
                                         +
                                         (_@1765 band 3 bsl 8 + _@1766 bsl 10)
                                     end);
                17506 ->
                    escape_surrogate(Rest, Input, Pos, Buffer, Acc,
                                     begin
                                         _@1767 = 219,
                                         _@1768 = Last,
                                         65536
                                         +
                                         (_@1767 band 3 bsl 8 + _@1768 bsl 10)
                                     end);
                17712 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 224);
                17713 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 225);
                17714 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 226);
                17715 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 227);
                17716 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 228);
                17717 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 229);
                17718 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 230);
                17719 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 231);
                17720 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 232);
                17721 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 233);
                17729 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 234);
                17730 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 235);
                17731 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 236);
                17732 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 237);
                17733 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 238);
                17734 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 239);
                17761 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 234);
                17762 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 235);
                17763 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 236);
                17764 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 237);
                17765 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 238);
                17766 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 239);
                17968 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 240);
                17969 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 241);
                17970 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 242);
                17971 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 243);
                17972 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 244);
                17973 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 245);
                17974 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 246);
                17975 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 247);
                17976 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 248);
                17977 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 249);
                17985 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 250);
                17986 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 251);
                17987 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 252);
                17988 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 253);
                17989 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 254);
                17990 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 255);
                18017 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 250);
                18018 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 251);
                18019 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 252);
                18020 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 253);
                18021 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 254);
                18022 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 255);
                24880 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 160);
                24881 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 161);
                24882 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 162);
                24883 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 163);
                24884 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 164);
                24885 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 165);
                24886 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 166);
                24887 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 167);
                24888 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 168);
                24889 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 169);
                24897 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 170);
                24898 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 171);
                24899 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 172);
                24900 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 173);
                24901 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 174);
                24902 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 175);
                24929 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 170);
                24930 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 171);
                24931 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 172);
                24932 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 173);
                24933 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 174);
                24934 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 175);
                25136 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 176);
                25137 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 177);
                25138 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 178);
                25139 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 179);
                25140 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 180);
                25141 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 181);
                25142 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 182);
                25143 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 183);
                25144 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 184);
                25145 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 185);
                25153 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 186);
                25154 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 187);
                25155 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 188);
                25156 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 189);
                25157 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 190);
                25158 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 191);
                25185 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 186);
                25186 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 187);
                25187 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 188);
                25188 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 189);
                25189 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 190);
                25190 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 191);
                25392 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 192);
                25393 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 193);
                25394 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 194);
                25395 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 195);
                25396 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 196);
                25397 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 197);
                25398 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 198);
                25399 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 199);
                25400 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 200);
                25401 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 201);
                25409 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 202);
                25410 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 203);
                25411 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 204);
                25412 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 205);
                25413 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 206);
                25414 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 207);
                25441 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 202);
                25442 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 203);
                25443 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 204);
                25444 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 205);
                25445 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 206);
                25446 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 207);
                25648 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 208);
                25649 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 209);
                25650 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 210);
                25651 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 211);
                25652 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 212);
                25653 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 213);
                25654 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 214);
                25655 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 215);
                25656 ->
                    escape_surrogate(Rest, Input, Pos, Buffer, Acc,
                                     begin
                                         _@2477 = 216,
                                         _@2478 = Last,
                                         65536
                                         +
                                         (_@2477 band 3 bsl 8 + _@2478 bsl 10)
                                     end);
                25657 ->
                    escape_surrogate(Rest, Input, Pos, Buffer, Acc,
                                     begin
                                         _@2479 = 217,
                                         _@2480 = Last,
                                         65536
                                         +
                                         (_@2479 band 3 bsl 8 + _@2480 bsl 10)
                                     end);
                25665 ->
                    escape_surrogate(Rest, Input, Pos, Buffer, Acc,
                                     begin
                                         _@2481 = 218,
                                         _@2482 = Last,
                                         65536
                                         +
                                         (_@2481 band 3 bsl 8 + _@2482 bsl 10)
                                     end);
                25666 ->
                    escape_surrogate(Rest, Input, Pos, Buffer, Acc,
                                     begin
                                         _@2483 = 219,
                                         _@2484 = Last,
                                         65536
                                         +
                                         (_@2483 band 3 bsl 8 + _@2484 bsl 10)
                                     end);
                25697 ->
                    escape_surrogate(Rest, Input, Pos, Buffer, Acc,
                                     begin
                                         _@2485 = 218,
                                         _@2486 = Last,
                                         65536
                                         +
                                         (_@2485 band 3 bsl 8 + _@2486 bsl 10)
                                     end);
                25698 ->
                    escape_surrogate(Rest, Input, Pos, Buffer, Acc,
                                     begin
                                         _@2487 = 219,
                                         _@2488 = Last,
                                         65536
                                         +
                                         (_@2487 band 3 bsl 8 + _@2488 bsl 10)
                                     end);
                25904 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 224);
                25905 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 225);
                25906 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 226);
                25907 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 227);
                25908 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 228);
                25909 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 229);
                25910 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 230);
                25911 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 231);
                25912 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 232);
                25913 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 233);
                25921 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 234);
                25922 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 235);
                25923 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 236);
                25924 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 237);
                25925 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 238);
                25926 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 239);
                25953 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 234);
                25954 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 235);
                25955 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 236);
                25956 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 237);
                25957 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 238);
                25958 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 239);
                26160 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 240);
                26161 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 241);
                26162 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 242);
                26163 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 243);
                26164 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 244);
                26165 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 245);
                26166 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 246);
                26167 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 247);
                26168 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 248);
                26169 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 249);
                26177 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 250);
                26178 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 251);
                26179 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 252);
                26180 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 253);
                26181 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 254);
                26182 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 255);
                26209 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 250);
                26210 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 251);
                26211 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 252);
                26212 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 253);
                26213 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 254);
                26214 -> escapeu_2(Rest, Input, Pos, Buffer, Acc, Last, 255);
                _ -> throw_token(6, Rest, Input, Pos, Buffer)
            end;
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

escapeu_last(Int, Rest, Input, Pos, Buffer) ->
    case Int of
        12336 -> 0; 12337 -> 1; 12338 -> 2; 12339 -> 3; 12340 -> 4; 12341 -> 5; 12342 -> 6;
        12343 -> 7; 12344 -> 8; 12345 -> 9; 12353 -> 10; 12354 -> 11; 12355 -> 12;
        12356 -> 13; 12357 -> 14; 12358 -> 15; 12385 -> 10; 12386 -> 11; 12387 -> 12;
        12388 -> 13; 12389 -> 14; 12390 -> 15; 12592 -> 16; 12593 -> 17; 12594 -> 18;
        12595 -> 19; 12596 -> 20; 12597 -> 21; 12598 -> 22; 12599 -> 23; 12600 -> 24;
        12601 -> 25; 12609 -> 26; 12610 -> 27; 12611 -> 28; 12612 -> 29; 12613 -> 30;
        12614 -> 31; 12641 -> 26; 12642 -> 27; 12643 -> 28; 12644 -> 29; 12645 -> 30;
        12646 -> 31; 12848 -> 32; 12849 -> 33; 12850 -> 34; 12851 -> 35; 12852 -> 36;
        12853 -> 37; 12854 -> 38; 12855 -> 39; 12856 -> 40; 12857 -> 41; 12865 -> 42;
        12866 -> 43; 12867 -> 44; 12868 -> 45; 12869 -> 46; 12870 -> 47; 12897 -> 42;
        12898 -> 43; 12899 -> 44; 12900 -> 45; 12901 -> 46; 12902 -> 47; 13104 -> 48;
        13105 -> 49; 13106 -> 50; 13107 -> 51; 13108 -> 52; 13109 -> 53; 13110 -> 54;
        13111 -> 55; 13112 -> 56; 13113 -> 57; 13121 -> 58; 13122 -> 59; 13123 -> 60;
        13124 -> 61; 13125 -> 62; 13126 -> 63; 13153 -> 58; 13154 -> 59; 13155 -> 60;
        13156 -> 61; 13157 -> 62; 13158 -> 63; 13360 -> 64; 13361 -> 65; 13362 -> 66;
        13363 -> 67; 13364 -> 68; 13365 -> 69; 13366 -> 70; 13367 -> 71; 13368 -> 72;
        13369 -> 73; 13377 -> 74; 13378 -> 75; 13379 -> 76; 13380 -> 77; 13381 -> 78;
        13382 -> 79; 13409 -> 74; 13410 -> 75; 13411 -> 76; 13412 -> 77; 13413 -> 78;
        13414 -> 79; 13616 -> 80; 13617 -> 81; 13618 -> 82; 13619 -> 83; 13620 -> 84;
        13621 -> 85; 13622 -> 86; 13623 -> 87; 13624 -> 88; 13625 -> 89; 13633 -> 90;
        13634 -> 91; 13635 -> 92; 13636 -> 93; 13637 -> 94; 13638 -> 95; 13665 -> 90;
        13666 -> 91; 13667 -> 92; 13668 -> 93; 13669 -> 94; 13670 -> 95; 13872 -> 96;
        13873 -> 97; 13874 -> 98; 13875 -> 99; 13876 -> 100; 13877 -> 101; 13878 -> 102;
        13879 -> 103; 13880 -> 104; 13881 -> 105; 13889 -> 106; 13890 -> 107; 13891 -> 108;
        13892 -> 109; 13893 -> 110; 13894 -> 111; 13921 -> 106; 13922 -> 107; 13923 -> 108;
        13924 -> 109; 13925 -> 110; 13926 -> 111; 14128 -> 112; 14129 -> 113; 14130 -> 114;
        14131 -> 115; 14132 -> 116; 14133 -> 117; 14134 -> 118; 14135 -> 119; 14136 -> 120;
        14137 -> 121; 14145 -> 122; 14146 -> 123; 14147 -> 124; 14148 -> 125; 14149 -> 126;
        14150 -> 127; 14177 -> 122; 14178 -> 123; 14179 -> 124; 14180 -> 125; 14181 -> 126;
        14182 -> 127; 14384 -> 128; 14385 -> 129; 14386 -> 130; 14387 -> 131; 14388 -> 132;
        14389 -> 133; 14390 -> 134; 14391 -> 135; 14392 -> 136; 14393 -> 137; 14401 -> 138;
        14402 -> 139; 14403 -> 140; 14404 -> 141; 14405 -> 142; 14406 -> 143; 14433 -> 138;
        14434 -> 139; 14435 -> 140; 14436 -> 141; 14437 -> 142; 14438 -> 143; 14640 -> 144;
        14641 -> 145; 14642 -> 146; 14643 -> 147; 14644 -> 148; 14645 -> 149; 14646 -> 150;
        14647 -> 151; 14648 -> 152; 14649 -> 153; 14657 -> 154; 14658 -> 155; 14659 -> 156;
        14660 -> 157; 14661 -> 158; 14662 -> 159; 14689 -> 154; 14690 -> 155; 14691 -> 156;
        14692 -> 157; 14693 -> 158; 14694 -> 159; 16688 -> 160; 16689 -> 161; 16690 -> 162;
        16691 -> 163; 16692 -> 164; 16693 -> 165; 16694 -> 166; 16695 -> 167; 16696 -> 168;
        16697 -> 169; 16705 -> 170; 16706 -> 171; 16707 -> 172; 16708 -> 173; 16709 -> 174;
        16710 -> 175; 16737 -> 170; 16738 -> 171; 16739 -> 172; 16740 -> 173; 16741 -> 174;
        16742 -> 175; 16944 -> 176; 16945 -> 177; 16946 -> 178; 16947 -> 179; 16948 -> 180;
        16949 -> 181; 16950 -> 182; 16951 -> 183; 16952 -> 184; 16953 -> 185; 16961 -> 186;
        16962 -> 187; 16963 -> 188; 16964 -> 189; 16965 -> 190; 16966 -> 191; 16993 -> 186;
        16994 -> 187; 16995 -> 188; 16996 -> 189; 16997 -> 190; 16998 -> 191; 17200 -> 192;
        17201 -> 193; 17202 -> 194; 17203 -> 195; 17204 -> 196; 17205 -> 197; 17206 -> 198;
        17207 -> 199; 17208 -> 200; 17209 -> 201; 17217 -> 202; 17218 -> 203; 17219 -> 204;
        17220 -> 205; 17221 -> 206; 17222 -> 207; 17249 -> 202; 17250 -> 203; 17251 -> 204;
        17252 -> 205; 17253 -> 206; 17254 -> 207; 17456 -> 208; 17457 -> 209; 17458 -> 210;
        17459 -> 211; 17460 -> 212; 17461 -> 213; 17462 -> 214; 17463 -> 215; 17464 -> 216;
        17465 -> 217; 17473 -> 218; 17474 -> 219; 17475 -> 220; 17476 -> 221; 17477 -> 222;
        17478 -> 223; 17505 -> 218; 17506 -> 219; 17507 -> 220; 17508 -> 221; 17509 -> 222;
        17510 -> 223; 17712 -> 224; 17713 -> 225; 17714 -> 226; 17715 -> 227; 17716 -> 228;
        17717 -> 229; 17718 -> 230; 17719 -> 231; 17720 -> 232; 17721 -> 233; 17729 -> 234;
        17730 -> 235; 17731 -> 236; 17732 -> 237; 17733 -> 238; 17734 -> 239; 17761 -> 234;
        17762 -> 235; 17763 -> 236; 17764 -> 237; 17765 -> 238; 17766 -> 239; 17968 -> 240;
        17969 -> 241; 17970 -> 242; 17971 -> 243; 17972 -> 244; 17973 -> 245; 17974 -> 246;
        17975 -> 247; 17976 -> 248; 17977 -> 249; 17985 -> 250; 17986 -> 251; 17987 -> 252;
        17988 -> 253; 17989 -> 254; 17990 -> 255; 18017 -> 250; 18018 -> 251; 18019 -> 252;
        18020 -> 253; 18021 -> 254; 18022 -> 255; 24880 -> 160; 24881 -> 161; 24882 -> 162;
        24883 -> 163; 24884 -> 164; 24885 -> 165; 24886 -> 166; 24887 -> 167; 24888 -> 168;
        24889 -> 169; 24897 -> 170; 24898 -> 171; 24899 -> 172; 24900 -> 173; 24901 -> 174;
        24902 -> 175; 24929 -> 170; 24930 -> 171; 24931 -> 172; 24932 -> 173; 24933 -> 174;
        24934 -> 175; 25136 -> 176; 25137 -> 177; 25138 -> 178; 25139 -> 179; 25140 -> 180;
        25141 -> 181; 25142 -> 182; 25143 -> 183; 25144 -> 184; 25145 -> 185; 25153 -> 186;
        25154 -> 187; 25155 -> 188; 25156 -> 189; 25157 -> 190; 25158 -> 191; 25185 -> 186;
        25186 -> 187; 25187 -> 188; 25188 -> 189; 25189 -> 190; 25190 -> 191; 25392 -> 192;
        25393 -> 193; 25394 -> 194; 25395 -> 195; 25396 -> 196; 25397 -> 197; 25398 -> 198;
        25399 -> 199; 25400 -> 200; 25401 -> 201; 25409 -> 202; 25410 -> 203; 25411 -> 204;
        25412 -> 205; 25413 -> 206; 25414 -> 207; 25441 -> 202; 25442 -> 203; 25443 -> 204;
        25444 -> 205; 25445 -> 206; 25446 -> 207; 25648 -> 208; 25649 -> 209; 25650 -> 210;
        25651 -> 211; 25652 -> 212; 25653 -> 213; 25654 -> 214; 25655 -> 215; 25656 -> 216;
        25657 -> 217; 25665 -> 218; 25666 -> 219; 25667 -> 220; 25668 -> 221; 25669 -> 222;
        25670 -> 223; 25697 -> 218; 25698 -> 219; 25699 -> 220; 25700 -> 221; 25701 -> 222;
        25702 -> 223; 25904 -> 224; 25905 -> 225; 25906 -> 226; 25907 -> 227; 25908 -> 228;
        25909 -> 229; 25910 -> 230; 25911 -> 231; 25912 -> 232; 25913 -> 233; 25921 -> 234;
        25922 -> 235; 25923 -> 236; 25924 -> 237; 25925 -> 238; 25926 -> 239; 25953 -> 234;
        25954 -> 235; 25955 -> 236; 25956 -> 237; 25957 -> 238; 25958 -> 239; 26160 -> 240;
        26161 -> 241; 26162 -> 242; 26163 -> 243; 26164 -> 244; 26165 -> 245; 26166 -> 246;
        26167 -> 247; 26168 -> 248; 26169 -> 249; 26177 -> 250; 26178 -> 251; 26179 -> 252;
        26180 -> 253; 26181 -> 254; 26182 -> 255; 26209 -> 250; 26210 -> 251; 26211 -> 252;
        26212 -> 253; 26213 -> 254; 26214 -> 255;
        _ -> throw_token(6, Rest, Input, Pos, Buffer)
    end.

escapeu_1(<<_/bitstring>> = Rest, Input, Pos, Buffer, Acc, Last, X) ->
    A = 6 bsl 5 + (X bsl 2) + (Last bsr 6),
    B = 2 bsl 6 + Last band 63,
    C = [Acc, A, B],
    string(Rest, Input, Pos + 6, Buffer, C, 0).

escapeu_2(<<_/bitstring>> = Rest, Input, Pos, Buffer, Acc, Last, X) ->
    A = 14 bsl 4 + (X bsr 4),
    B = 2 bsl 6 + (X band 15 bsl 2) + (Last bsr 6),
    C = 2 bsl 6 + Last band 63,
    D = [Acc, A, B, C],
    string(Rest, Input, Pos + 6, Buffer, D, 0).

escape_surrogate(Data, Input, Pos, Buffer, Acc, Hi) ->
    case Data of
        <<92/integer, 117/integer, Int1:16/integer, Int2:16/integer, Rest/bitstring>> ->
            Last = escapeu_last(Int2, Rest, Input, Pos + 6, Buffer),
            X = case Int1 of
                17475 -> 220; 17476 -> 221; 17477 -> 222; 17478 -> 223;
                17507 -> 220; 17508 -> 221; 17509 -> 222; 17510 -> 223;
                25667 -> 220; 25668 -> 221; 25669 -> 222; 25670 -> 223;
                25699 -> 220; 25700 -> 221; 25701 -> 222; 25702 -> 223;
                _ -> throw_token(12, Rest, Input, Pos, Buffer)
            end,
            Y = X band 3 bsl 8 + Last,
            Acc1 = [Acc | <<(Hi + Y)/utf8>>],
            string(Rest, Input, Pos + 12, Buffer, Acc1, 0);
        <<_/integer, _/bitstring>> ->
            throw_byte(Data, Input, Pos + 6, Buffer);
        <<_/bitstring>> ->
            throw_eof(Input, Pos, Buffer)
    end.

throw_byte(<<Rest/bitstring>>, Input, Pos, Buffer) ->
    Byte = binary:at(Input, Pos),
    throw({{byte, Byte}, Rest, Input, Pos, Buffer}).

throw_token(Len, Rest, Input, Pos, Buffer) when is_integer(Len) ->
    Token = binary_part(Input, Pos, Len),
    throw_token(Token, Rest, Input, Pos, Buffer);
throw_token(Token, Rest, Input, Pos, Buffer) ->
    throw({{token, Token}, Rest, Input, Pos, Buffer}).

throw_eof(Input, Pos, Buffer) ->
    throw({eof, Input, Pos, Buffer}).

%%%=====================================================================
%%% Eunit tests
%%%=====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

decode_test() ->
    [ ?assertEqual(Expect, decode(Input))
      || {Expect, Input} <- [
        {{ok, <<"foo">>}, <<"\"foo\"">>},
        {{ok, 123}, <<"123">>},
        {{ok, 1.234}, <<"1.234">>},
        {{ok, 6.02e23}, <<"6.02e+23">>},
        { {ok, [<<"foo">>, 123, 6.02e+23, true]}
        , <<"[\"foo\",123,6.02e+23,true]">>},
        { {ok, #{<<"foo">> => <<"bar">>, <<"bar">> => <<"baz">>}}
        , <<"{\"foo\": \"bar\", \"bar\": \"baz\"}">>},
        {{ok, true}, <<"true">>},
        {{ok, false}, <<"false">>},
        {{ok, undefined}, <<"null">>},
        {{ok, <<"ABC">>}, <<"\"\\u0041\\u0042\\u0043\"">>},
        { {ok, #{<<"datetime">> => {{1970,1,1},{0,0,0}}}}
        , <<"{\"datetime\": \"1970-01-01T00:00:00Z\"}">> },
        { {ok, #{<<"timestamp">> => {0,0,0}}}
        , <<"{\"timestamp\": \"1970-01-01T00:00:00.000Z\"}">> }
    ]].

-endif.
