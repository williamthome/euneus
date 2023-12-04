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
-module(euneus_encoder_smart_unicode).

-include("euneus_encoder_smart.hrl").

escape(Data, Acc, Input, Pos) ->
    case Data of
        <<$"/integer, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\\"">>],
            escape(Rest, Acc1, Input, Pos+1);
        <<$\\/integer, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\\\">>],
            escape(Rest, Acc1, Input, Pos+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?NON_PRINTABLE_LAST ->
            Acc1 = [Acc | escape_byte(Byte)],
            escape(Rest, Acc1, Input, Pos+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?ONE_BYTE_LAST ->
            escape_chunk(Rest, Acc, Input, Pos, 1);
        <<Char/utf8, Rest/bitstring>> when Char < 256 ->
            Acc1 = [Acc | [<<"\\u00">>, integer_to_binary(Char, 16)]],
            escape(Rest, Acc1, Input, Pos+2);
        <<Char/utf8, Rest/bitstring>> when Char =< ?TWO_BYTE_LAST ->
            Acc1 = [Acc | [<<"\\u0">>, integer_to_binary(Char, 16)]],
            escape(Rest, Acc1, Input, Pos+2);
        <<Char/utf8, Rest/bitstring>> when Char < 4096 ->
            Acc1 = [Acc | [<<"\\u0">>, integer_to_binary(Char, 16)]],
            escape(Rest, Acc1, Input, Pos+3);
        <<Char/utf8, Rest/bitstring>> when Char =< ?THREE_BYTE_LAST ->
            Acc1 = [Acc | [<<"\\u">>, integer_to_binary(Char, 16)]],
            escape(Rest, Acc1, Input, Pos+3);
        <<Char0/utf8, Rest/bitstring>> ->
            Char = Char0 - 65536,
            Acc1 = [ Acc
                   | [ <<"\\uD">>
                     , integer_to_binary(2048 bor (Char bsr 10), 16)
                     , <<"\\uD">>
                     , integer_to_binary(3072 bor Char band 1023, 16) ]
                   ],
            escape(Rest, Acc1, Input, Pos+4);
        <<>> ->
            Acc;
        <<Byte/integer, _Rest/bitstring>> ->
            throw_invalid_byte_error(Byte, Input)
    end.

escape_chunk(Data, Acc, Input, Pos, Len) ->
    case Data of
        <<$"/integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\\"">>]],
            escape(Rest, Acc1, Input, Pos+Len+1);
        <<$\\/integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\\\">>]],
            escape(Rest, Acc1, Input, Pos+Len+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?NON_PRINTABLE_LAST ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, escape_byte(Byte)]],
            escape(Rest, Acc1, Input, Pos+Len+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?ONE_BYTE_LAST ->
            escape_chunk(Rest, Acc, Input, Pos, Len+1);
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
            escape(Rest, Acc1, Input, Pos+Len+2);
        <<Char/utf8, Rest/bitstring>> when Char =< ?TWO_BYTE_LAST ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\u0">>, integer_to_binary(Char, 16)]],
            escape(Rest, Acc1, Input, Pos+Len+2);
        <<Char/utf8, Rest/bitstring>> when Char < 4096 ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\u0">>, integer_to_binary(Char, 16)]],
            escape(Rest, Acc1, Input, Pos+Len+3);
        <<Char/utf8, Rest/bitstring>> when Char =< ?THREE_BYTE_LAST ->
            Part = binary_part(Input, Pos, Len),
            Acc1 = [Acc | [Part, <<"\\u">>, integer_to_binary(Char, 16)]],
            escape(Rest, Acc1, Input, Pos+Len+3);
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
            escape(Rest, Acc1, Input, Pos+Len+4);
        <<Byte/integer, _Rest/bitstring>> ->
            throw_invalid_byte_error(Byte, Input)
    end.
