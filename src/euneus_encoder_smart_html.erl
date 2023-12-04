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
-module(euneus_encoder_smart_html).

-include("euneus_encoder_smart.hrl").

escape(Data, Acc, Input, Pos) ->
    case Data of
        <<$"/integer, Rest/bitstring>> ->
          Acc1 = [Acc, <<"\\\"">>],
          escape(Rest, Acc1, Input, Pos+1);
        <<$\\/integer, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\\\">>],
            escape(Rest, Acc1, Input, Pos+1);
        <<$//integer, Rest/bitstring>> ->
          Acc1 = [Acc | <<"\\/">>],
          escape(Rest, Acc1, Input, Pos+1);
        <<Byte/integer, Rest/bitstring>> when Byte < 33 ->
            Acc1 = [Acc, escape_byte(Byte)],
            escape(Rest, Acc1, Input, Pos+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?ONE_BYTE_LAST ->
            escape_chunk(Rest, Acc, Input, Pos, 1);
        <<Char/utf8, Rest/bitstring>> when Char =< ?TWO_BYTE_LAST ->
            escape_chunk(Rest, Acc, Input, Pos, 2);
        <<8232/utf8, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\u2028">>],
            escape(Rest, Acc1, Input, Pos+3);
        <<8233/utf8, Rest/bitstring>> ->
            Acc1 = [Acc | <<"\\u2029">>],
            escape(Rest, Acc1, Input, Pos+3);
        <<Char/utf8, Rest/bitstring>> when Char =< ?THREE_BYTE_LAST ->
            escape_chunk(Rest, Acc, Input, Pos, 3);
        <<_Char/utf8, Rest/bitstring>> ->
            escape_chunk(Rest, Acc, Input, Pos, 4);
        <<>> ->
            Acc;
        <<Byte/integer, _Rest/bitstring>> ->
            throw_invalid_byte_error(Byte, Input)
    end.

escape_chunk(Data, Acc, Input, Pos, Len) ->
    case Data of
        <<$"/integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc2 = [Acc | [Part, <<"\\\"">>]],
            escape(Rest, Acc2, Input, Pos+Len+1);
        <<$\\/integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc2 = [Acc | [Part, <<"\\\\">>]],
            escape(Rest, Acc2, Input, Pos+Len+1);
        <<$//integer, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc2 = [Acc | [Part, <<"\\/">>]],
            escape(Rest, Acc2, Input, Pos+Len+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?NON_PRINTABLE_LAST ->
            Part = binary_part(Input, Pos, Len),
            Acc2 = [Acc, Part, escape_byte(Byte)],
            escape(Rest, Acc2, Input, Pos+Len+1);
        <<Byte/integer, Rest/bitstring>> when Byte =< ?ONE_BYTE_LAST ->
            escape_chunk(Rest, Acc, Input, Pos, Len+1);
        <<>> ->
            case Acc =:= [] of
                true ->
                    binary_part(Input, Pos, Len);
                false ->
                    [Acc, binary_part(Input, Pos, Len)]
            end;
        <<Char/utf8, Rest/bitstring>> when Char =< ?TWO_BYTE_LAST ->
            escape_chunk(Rest, Acc, Input, Pos, Len+2);
        <<8232/utf8, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc2 = [Acc | [Part, <<"\\u2028">>]],
            escape(Rest, Acc2, Input, Pos+Len+3);
        <<8233/utf8, Rest/bitstring>> ->
            Part = binary_part(Input, Pos, Len),
            Acc2 = [Acc | [Part, <<"\\u2029">>]],
            escape(Rest, Acc2, Input, Pos+Len+3);
        <<Char/utf8, Rest/bitstring>> when Char =< ?THREE_BYTE_LAST ->
            escape_chunk(Rest, Acc, Input, Pos, Len+3);
        <<_Char/utf8, Rest/bitstring>> ->
            escape_chunk(Rest, Acc, Input, Pos, Len+4);
        <<Byte/integer, _Rest/bitstring>> ->
            throw_invalid_byte_error(Byte, Input)
    end.
