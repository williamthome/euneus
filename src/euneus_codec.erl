%%%---------------------------------------------------------------------
%%% @copyright 2023 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Codec behavior.
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
-module(euneus_codec).

%% Types

-export_type([ encode/1 ]).
-export_type([ decode/1 ]).

-type encode_settings() :: euneus_encoder:settings().
-type encode(Type) :: next
                    | {next, Type}
                    | {encode, Type}
                    | {halt, iolist()}
                    | {new_type, term()}.

-type decode_settings() :: euneus_decoder:settings().
% Should we support 'new_type' like encode?
-type decode(Type) :: next
                    | {next, Type}
                    | {halt, term()}.

%% Callbacks

-callback init(term()) -> {ok, term()}.

-callback encode(Input, CodecOpts, EncodeSettings) -> Output when
    Input :: term(),
    CodecOpts :: term(),
    EncodeSettings :: encode_settings(),
    Output :: encode(Input).

-callback decode(Input, CodecOpts, DecodeSettings) -> Output when
    Input :: term(),
    CodecOpts :: term(),
    DecodeSettings :: decode_settings(),
    Output :: decode(Input).
