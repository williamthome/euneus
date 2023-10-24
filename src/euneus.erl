%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Core module to parse and generate JSON.

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

-compile({inline, [ encode/1, encode_to_binary/1, encode_to_binary/2, decode/1 ]}).

-export([ encode/1, encode/2 ]).
-export([ encode_to_binary/1, encode_to_binary/2 ]).
-export([ decode/1, decode/2 ]).

encode(Term) ->
    encode(Term, #{}).

encode(Term, Opts) ->
    euneus_encoder:encode(Term, Opts).

encode_to_binary(Term) ->
    encode_to_binary(Term, #{}).

encode_to_binary(Term, Opts) ->
    iolist_to_binary(encode(Term, Opts)).

decode(Bin) ->
    decode(Bin, #{}).

decode(Bin, Opts) ->
    euneus_decoder:decode(Bin, Opts).
