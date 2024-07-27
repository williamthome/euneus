-module(euneus_decoder).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([decode/1]).
-export([decode/2]).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

decode(JSON) ->
    decode(JSON, #{}).

decode(JSON, Opts) when is_binary(JSON), is_map(Opts) ->
    norm_result(json:decode(JSON, [], decoders(Opts))).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

decoders(Opts) ->
    #{
        null => maps:get(null, Opts, null),
        array_finish => array_finish_decoder(maps:get(array_finish, Opts, ordered)),
        object_push => object_push_decoder(maps:get(object_push, Opts, push)),
        object_finish => object_finish_decoder(maps:get(object_finish, Opts, map)),
        integer => maps:get(binary_to_integer, Opts, fun erlang:binary_to_integer/1),
        float => maps:get(binary_to_float, Opts, fun erlang:binary_to_float/1)
     }.

array_finish_decoder(ordered) ->
    fun(Acc, OldAcc) -> {lists:reverse(Acc), OldAcc} end;
array_finish_decoder(reversed) ->
    fun(Acc, OldAcc) -> {Acc, OldAcc} end;
array_finish_decoder(Decoder) when is_function(Decoder, 2) ->
    Decoder.

object_push_decoder(push) ->
    fun(Key, Value, Acc) -> [{Key, Value} | Acc] end;
object_push_decoder(Decoder) when is_function(Decoder, 3) ->
    Decoder.

object_finish_decoder(map) ->
    fun(Acc, OldAcc) -> {maps:from_list(Acc), OldAcc} end;
object_finish_decoder(proplist) ->
    fun(Acc, OldAcc) -> {lists:reverse(Acc), OldAcc} end;
object_finish_decoder(reversed_proplist) ->
    fun(Acc, OldAcc) -> {Acc, OldAcc} end;
object_finish_decoder(Decoder) when is_function(Decoder, 2) ->
    Decoder.

norm_result({Result, [], <<>>}) ->
    Result.

