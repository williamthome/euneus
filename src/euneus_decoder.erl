-module(euneus_decoder).

-export([ decode/1, decode/2, decode/3 ]).

-callback decode(Bin :: binary(), Opts :: map(), Buffer :: iolist()) -> iolist().

-include("euneus_decoder.hrl").

decode(Bin) ->
    decode(Bin, #{}).

decode(Bin, Opts) ->
    decode(Bin, Opts, []).

decode(Bin, Opts0, Buffer) ->
    Opts = #{
        decoders => decoders(Opts0)
    },
    do_decode(Bin, Opts, Buffer).

decoders(Options) ->
    Defaults = #{
        object => euneus_object_decoder,
        array => euneus_array_decoder,
        string => euneus_string_decoder,
        number => euneus_number_decoder
    },
    maps:merge(Defaults, maps:get(decoders, Options, #{})).

do_decode(<<${, T/binary>>, #{decoders := #{object := Decoder}} = Opts, Buffer) ->
    Decoder:decode(T, Opts, Buffer);
do_decode(<<$[, T/binary>>, #{decoders := #{array := Decoder}} = Opts, Buffer) ->
    Decoder:decode(T, Opts, Buffer);
do_decode(<<$", T/binary>>, #{decoders := #{string := Decoder}} = Opts, Buffer) ->
    Decoder:decode(T, Opts, Buffer);
do_decode(<<H, _/binary>> = T, #{decoders := #{number := Decoder}} = Opts, Buffer)
  when ?is_number(H) ->
    Decoder:decode(T, Opts, Buffer);
do_decode(<<H, T/binary>>, Opts, Buffer)
  when ?is_whitespace(H) ->
    do_decode(T, Opts, Buffer);
do_decode(<<$,, T/binary>>, Opts, Buffer) ->
    {more, T, Opts, Buffer};
do_decode(<<$], T/binary>>, Opts, Buffer) ->
    {array_end, T, Opts, Buffer};
do_decode(<<$:, T/binary>>, Opts, Buffer) ->
    {object_key, T, Opts, Buffer};
do_decode(<<$}, T/binary>>, Opts, Buffer) ->
    {object_end, T, Opts, Buffer};
do_decode(<<>>, _, Buffer) ->
    lists:reverse(Buffer).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

% @fixme: output must not be an array.
encode_test() ->
    ?assertEqual([<<"foo">>], decode(<<"\"foo\"">>)),
    ?assertEqual([123], decode(<<"123">>)),
    ?assertEqual([[[<<"foo">>], [123]]], decode(<<"[\"foo\",123]">>)),
    ?assertEqual([#{[<<"foo">>] => [123]}], decode(<<"{\"foo\":123}">>)).

-endif.
