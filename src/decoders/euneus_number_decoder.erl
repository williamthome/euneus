-module(euneus_number_decoder).

-behaviour(euneus_decoder).

-export([ decode/2 ]).

-include("euneus_decoder.hrl").

decode(Bin, _Opts) ->
    do_decode(Bin, <<>>).

% @fixme: accepts only valid notation/format.
do_decode(<<H, T/binary>>, Buffer) when ?is_number(H) ->
    do_decode(T, <<Buffer/binary, H>>);
do_decode(<<$., T/binary>>, Buffer) ->
    do_decode(T, <<Buffer/binary, $.>>);
do_decode(<<$+, T/binary>>, Buffer) ->
do_decode(T, <<Buffer/binary, $+>>);
do_decode(<<$-, T/binary>>, Buffer) ->
do_decode(T, <<Buffer/binary, $->>);
do_decode(<<$e, T/binary>>, Buffer) ->
    do_decode(T, <<Buffer/binary, $e>>);
do_decode(<<$E, T/binary>>, Buffer) ->
    do_decode(T, <<Buffer/binary, $e>>);
% @fixme: resolve float values.
do_decode(T, Buffer) ->
    {T, binary_to_integer(Buffer)}.
