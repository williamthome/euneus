-module(euneus).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([encode/1]).
-export([encode/2]).
-export([encode_iodata/1]).
-export([encode_iodata/2]).
-export([decode/1]).
-export([decode/2]).
-export([decode_iodata/1]).
-export([decode_iodata/2]).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-spec encode(term()) -> binary().
encode(Term) ->
    encode(Term, #{}).

-spec encode(term(), euneus_encoder:options()) -> binary().
encode(Term, Opts) ->
    iolist_to_binary(euneus_encoder:encode(Term, Opts)).

-spec encode_iodata(term()) -> iodata().
encode_iodata(Term) ->
    encode_iodata(Term, #{}).

-spec encode_iodata(term(), euneus_encoder:options()) -> iodata().
encode_iodata(Term, Opts) ->
    euneus_encoder:encode(Term, Opts).

-spec decode(binary()) -> term().
decode(JSON) ->
    decode(JSON, #{}).

-spec decode(binary(), euneus_decoder:options()) -> term().
decode(JSON, Opts) ->
    euneus_decoder:decode(JSON, Opts).

-spec decode_iodata(iodata()) -> term().
decode_iodata(JSON) ->
    decode_iodata(JSON, #{}).

-spec decode_iodata(iodata(), euneus_decoder:options()) -> term().
decode_iodata(JSON, Opts) ->
    euneus_decoder:decode(iolist_to_binary(JSON), Opts).

