-module(euneus).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([encode/1]).
-export([encode/2]).
-export([decode/1]).
-export([decode/2]).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-spec encode(term()) -> iodata().
encode(Term) ->
    euneus_encoder:encode(Term, #{}).

-spec encode(term(), euneus_encoder:options()) -> iodata().
encode(Term, Opts) ->
    euneus_encoder:encode(Term, Opts).

-spec decode(binary()) -> term().
decode(JSON) ->
    euneus_decoder:decode(JSON, #{}).

-spec decode(binary(), euneus_decoder:options()) -> term().
decode(JSON, Opts) ->
    euneus_decoder:decode(JSON, Opts).

