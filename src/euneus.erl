-module(euneus).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([decode/1]).
-export([decode/2]).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-spec decode(binary()) -> term().
decode(JSON) ->
    euneus_decoder:decode(JSON, #{}).

-spec decode(binary(), euneus_decoder:options()) -> term().
decode(JSON, Opts) ->
    euneus_decoder:decode(JSON, Opts).

