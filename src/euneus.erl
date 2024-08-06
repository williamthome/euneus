-module(euneus).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([encode/1]).
-export([encode/2]).
-export([encode_to_iodata/1]).
-export([encode_to_iodata/2]).
-export([decode/1]).
-export([decode/2]).
-export([decode_iodata/1]).
-export([decode_iodata/2]).
-export([minify/1]).
-export([format/2]).

%

-ignore_xref([encode/1]).
-ignore_xref([encode/2]).
-ignore_xref([encode_to_iodata/1]).
-ignore_xref([encode_to_iodata/2]).
-ignore_xref([decode/1]).
-ignore_xref([decode/2]).
-ignore_xref([decode_iodata/1]).
-ignore_xref([decode_iodata/2]).
-ignore_xref([minify/1]).
-ignore_xref([format/2]).

%% --------------------------------------------------------------------
%% DocTest
%% --------------------------------------------------------------------

-if(?OTP_RELEASE >= 27).
-ifdef(TEST).
-include_lib("doctest/include/doctest.hrl").
-endif.
-endif.

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-spec encode(Term) -> binary() when
    Term :: term().
%% @doc Encodes a term into a binary JSON.
%%
%% <em>Example:</em>
%%
%% ```
%% 1> euneus:encode(foo).
%% <<"\"foo\"">>
%% '''
encode(Term) ->
    encode(Term, #{}).

-spec encode(Term, Options) -> binary() when
    Term :: term(),
    Options :: euneus_encoder:options().
%% @doc Encodes a term into a binary JSON.
%%
%% <em>Example:</em>
%%
%% ```
%% 1> euneus:encode(foo, #{}).
%% <<"\"foo\"">>
%% '''
encode(Term, Opts) ->
    iolist_to_binary(euneus_encoder:encode(Term, Opts)).

-spec encode_to_iodata(Term) -> iodata() when
    Term :: term().
%% @doc Encodes a term into an iodata JSON.
%%
%% <em>Example:</em>
%%
%% ```
%% 1> euneus:encode_to_iodata(foo).
%% [$", <<"foo">>, $"]
%% '''
encode_to_iodata(Term) ->
    encode_to_iodata(Term, #{}).

-spec encode_to_iodata(Term, Options) -> iodata() when
    Term :: term(),
    Options :: euneus_encoder:options().
%% @doc Encodes a term into an iodata JSON.
%%
%% <em>Example:</em>
%%
%% ```
%% 1> euneus:encode_to_iodata(foo, #{}).
%% [$", <<"foo">>, $"]
%% '''
encode_to_iodata(Term, Opts) ->
    euneus_encoder:encode(Term, Opts).

-spec decode(JSON) -> term() when
    JSON :: binary().
%% @doc Decodes a binary JSON into a term.
%%
%% <em>Example:</em>
%%
%% ```
%% 1> euneus:decode(<<"\"foo\"">>).
%% <<"foo">>
%% '''
decode(JSON) ->
    decode(JSON, #{}).

-spec decode(JSON, Options) -> term() when
    JSON :: binary(),
    Options :: euneus_decoder:options().
%% @doc Decodes a binary JSON into a term.
%%
%% <em>Example:</em>
%%
%% ```
%% 1> euneus:decode(<<"\"foo\"">>, #{}).
%% <<"foo">>
%% '''
decode(JSON, Opts) ->
    euneus_decoder:decode(JSON, Opts).

-spec decode_iodata(JSON) -> term() when
    JSON :: iodata().
%% @doc Decodes an iodata JSON into a term.
%%
%% <em>Example:</em>
%%
%% ```
%% 1> euneus:decode_iodata([$", <<"foo">>, $"]).
%% <<"foo">>
%% '''
decode_iodata(JSON) ->
    decode_iodata(JSON, #{}).

-spec decode_iodata(JSON, Options) -> term() when
    JSON :: iodata(),
    Options :: euneus_decoder:options().
%% @doc Decodes an iodata JSON into a term.
%%
%% <em>Example:</em>
%%
%% ```
%% 1> euneus:decode_iodata([$", <<"foo">>, $"], #{}).
%% <<"foo">>
%% '''
decode_iodata(JSON, Opts) ->
    euneus_decoder:decode(iolist_to_binary(JSON), Opts).

-spec minify(JSON) -> binary() when
    JSON :: binary().
%% @doc Minifies a binary JSON.
%%
%% <em>Example:</em>
%%
%% ```
%% 1> euneus:minify(<<" \n{\"foo\"  :  [ true  , \n null ] \n  }  ">>).
%% <<"{\"foo\":[true,null]}">>
%% '''
minify(JSON) ->
    format(JSON, #{
        indent_type => spaces,
        indent_width => 0,
        spaced_values => false,
        crlf => none
    }).

-spec format(JSON, Options) -> binary() when
    JSON :: binary(),
    Options :: euneus_formatter:options().
%% @doc Formats a binary JSON.
%%
%% <em>Example:</em>
%%
%% ```
%% 1> Opts = #{
%% ..     indent_type => tabs,
%% ..     indent_width => 1,
%% ..     spaced_values => true,
%% ..     crlf => n
%% .. }.
%% #{indent_type => tabs,indent_width => 1,spaced_values => true, crlf => n}
%% 2> euneus:format(<<" \n{\"foo\"  :  [ true  , \n null ] \n  }  ">>, Opts).
%% <<"{\n\t\"foo\": [\n\t\ttrue,\n\t\tnull\n\t]\n}">>
%% '''
format(JSON, Opts) ->
    iolist_to_binary(euneus_formatter:format(JSON, Opts)).
