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
-export([minify/1]).
-export([format/2]).

%

-ignore_xref([encode/1]).
-ignore_xref([encode/2]).
-ignore_xref([encode_iodata/1]).
-ignore_xref([encode_iodata/2]).
-ignore_xref([decode/1]).
-ignore_xref([decode/2]).
-ignore_xref([decode_iodata/1]).
-ignore_xref([decode_iodata/2]).
-ignore_xref([minify/1]).
-ignore_xref([format/2]).

%% --------------------------------------------------------------------
%% DocTest
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("doctest/include/doctest.hrl").
-endif.

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-spec encode(term()) -> binary().
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

-spec encode(term(), euneus_encoder:options()) -> binary().
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

-spec encode_iodata(term()) -> iodata().
%% @doc Encodes a term into an iodata JSON.
%%
%% <em>Example:</em>
%%
%% ```
%% 1> euneus:encode_iodata(foo).
%% [$", <<"foo">>, $"]
%% '''
encode_iodata(Term) ->
    encode_iodata(Term, #{}).

-spec encode_iodata(term(), euneus_encoder:options()) -> iodata().
%% @doc Encodes a term into an iodata JSON.
%%
%% <em>Example:</em>
%%
%% ```
%% 1> euneus:encode_iodata(foo, #{}).
%% [$", <<"foo">>, $"]
%% '''
encode_iodata(Term, Opts) ->
    euneus_encoder:encode(Term, Opts).

-spec decode(binary()) -> term().
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

-spec decode(binary(), euneus_decoder:options()) -> term().
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

-spec decode_iodata(iodata()) -> term().
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

-spec decode_iodata(iodata(), euneus_decoder:options()) -> term().
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

-spec minify(binary()) -> binary().
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

-spec format(binary(), euneus_formatter:options()) -> binary().
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
