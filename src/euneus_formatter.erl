-module(euneus_formatter).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([format/2]).

%% --------------------------------------------------------------------
%% Type exports
%% --------------------------------------------------------------------

-export_type([options/0]).

%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------

-type options() :: #{
    indent_type := tabs | spaces,
    indent_width := non_neg_integer(),
    spaced_values := boolean(),
    crlf := r | n | rn | none
}.

-record(state, {
    depth,
    indent,
    spaces,
    crlf
}).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-spec format(binary(), options()) -> iodata().
%% @doc Formats a binary JSON.
%%
%% Option details:
%%
%% <ul>
%%   <blockquote>
%%     <h4 class="info">Note</h4>
%%     There is no default for any option, all are required.
%%   </blockquote>
%%   <li>
%%     `indent_type' - Indent using `tabs' or `spaces'.
%%
%%     <ul>
%%       <li>
%%         `tabs' - The indent char will be `$\t'.
%%
%%       </li>
%%       <li>
%%         `spaces' - The indent char will be `$\s'.
%%
%%       </li>
%%     </ul>
%%
%%   </li>
%%   <li>
%%     `indent_width' - The `indent_type' will be copied N times based on it.
%%
%%   </li>
%%   <li>
%%     `spaced_values' - Defines if keys and values of objects should be
%%     spaced by one `$\s' char.
%%
%%   </li>
%%   <li>
%%     `crlf' - Defines the Carriage Return/Line Feed.
%%
%%     <ul>
%%       <li>
%%         `r' - The CRLF will be `<<$\r>>'.
%%
%%       </li>
%%       <li>
%%         `n' - The CRLF will be `<<$\n>>'.
%%
%%       </li>
%%       <li>
%%         `rn' - The CRLF will be `<<$\r, $\n>>'.
%%
%%       </li>
%%       <li>
%%         `none' - The CRLF will be `<<>>'.
%%
%%       </li>
%%     </ul>
%%   </li>
%% </ul>
format(JSON, Opts) when is_binary(JSON), is_map(Opts) ->
    do_format(JSON, new_state(Opts)).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

% State

new_state(Opts) ->
    #state{
        depth = 0,
        indent = parse_indent(
            maps:get(indent_type, Opts),
            maps:get(indent_width, Opts)
        ),
        spaces = parse_spaces(maps:get(spaced_values, Opts)),
        crlf = maps:get(crlf, Opts)
    }.

parse_indent(Type, Width) when is_integer(Width), Width >= 0 ->
    binary:copy(parse_indent_type(Type), Width).

parse_indent_type(tabs) ->
    <<$\t>>;
parse_indent_type(spaces) ->
    <<$\s>>.

parse_spaces(true) ->
    <<$\s>>;
parse_spaces(false) ->
    <<>>.

incr_depth(State) ->
    State#state{depth = State#state.depth + 1}.

decr_depth(State) ->
    State#state{depth = State#state.depth - 1}.

% Format

do_format(<<$\s, Rest/binary>>, State) ->
    do_format(Rest, State);
do_format(<<$\t, Rest/binary>>, State) ->
    do_format(Rest, State);
do_format(<<$\r, Rest/binary>>, State) ->
    do_format(Rest, State);
do_format(<<$\n, Rest/binary>>, State) ->
    do_format(Rest, State);
do_format(<<$", Rest0/binary>>, State) ->
    {Str, Rest} = string(Rest0),
    [Str | do_format(Rest, State)];
do_format(<<$:, Rest/binary>>, State) ->
    [$:, spaces(State) | do_format(Rest, State)];
do_format(<<$,, Rest/binary>>, State) ->
    [$,, new_line(State) | do_format(Rest, State)];
do_format(<<"true", Rest/binary>>, State) ->
    [<<"true">> | do_format(Rest, State)];
do_format(<<"false", Rest/binary>>, State) ->
    [<<"false">> | do_format(Rest, State)];
do_format(<<"null", Rest/binary>>, State) ->
    [<<"null">> | do_format(Rest, State)];
do_format(<<${, Rest/binary>>, State0) ->
    State = incr_depth(State0),
    [${, new_line(State) | do_format(Rest, State)];
do_format(<<$}, Rest/binary>>, State0) ->
    State = decr_depth(State0),
    [new_line(State), $} | do_format(Rest, State)];
do_format(<<$[, Rest/binary>>, State0) ->
    State = incr_depth(State0),
    [$[, new_line(State) | do_format(Rest, State)];
do_format(<<$], Rest/binary>>, State0) ->
    State = decr_depth(State0),
    [new_line(State), $] | do_format(Rest, State)];
do_format(<<Char/integer, Rest/binary>>, State) ->
    [Char | do_format(Rest, State)];
do_format(<<>>, _State) ->
    [].

new_line(State) ->
    do_new_line(State#state.crlf, State).

do_new_line(r, State) ->
    <<$\r, (indent(State))/binary>>;
do_new_line(n, State) ->
    <<$\n, (indent(State))/binary>>;
do_new_line(rn, State) ->
    <<$\r, $\n, (indent(State))/binary>>;
do_new_line(none, _State) ->
    <<>>.

indent(State) ->
    binary:copy(State#state.indent, State#state.depth).

spaces(State) ->
    State#state.spaces.

string(Rest0) ->
    {Len, Rest} = find_string_end(Rest0, 0),
    {<<$", (binary_part(Rest0, 0, Len))/binary, $">>, Rest}.

find_string_end(<<$\\, $", Rest/binary>>, Len) ->
    find_string_end(Rest, Len + 2);
find_string_end(<<$", Rest/binary>>, Len) ->
    {Len, Rest};
find_string_end(<<_, Rest/binary>>, Len) ->
    find_string_end(Rest, Len + 1).
