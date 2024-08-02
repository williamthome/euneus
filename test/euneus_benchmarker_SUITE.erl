-module(euneus_benchmarker_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        Fun
     || {Fun, 1} <- ?MODULE:module_info(exports),
        re:run(atom_to_binary(Fun), <<".*_test$">>) =/= nomatch
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

jiffy_decode_test(Config) when is_list(Config) ->
    map_data_dir_files(
        fun({_Filename, JSON}) ->
            ?assertEqual(
                euneus:decode(JSON, #{}),
                jiffy:decode(JSON, [return_maps])
            )
        end,
        get_data_dir(Config)
    ).

thoas_decode_test(Config) when is_list(Config) ->
    map_data_dir_files(
        fun({_Filename, JSON}) ->
            ?assertEqual(
                euneus:decode(JSON, #{}),
                element(2, thoas:decode(JSON, #{}))
            )
        end,
        get_data_dir(Config)
    ).

% We cannot test for equality here since keys of other libs are not sortable.
encode_test(Config) when is_list(Config) ->
    Data = read_decoded_data_dir_files(get_data_dir(Config)),
    lists:foreach(
        fun({_Filename, Term}) ->
            euneus:encode(Term, #{}),
            ?assert(true)
        end,
        Data
    ).

%% --------------------------------------------------------------------
%% Test support
%% --------------------------------------------------------------------

read_decoded_data_dir_files(DataDir) ->
    map_data_dir_files(
        fun({Filename, JSON}) ->
            {Filename, euneus:decode(JSON)}
        end,
        DataDir
    ).

get_data_dir(Config) ->
    proplists:get_value(data_dir, Config).

map_data_dir_files(Callback, DataDir) ->
    {ok, Files} = file:list_dir(DataDir),
    [
        Callback({
            filename:join(DataDir, Filename),
            read_file(filename:join(DataDir, Filename))
        })
     || Filename <- Files
    ].

read_file(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    Binary.
