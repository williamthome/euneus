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
    foreach_data_dir_file(
        fun(JSON) ->
            [
                ?assertEqual(
                    euneus:decode(JSON, #{}),
                    jiffy:decode(JSON, [return_maps])
                )
            ]
        end,
        get_data_dir(Config)
    ).

thoas_decode_test(Config) when is_list(Config) ->
    foreach_data_dir_file(
        fun(JSON) ->
            [
                ?assertEqual(
                    euneus:decode(JSON, #{}),
                    element(2, thoas:decode(JSON, #{}))
                )
            ]
        end,
        get_data_dir(Config)
    ).

%% --------------------------------------------------------------------
%% Test support
%% --------------------------------------------------------------------

get_data_dir(Config) ->
    proplists:get_value(data_dir, Config).

foreach_data_dir_file(Callback, DataDir) ->
    {ok, Files} = file:list_dir(DataDir),
    [Callback(read_data_dir_file(DataDir, Filename)) || Filename <- Files].

read_data_dir_file(DataDir, Filename) ->
    AbsFilename = filename:join(DataDir, Filename),
    {ok, Binary} = file:read_file(AbsFilename),
    Binary.
