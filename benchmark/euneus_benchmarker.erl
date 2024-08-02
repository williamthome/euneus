-module(euneus_benchmarker).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([bootstrap/0]).
-export([encode_benchmark/0]).
-export([decode_benchmark/0]).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-spec bootstrap() -> ok.
bootstrap() ->
    io:format(
        os:cmd(
            "cd " ++ code:lib_dir(erlperf) ++
                " && "
                "rebar3 escriptize"
        )
    ).

-spec encode_benchmark() -> ok.
encode_benchmark() ->
    Data = read_decoded_data_dir_files(),
    Reports = erlperf:benchmark(
        [
            % #Fun<euneus_benchmark.0.*>
            #{
                runner => fun(ok) ->
                    lists:foreach(
                        fun({_Filename, Term}) ->
                            euneus:encode(Term, #{}),
                            ok
                        end,
                        Data
                    )
                end,
                init_runner => "ok."
            },
            % #Fun<euneus_benchmark.1.*>
            #{
                runner => fun(ok) ->
                    lists:foreach(
                        fun({_Filename, Term}) ->
                            jiffy:encode(Term, []),
                            ok
                        end,
                        Data
                    )
                end,
                init_runner => "ok."
            },
            % #Fun<euneus_benchmark.2.*>
            #{
                runner => fun(ok) ->
                    lists:foreach(
                        fun({_Filename, Term}) ->
                            thoas:encode(Term, #{}),
                            ok
                        end,
                        Data
                    )
                end,
                init_runner => "ok."
            }
        ],
        #{report => full},
        undefined
    ),
    io:format(
        erlperf_cli:format(
            Reports,
            #{format => full}
        )
    ).

-spec decode_benchmark() -> ok.
decode_benchmark() ->
    Files = read_data_dir_files(),
    Reports = erlperf:benchmark(
        [
            % #Fun<euneus_benchmark.3.*>
            #{
                runner => fun(ok) ->
                    lists:foreach(
                        fun({_Filename, JSON}) ->
                            euneus:decode(JSON, #{}),
                            ok
                        end,
                        Files
                    )
                end,
                init_runner => "ok."
            },
            % #Fun<euneus_benchmark.4.*>
            #{
                runner => fun(ok) ->
                    lists:foreach(
                        fun({_Filename, JSON}) ->
                            jiffy:decode(JSON, [return_maps]),
                            ok
                        end,
                        Files
                    )
                end,
                init_runner => "ok."
            },
            % #Fun<euneus_benchmark.5.*>
            #{
                runner => fun(ok) ->
                    lists:foreach(
                        fun({_Filename, JSON}) ->
                            {ok, _} = thoas:decode(JSON, #{}),
                            ok
                        end,
                        Files
                    )
                end,
                init_runner => "ok."
            }
        ],
        #{report => full},
        undefined
    ),
    io:format(
        erlperf_cli:format(
            Reports,
            #{format => full}
        )
    ).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

read_decoded_data_dir_files() ->
    map_data_dir_files(
        fun({Filename, JSON}) ->
            {Filename, euneus:decode(JSON)}
        end
    ).

read_data_dir_files() ->
    map_data_dir_files(fun({Filename, JSON}) -> {Filename, JSON} end).

map_data_dir_files(Callback) ->
    {ok, CWD} = file:get_cwd(),
    DataDir = filename:join([CWD, "test", "euneus_benchmarker_SUITE_data"]),
    {ok, Files} = file:list_dir(DataDir),
    [
        Callback({
            Filename,
            read_data_dir_file(DataDir, Filename)
        })
     || Filename <- Files
    ].

read_data_dir_file(DataDir, Filename) ->
    AbsFilename = filename:join(DataDir, Filename),
    {ok, Binary} = file:read_file(AbsFilename),
    Binary.
