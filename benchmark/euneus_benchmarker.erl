-module(euneus_benchmarker).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([bootstrap/0]).
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

-spec decode_benchmark() -> ok.
decode_benchmark() ->
    Files = read_data_dir_files(),
    Reports = erlperf:benchmark(
        [
            % #Fun<euneus_benchmark.0.*>
            #{
                runner => fun(ok) ->
                    lists:foreach(
                        fun(JSON) ->
                            euneus:decode(JSON, #{}),
                            ok
                        end,
                        Files
                    )
                end,
                init_runner => "ok."
            },
            % #Fun<euneus_benchmark.1.*>
            #{
                runner => fun(ok) ->
                    lists:foreach(
                        fun(JSON) ->
                            jiffy:decode(JSON, [return_maps]),
                            ok
                        end,
                        Files
                    )
                end,
                init_runner => "ok."
            },
            % #Fun<euneus_benchmark.2.*>
            #{
                runner => fun(ok) ->
                    lists:foreach(
                        fun(JSON) ->
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

read_data_dir_files() ->
    {ok, CWD} = file:get_cwd(),
    DataDir = filename:join([CWD, "test", "euneus_benchmarker_SUITE_data"]),
    {ok, Files} = file:list_dir(DataDir),
    [read_data_dir_file(DataDir, Filename) || Filename <- Files].

read_data_dir_file(DataDir, Filename) ->
    AbsFilename = filename:join(DataDir, Filename),
    {ok, Binary} = file:read_file(AbsFilename),
    Binary.
