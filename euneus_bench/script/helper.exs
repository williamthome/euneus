defmodule EuneusBench.Helper do
  @priv_dir :code.priv_dir(:euneus_bench)

  def read_data(name) do
    name
    |> String.downcase()
    |> String.replace(~r/([^\w]|-|_)+/, "-")
    |> String.trim("-")
    |> (&Path.join(@priv_dir, "data/#{&1}.json")).()
    |> File.read!()
  end

  def run(label, jobs, inputs, opts \\ %{}) do
    Benchee.run(jobs,
      parallel: Map.get(opts, :parallel, 1),
      warmup: Map.get(opts, :warmup, 5),
      time: Map.get(opts, :time, 30),
      memory_time: Map.get(opts, :memory_time, 1),
      inputs: inputs,
      formatters: [
        {Benchee.Formatters.HTML, file: Path.expand("../tmp/#{label}.html", __DIR__)},
        Benchee.Formatters.Console
      ]
    )
  end
end
