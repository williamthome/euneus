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
      warmup: Map.get(opts, :warmup, 1),
      time: Map.get(opts, :time, 10),
      memory_time: Map.get(opts, :memory_time, 1),
      pre_check: true,
      load:
        if opts[:save] do
          Path.join(__DIR__, "../runs/#{label}.benchee")
        else
          false
        end,
      save:
        if opts[:save] do
          [path: Path.join(__DIR__, "../runs/#{label}.benchee")]
        else
          false
        end,
      inputs: inputs,
      formatters:
        [
          {Benchee.Formatters.Console, comparison: true, extended_statistics: false}
        ] ++
          if opts[:graph] do
            [{Benchee.Formatters.HTML, file: Path.expand("../tmp/html/#{label}.html", __DIR__)}]
          else
            []
          end ++
          if opts[:markdown] do
            [
              {Benchee.Formatters.Markdown,
               file: Path.expand("../tmp/markdown/#{label}.md", __DIR__)}
            ]
          else
            []
          end
    )
  end
end
