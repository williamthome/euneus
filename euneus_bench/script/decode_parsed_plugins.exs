Code.eval_file("helper.exs", "./script")

opts =
  :euneus.parse_decode_opts(%{
    plugins: [
      :datetime,
      :timestamp,
      :pid,
      :port,
      :reference,
      :inet
    ]
  })

jobs = %{
  "euneus" => &:euneus.decode_parsed(&1, opts),
  "thoas" => &:thoas.decode/1,
  "jsone" => &:jsone.decode/1,
  "jsx" => &:jsx.decode/1,
  "jiffy" => &:jiffy.decode/1,
  "Jason" => &Jason.decode!/1
}

data = [
  "Blockchain",
  "Giphy",
  "GitHub",
  "GovTrack",
  "Issue 90",
  "JSON Generator (Pretty)",
  "JSON Generator",
  "Pokedex",
  "UTF-8 escaped",
  "UTF-8 unescaped"
]

inputs =
  for name <- data, into: %{} do
    name
    |> EuneusBench.Helper.read_data()
    |> (&{name, &1}).()
  end

EuneusBench.Helper.run(
  "decode_parsed_plugins",
  jobs,
  inputs,
  %{
    # markdown: true,
    # graph: true,
    # save: true,
    # parallel: 1,
    # warmup: 5,
    # time: 5,
    # memory_time: 1,
  }
)
