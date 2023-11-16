Code.eval_file("helper.exs", "./script")

opts = :euneus.parse_encode_opts(%{})

jobs = %{
  "euneus" => &:euneus.encode(&1, opts),
  "thoas" => &:thoas.encode_to_iodata/1,
  "jsone" => &:jsone.encode/1,
  "jsx" => &:jsx.encode/1,
  "jiffy" => &:jiffy.encode/1
  # "Jason" => &Jason.encode_to_iodata/1,
}

data = [
  "Blockchain",
  "Giphy",
  "GitHub",
  "GovTrack",
  "Issue 90",
  "JSON Generator",
  "Pokedex",
  "UTF-8 unescaped"
]

inputs =
  for name <- data, into: %{} do
    name
    |> EuneusBench.Helper.read_data()
    |> Jason.decode!()
    |> (&{name, &1}).()
  end

EuneusBench.Helper.run(
  "encode",
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
