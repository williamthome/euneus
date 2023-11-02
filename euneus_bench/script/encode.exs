Code.eval_file("helper.exs", "./script")

jobs = %{
  "euneus" => &:euneus.encode/1,
  "thoas" => &:thoas.encode_to_iodata/1
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
    |> Jason.decode!()
    |> (&{name, &1}).()
  end

EuneusBench.Helper.run(
  "encode",
  jobs,
  inputs,
  %{
    # graph: true,
    # parallel: 1,
    # warmup: 5,
    # time: 5,
    # memory_time: 1,
  }
)
