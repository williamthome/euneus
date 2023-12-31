Code.eval_file("helper.exs", "./script")

jobs = %{
  "euneus" => &:euneus.decode/1,
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
  "decode",
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
