Code.eval_file("helper.exs", "./script")

euneus_opts = :euneus_decoder.parse_opts(%{})
thoas_opts = %{}

jobs = %{
  "euneus" => &:euneus_decoder.decode_parsed(&1, euneus_opts),
  "thoas" => &:thoas_decode.decode(&1, thoas_opts)
}

data = [
  "Blockchain",
  # "Giphy",
  # "GitHub",
  # "GovTrack",
  # "Issue 90",
  # "JSON Generator (Pretty)",
  # "JSON Generator",
  # "Pokedex",
  # "UTF-8 escaped",
  # "UTF-8 unescaped"
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
    # graph: true,
    # parallel: 1,
    # warmup: 5,
    # time: 5,
    # memory_time: 1,
  }
)
