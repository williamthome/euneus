defmodule EuneusBench.MixProject do
  use Mix.Project

  def project do
    [
      app: :euneus_bench,
      version: "0.1.0",
      elixir: "~> 1.15-dev",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  defp aliases() do
    [
      encode: ["run script/encode.exs"],
      encode_opts: ["run script/encode_opts.exs"],
      decode: ["run script/decode.exs"],
      decode_opts: ["run script/decode_opts.exs"]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:euneus, path: "../"},
      {:thoas, "~> 1.2"},
      {:jason, "~> 1.4"},
      {:benchee, "~> 1.1"},
      {:benchee_html, "~> 1.0"}
    ]
  end
end
