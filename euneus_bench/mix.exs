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
      encode_v2: ["run script/encode_v2.exs"],
      encode_parsed: ["run script/encode_parsed.exs"],
      encode_parsed_codecs: ["run script/encode_parsed_codecs.exs"],
      decode: ["run script/decode.exs"],
      decode_v2: ["run script/decode_v2.exs"],
      decode_parsed: ["run script/decode_parsed.exs"],
      decode_parsed_codecs: ["run script/decode_parsed_codecs.exs"]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:euneus, path: "../"},
      {:thoas, "~> 1.2"},
      {:jason, "~> 1.4"},
      {:jsone, "~> 1.6"},
      {:jsx, "~> 3.1"},
      {:jiffy, "~> 1.1"},
      {:benchee, "~> 1.1"},
      {:benchee_html, "~> 1.0"},
      {:benchee_markdown, "~> 0.3"}
    ]
  end
end
