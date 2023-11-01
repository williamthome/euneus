defmodule EuneusTest.MixProject do
  use Mix.Project

  def project do
    [
      app: :euneus_test,
      version: "0.1.0",
      elixir: "~> 1.15-dev",
      start_permanent: Mix.env() == :prod,
      consolidate_protocols: Mix.env() != :test,
      deps: deps()
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:euneus, path: "../"},
      {:stream_data, "~> 0.4", only: :test},
    ]
  end
end
