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
    deps = [
      {:euneus, path: "../"},
      {:stream_data, "~> 0.4", only: :test}
    ]

    case :erlang.system_info(:otp_release) do
      rel when is_integer(rel) and rel >= 27 ->
        deps

      _ ->
        deps ++ [{:json_polyfill, "~> 0.1"}]
    end
  end
end
