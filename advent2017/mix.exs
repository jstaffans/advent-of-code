defmodule Advent2017.Mixfile do
  use Mix.Project

  def project do
    [
      app: :advent2017,
      version: "0.1.0",
      elixir: "~> 1.5",
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      applications: [:complex_num]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:complex_num, "~> 1.0.0"}
    ]
  end
end
