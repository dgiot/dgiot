defmodule Eredis.Mixfile do
  use Mix.Project

  def project do
    [
      app: :eredis,
      version: "1.1.0",
      elixir: "~> 1.5.1",
      start_permanent: Mix.env == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    []
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    []
  end
end
