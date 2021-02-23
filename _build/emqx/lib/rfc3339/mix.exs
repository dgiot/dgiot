defmodule RFC3339.Mixfile do
  use Mix.Project

  @version "0.2.1-no-maps"

  def project do
    [app: :rfc3339,
     version: @version,
     description: description,
     elixir: "~> 1.2",
     package: package,
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: []]
  end

  defp description do
    "an rfc3339 parser and formatter"
  end

  defp package do
    [maintainers: ["alisdair sullivan"],
     licenses: ["Apache 2.0", "MIT"],
     links: %{"Github" => "https://github.com/talentdeficit/rfc3339"},
     files: ["src", "lib", "README.md", "LICENSE*", "mix.exs", "rebar.config"]]
  end

  defp deps do
    []
  end
end
