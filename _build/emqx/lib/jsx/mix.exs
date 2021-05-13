defmodule JSX.Mixfile do
use Mix.Project

  def project do
    [
      app: :jsx,
      version: "2.9.0",
      description: "an erlang application for consuming, producing and manipulating json. inspired by yajl",
      deps: deps(Mix.env),
      package: package(),
      language: :erlang,
      erlc_options: opts(Mix.env)
    ]
  end

  defp opts(:dev), do: [d: :TEST] ++ opts(:prod)
  defp opts(_) do
    force_maps = case System.get_env("JSX_FORCE_MAPS") do
      nil -> []
      _   -> [d: :maps_always]
    end
    [:debug_info, d: :maps_support] ++ force_maps
  end

  defp deps(_), do: [{:mixunit, "~> 0.9.2", only: :dev}]

  defp package do
    [
      files: [
        "CHANGES.md",
        "LICENSE",
        "mix.exs",
        "rebar.config",
        "rebar.config.script",
        "README.md",
        "src"
      ],
      contributors: ["alisdair sullivan"],
      links: %{"github" => "https://hub.fastgit.org/talentdeficit/jsx"},
      licenses: ["MIT"]
    ]
  end
end
