defmodule Prometheus.Mixfile do
  use Mix.Project

  def project do
    [app: :prometheus,
     version: "3.1.0",
     description: description,
     package: package]
  end

  defp description do
    """
    Prometheus monitoring system and time series database client in Erlang.
    """
  end

  defp package do
    [build_tools: ["rebar3"],
     maintainers: ["Ilya Khaprov"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/deadtrickster/prometheus.erl",
              "Prometheus.ex" => "https://hex.pm/packages/prometheus_ex",
              "Ecto Instrumenter" => "https://hex.pm/packages/prometheus_ecto",
              "Phoenix Instrumenter" => "https://hex.pm/packages/prometheus_phoenix",
              "Plugs Instrumenter/Exporter" => "https://hex.pm/packages/prometheus_plugs",
              "Process info Collector" => "https://hex.pm/packages/prometheus_process_collector"},
     files: ["bin", "src", "include", "README.md", "LICENSE", "rebar.config"]]
  end
end
