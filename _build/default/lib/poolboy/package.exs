defmodule Poolboy.Mixfile do
  use Mix.Project

  @version File.read!("VERSION") |> String.strip

  def project do
    [app: :poolboy,
     version: @version,
     description: "A hunky Erlang worker pool factory",
     package: package]
  end

  defp package do
    [files: ~w(src rebar.config README.md LICENSE UNLICENSE VERSION),
     contributors: ["Devin Torres", "Andrew Thompson", "Kurt Williams"],
     licenses: ["Unlicense", "Apache 2.0"],
     links: %{"GitHub" => "https://github.com/devinus/poolboy"}]
  end
end
