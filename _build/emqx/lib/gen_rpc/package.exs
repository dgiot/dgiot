defmodule GenRPC.Mixfile do
  use Mix.Project

  def project do
    [app: :gen_rpc,
     version: "2.0.0",
     description: "A scalable RPC library for Erlang-VM based languages",
     package: package]
  end

  defp package do
    [files: ~w(include src LICENSE Makefile package.exs README.md TODO.md CHANGELOG.md rebar.config rebar.config.script otp-release.escript),
     maintainers: ["Panagiotis PJ Papadomitsos"],
     licenses: ["Apache 2.0"],
     links: %{"GitHub" => "https://github.com/priestjim/gen_rpc"}]
   end
end

