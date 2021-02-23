defmodule SSLVerifyFun.Mixfile do
  use Mix.Project

  def project do
    [app: :ssl_verify_fun,
     version: "1.1.4",
     description: description(),
     package: package()]
  end

  defp description() do
    """
    SSL verification functions for Erlang
    """
  end

  defp package() do
    [maintainers: ["Ilya Khaprov"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/deadtrickster/ssl_verify_fun.erl"},
     files: ["src", "README.md", "LICENSE", "Makefile", "rebar.config", "mix.exs"]]
  end
end
