defmodule Counters.Mixfile do
  use Mix.Project

  def project do
    [app: :counters,
     version: "0.2.1",
     description: description(),
     package: package()]
  end

  defp description do
    """
    Counters for BEAM.
    """
  end

  def application do
    [mod: { :counters, [] }]
  end

  defp package do
    [build_tools: ["rebar3", "mix"],
     maintainers: ["Ilya Khaprov <i.khaprov@gmail.com>"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/deadtrickster/counters.erl"},
     files: ["mix.exs", "src", "include", "README.md", "LICENSE", "rebar.config"]]
  end
end
