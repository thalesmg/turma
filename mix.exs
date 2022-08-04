defmodule Turma.MixProject do
  use Mix.Project

  def project do
    [
      app: :turma,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env()),
      aliases: [
        fmt: "format"
      ],
      releases: releases()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Turma.Application, []}
    ]
  end

  defp elixirc_paths(:test), do: ["test/support", "lib"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:chumak, "~> 1.4"},
      {:dialyxir, "~> 1.2", only: [:dev], runtime: false}
    ]
  end

  def releases() do
    [
      legionarius: [
        applications: [
          turma: :permanent
        ],
        config_providers: [
          {Config.Reader, {:system, "RELEASE_ROOT", "/etc/runtime.exs"}}
        ],
        steps: [:assemble, &prepare_tar_overlays/1, :tar]
      ],
      decurio: [
        applications: [
          turma: :permanent
        ],
        config_providers: [
          {Config.Reader, {:system, "RELEASE_ROOT", "/etc/runtime.exs"}}
        ],
        steps: [:assemble, &prepare_tar_overlays/1, :tar]
      ]
    ]
  end

  defp prepare_tar_overlays(release) do
    overwrite? = Keyword.get(release.options, :overwrite, false)

    etc = Path.join(release.path, "etc")

    Mix.Generator.copy_file(
      "config/runtime.exs",
      Path.join(etc, "runtime.exs"),
      force: overwrite?
    )

    Map.update!(release, :overlays, &["etc" | &1])
  end
end
