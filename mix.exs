defmodule Mix.Tasks.Compile.XalsaNif do
  def run(_args) do
    {result, _errcode} = System.cmd("make", [], cd: "c_src", stderr_to_stdout: true)
    Mix.Project.build_structure()
    IO.binwrite(result)
  end

  def clean(_args) do
    {result, _errcode} = System.cmd("make", ["clean"], cd: "c_src", stderr_to_stdout: true)
    IO.binwrite(result)
  end
end

defmodule Xalsa.MixProject do
  use Mix.Project

  def project do
    [
      app: :xalsa,
      compilers: [:xalsa_nif] ++ Mix.compilers(),
      version: "0.1.0",
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      package: package(),
      # Docs
      name: "Xalsa",
      source_url: "https://github.com/karlsson/xalsa",
      docs: [
        main: "Xalsa",
        extras: ["README.md"]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {:xalsa_app, []},
      env: [
	rate: 96000,
	pcms: ["plughw:1,0": 2]
      ],
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, "~> 0.20.2", only: :dev, runtime: false}
    ]
  end

  defp description do
    "An Elixir to ALSA Connector."
  end

  defp package do
    [
      maintainers: ["Mikael Karlsson"],
      licences: ["Apache 2.0"],
      links: %{"GitHub" => "https://github.com/karlsson/xalsa"}
    ]
  end
end
