defmodule Xalsa.MixProject do
  use Mix.Project

  @target System.get_env("MIX_TARGET") || "host"

  def project do
    [
      app: :xalsa,
      make_cwd: "c_src",
      make_clean: ["clean"],
      compilers: [:elixir_make] ++ Mix.compilers(),
      version: "0.3.0",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      package: package(),
      # Docs
      name: "Xalsa",
      source_url: "https://github.com/karlsson/xalsa",
      docs: [
        main: "readme",
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
	rate: 44100,
	pcms: pcms(@target)
      ]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:elixir_make, "~> 0.6", runtime: false},
      {:ex_doc, "~> 0.22.1", only: :dev, runtime: false}
    ]
  end

  defp description do
    "An Elixir to ALSA Connector."
  end

  defp package do
    [
      maintainers: ["Mikael Karlsson"],
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/karlsson/xalsa"}
    ]
  end

  defp pcms("host"), do: ["plughw:PCH,0": [channels: 2]]
  defp pcms(_rpi), do: ["hw:0,1": [channels: 2, period_size: 1024, buffer_period_size_ratio: 3]]

end
