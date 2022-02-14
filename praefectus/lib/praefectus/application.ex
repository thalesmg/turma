defmodule Praefectus.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Start the Telemetry supervisor
      PraefectusWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: Praefectus.PubSub},
      # Start the Endpoint (http/https)
      PraefectusWeb.Endpoint
      # Start a worker by calling: Praefectus.Worker.start_link(arg)
      # {Praefectus.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Praefectus.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    PraefectusWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
