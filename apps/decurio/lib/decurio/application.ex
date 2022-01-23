defmodule Decurio.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Starts a worker by calling: Decurio.Worker.start_link(arg)
      # {Decurio.Worker, arg}
      %{
        id: Decurio,
        start: {Decurio, :start_link, []}
      }
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Decurio.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
