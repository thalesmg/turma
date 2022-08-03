defmodule Turma.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    mdecurio =
      if Application.get_env(:turma, :decurio, false) do
        [
          %{
            id: Turma.Decurio,
            start:
              {Turma.Decurio, :start_link,
               [
                 %{
                   inventory: Application.get_env(Turma.Decurio, :inventory, %{}),
                   name: Application.get_env(Turma.Decurio, :name, "decurio")
                 }
               ]},
            restart: :permanent,
            type: :worker
          }
        ]
      else
        []
      end

    {bind_iface, port} = Application.get_env(Turma.Legionarius, :bind, {"0.0.0.0", 19876})

    children =
      [
        # Starts a worker by calling: Turma.Worker.start_link(arg)
        # {Turma.Worker, arg}
        %{
          id: Turma.Legionarius,
          start:
            {Turma.Legionarius, :start_link,
             [
               %{
                 id: Application.get_env(Turma.Legionarius, :id, hostname(port)),
                 bind: {bind_iface, port}
               }
             ]},
          restart: :permanent,
          type: :worker
        }
      ] ++ mdecurio

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Turma.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp hostname(port) do
    {:ok, host} = :inet.gethostname()
    to_string(host) <> ":" <> to_string(port)
  end
end
