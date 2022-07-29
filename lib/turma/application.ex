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
                   inventory: Application.get_env(Turma.Decurio, :inventory),
                   name: Application.get_env(Turma.Decurio, :name)
                 }
               ]},
            restart: :permanent,
            type: :worker
          }
        ]
      else
        []
      end

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
                 bind: Application.get_env(Turma.Legionarius, :bind),
                 subscriptions: Application.get_env(Turma.Legionarius, :subscriptions)
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
end
