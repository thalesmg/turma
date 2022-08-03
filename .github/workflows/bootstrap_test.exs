#!/usr/bin/env elixir

namespace = System.fetch_env!("NAMESPACE")

run = fn cmd ->
  {res, rc} =
    System.cmd(
      "kubectl",
      [
        "-n",
        namespace,
        "exec",
        "pod/node0",
        "--",
        "/decurio/bin/decurio",
        "rpc",
        cmd
      ]
    )

  IO.inspect(%{cmd: cmd, res: res, rc: rc})
  {res, rc}
end

dec_endpoint = "node0.#{namespace}.svc.cluster.local:19876"
leg_endpoint = "node1.#{namespace}.svc.cluster.local:19876"
cfg = (quote do
  config Turma.Legionarius,
    bind: {"0.0.0.0", 19876},
    id: unquote(leg_endpoint)
end
|> Macro.to_string()
|> Base.encode64())

{res, rc} =
  run.("""
  Turma.Decurio.bootstrap(%{
    desired_inventory: %{
      "legionarius" => ["#{leg_endpoint}"],
    },
    command:
      "mkdir -p /legionarius && tar -C /legionarius -xf \\\$(find /tmp/ -name legionarius-*.tar.gz) && echo #{cfg} | base64 -d - >> /legionarius/releases/*/runtime.exs && /legionarius/bin/legionarius daemon_iex"
  })
  |> IO.inspect()
  """)

IO.puts(res)
0 = rc

{%{
   failed: [],
   inventory: %{
     "decurio" => [^dec_endpoint],
     "legionarius" => [^leg_endpoint],
   }
 }, _} = Code.eval_string(res)

Process.sleep(10_000)

{res, rc} =
  run.("""
  {:ok, id} =
    Turma.Decurio.run(fn -> node() end)

  id
  |> :erlang.ref_to_list()
  |> IO.inspect()
  """)

IO.puts(res)
0 = rc
{id_cl, _} = Code.eval_string(res)

Process.sleep(10_000)

{res, rc} =
  run.("""
  '#{id_cl}'
  |> :erlang.list_to_ref()
  |> Turma.Decurio.get()
  |> IO.inspect()
  """)

IO.puts(res)
0 = rc

{{:ok,
  %{
    decurio@node0: {:ok, :decurio@node0},
    legionarius@node1: {:ok, :legionarius@node1}
  }}, _} = Code.eval_string(res)
