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

{res, rc} =
  run.("""
  Turma.Decurio.bootstrap(%{
    desired_inventory: %{
      "localhost:19876" => ["decurio"],
      "node1.#{namespace}.svc.cluster.local:19876" => ["legionarius"]
    },
    command:
      "mkdir -p /legionarius && tar -C /legionarius -xf /tmp/legionarius-*.tar.gz && /legionarius/bin/legionarius daemon_iex"
  })
  |> IO.inspect()
  """)

0 = rc

leg_endpoint = "node1.#{namespace}.svc.cluster.local:19876"
{%{
   failed: [],
   inventory: %{
     "localhost:19876" => ["decurio"],
     ^leg_endpoint => ["legionarius"]
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

0 = rc

{{:ok,
  %{
    decurio@node0: {:ok, :decurio@node0},
    legionarius@node1: {:ok, :legionarius@node1}
  }}, _} = Code.eval_string(res)
