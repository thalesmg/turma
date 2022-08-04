defmodule TurmaTest do
  use ExUnit.Case

  alias Turma.Decurio
  alias Turma.Legionarius
  alias Turma.Test.Utils

  setup_all do
    {:ok, host} = :inet.gethostname()
    {:ok, _} = Node.start(:test, :shortnames)

    {:ok, %{host: host}}
  end

  def start_slave(:legionarius, name, host) do
    args = '-pa #{Enum.join(:code.get_path(), " ")}'
    :slave.start_link(host, name, args)
  end

  def start_slave(:decurio, name, host) do
    args = '-pa #{Enum.join(:code.get_path(), " ")} -turma decurio true'
    :slave.start_link(host, name, args)
  end

  setup env = %{host: host} do
    {:ok, leg_node1} = start_slave(:legionarius, :leg1, host)
    {:ok, leg_node2} = start_slave(:legionarius, :leg2, host)
    {:ok, dec_node} = start_slave(:decurio, :dec, host)
    legs = [leg_node1, leg_node2]
    nodes = [dec_node | legs]

    on_exit(fn ->
      Enum.each(nodes, fn n ->
        :slave.stop(n)
      end)
    end)

    binds =
      nodes
      |> Enum.with_index()
      |> Map.new(fn {n, i} ->
        {n, 29876 + 2 * i}
      end)

    peers = Map.new(nodes, &{&1, "localhost:#{Map.fetch!(binds, &1)}"})

    inventory = %{
      "legionarius" =>
        Enum.map(legs, fn leg ->
          "localhost:#{Map.fetch!(binds, leg)}"
        end),
      "decurio" => ["localhost:#{Map.fetch!(binds, dec_node)}"]
    }

    Enum.each(nodes, fn n ->
      :ok =
        :erpc.call(
          n,
          Application,
          :load,
          [:turma]
        )
    end)

    set_inventory? = Map.get(env, :set_inventory?, true)

    if set_inventory? do
      :ok = :erpc.call(dec_node, Application, :put_env, [Decurio, :inventory, inventory])
    end

    :ok = :erpc.call(dec_node, Application, :put_env, [Decurio, :name, "decurio-test"])

    Enum.each(nodes, fn n ->
      :ok =
        :erpc.call(n, Application, :put_env, [
          Legionarius,
          :bind,
          {"localhost", Map.fetch!(binds, n)}
        ])

      :ok =
        :erpc.call(n, Application, :put_env, [
          Legionarius,
          :id,
          Map.fetch!(peers, n)
        ])
    end)

    Enum.each(nodes, fn n ->
      {:ok, _} =
        :erpc.call(
          n,
          Application,
          :ensure_all_started,
          [:turma]
        )
    end)

    Process.sleep(5_000)

    {:ok,
     %{
       nodes: nodes,
       legs: legs,
       dec: dec_node,
       binds: binds,
       peers: peers,
       inventory: inventory
     }}
  end

  test "success", %{nodes: nodes, peers: peers, dec: dec} do
    assert {:ok, req_id} =
             :erpc.call(
               dec,
               Decurio,
               :run,
               [&Utils.success/0]
             )

    Process.sleep(1_000)

    expected =
      Map.new(nodes, fn n ->
        {Map.fetch!(peers, n), {:done, n}}
      end)

    expected_all = %{
      req_id => expected
    }

    assert :erpc.call(
             dec,
             Decurio,
             :get,
             [req_id]
           ) == {:ok, expected}

    assert :erpc.call(
             dec,
             Decurio,
             :get_all,
             []
           ) == expected_all
  end

  test "pendings", %{nodes: nodes, peers: peers, dec: dec} do
    assert {:ok, req_id} =
             :erpc.call(
               dec,
               Decurio,
               :run,
               [&Utils.delayed_success/0]
             )

    expected_pending =
      Map.new(nodes, fn n ->
        {Map.fetch!(peers, n), :pending}
      end)

    assert :erpc.call(
             dec,
             Decurio,
             :get,
             [req_id]
           ) == {:ok, expected_pending}

    Process.sleep(1_500)

    expected_done =
      Map.new(nodes, fn n ->
        {Map.fetch!(peers, n), {:done, n}}
      end)

    assert :erpc.call(
             dec,
             Decurio,
             :get,
             [req_id]
           ) == {:ok, expected_done}
  end

  test "error", %{nodes: nodes, peers: peers, dec: dec} do
    assert {:ok, req_id} =
             :erpc.call(
               dec,
               Decurio,
               :run,
               [&Utils.error_fn/0]
             )

    Process.sleep(1_000)

    expected =
      Map.new(nodes, fn n ->
        {Map.fetch!(peers, n), {:error, :boom}}
      end)

    assert :erpc.call(
             dec,
             Decurio,
             :get,
             [req_id]
           ) == {:ok, expected}
  end

  test "raise", %{nodes: nodes, peers: peers, dec: dec} do
    assert {:ok, req_id} =
             :erpc.call(
               dec,
               Decurio,
               :run,
               [&Utils.raise_fn/0]
             )

    Process.sleep(1_000)

    expected =
      Map.new(nodes, fn n ->
        {Map.fetch!(peers, n), {:error, %RuntimeError{message: "boom"}}}
      end)

    assert :erpc.call(
             dec,
             Decurio,
             :get,
             [req_id]
           ) == {:ok, expected}
  end

  test "throw", %{nodes: nodes, peers: peers, dec: dec} do
    assert {:ok, req_id} =
             :erpc.call(
               dec,
               Decurio,
               :run,
               [&Utils.throw_fn/0]
             )

    Process.sleep(1_000)

    expected =
      Map.new(nodes, fn n ->
        {Map.fetch!(peers, n), {:throw, :boom}}
      end)

    assert :erpc.call(
             dec,
             Decurio,
             :get,
             [req_id]
           ) == {:ok, expected}
  end

  test "subscriptions", %{legs: legs, peers: peers, dec: dec} do
    assert {:ok, req_id} =
             :erpc.call(
               dec,
               Decurio,
               :run,
               ["legionarius", &Utils.success/0]
             )

    Process.sleep(1_000)

    expected =
      Map.new(legs, fn n ->
        {Map.fetch!(peers, n), {:done, n}}
      end)

    assert :erpc.call(
             dec,
             Decurio,
             :get,
             [req_id]
           ) == {:ok, expected}
  end

  @tag set_inventory?: false
  test "set_inventory", %{legs: [leg1 | _], peers: peers, binds: binds, dec: dec} do
    inv = %{
      "tag" => ["localhost:#{Map.fetch!(binds, leg1)}"]
    }

    assert :erpc.call(
             dec,
             Decurio,
             :set_inventory,
             [inv]
           ) == :ok

    Process.sleep(1_000)

    assert {:ok, req_id} =
             :erpc.call(
               dec,
               Decurio,
               :run,
               ["tag", &Utils.success/0]
             )

    Process.sleep(1_000)

    expected = %{Map.fetch!(peers, leg1) => {:done, leg1}}

    assert :erpc.call(
             dec,
             Decurio,
             :get,
             [req_id]
           ) == {:ok, expected}
  end
end
