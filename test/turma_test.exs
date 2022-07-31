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

    inventory =
      Map.new(nodes, fn n ->
        host = "localhost:#{Map.fetch!(binds, n)}"
        {host, []}
      end)

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

      subs =
        if n == dec_node do
          ["decurio"]
        else
          ["legionarius"]
        end

      :ok = :erpc.call(n, Application, :put_env, [Legionarius, :subscriptions, subs])
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
       inventory: inventory
     }}
  end

  test "success", %{nodes: nodes, dec: dec} do
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
        {n, {:ok, n}}
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

  test "error", %{nodes: nodes, dec: dec} do
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
        {n, {:error, :boom}}
      end)

    assert :erpc.call(
             dec,
             Decurio,
             :get,
             [req_id]
           ) == {:ok, expected}
  end

  test "raise", %{nodes: nodes, dec: dec} do
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
        {n, {:error, %RuntimeError{message: "boom"}}}
      end)

    assert :erpc.call(
             dec,
             Decurio,
             :get,
             [req_id]
           ) == {:ok, expected}
  end

  test "throw", %{nodes: nodes, dec: dec} do
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
        {n, {:throw, :boom}}
      end)

    assert :erpc.call(
             dec,
             Decurio,
             :get,
             [req_id]
           ) == {:ok, expected}
  end

  test "subscriptions", %{legs: legs, dec: dec} do
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
        {n, {:ok, n}}
      end)

    assert :erpc.call(
             dec,
             Decurio,
             :get,
             [req_id]
           ) == {:ok, expected}
  end

  test "set_inventory", %{legs: legs, binds: binds, dec: dec} do
    inv =
      Map.new(legs, fn leg ->
        {"localhost:#{Map.fetch!(binds, leg)}", ["legionarius"]}
      end)

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
               ["legionarius", &Utils.success/0]
             )

    Process.sleep(1_000)

    expected =
      Map.new(legs, fn n ->
        {n, {:ok, n}}
      end)

    assert :erpc.call(
             dec,
             Decurio,
             :get,
             [req_id]
           ) == {:ok, expected}
  end
end
