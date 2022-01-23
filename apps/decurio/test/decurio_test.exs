defmodule DecurioTest do
  use ExUnit.Case
  doctest Decurio

  alias Legionarius.Result

  setup_all do
    {:ok, host} = :inet.gethostname()
    {:ok, _} = Node.start(:test, :shortnames)

    {:ok, %{host: host}}
  end

  setup %{host: host} do
    args = '-pa #{Enum.join(:code.get_path(), " ")}'
    {:ok, leg_node1} = :slave.start_link(host, :leg1, args)
    {:ok, leg_node2} = :slave.start_link(host, :leg2, args)
    {:ok, dec_node} = :slave.start_link(host, :dec, args)
    legs = [leg_node1, leg_node2]

    on_exit(fn ->
      Enum.each([dec_node | legs], fn n ->
        :slave.stop(n)
      end)
    end)

    {:ok, _} =
      :erpc.call(
        dec_node,
        Application,
        :ensure_all_started,
        [:decurio]
      )

    [ok: _, ok: _] =
      :erpc.multicall(
        legs,
        Application,
        :ensure_all_started,
        [:legionarius]
      )

    {:ok,
     %{
       legs: legs,
       dec: dec_node
     }}
  end

  test "unknown id", %{dec: dec} do
    assert {:error, :bad_request} ==
             :erpc.call(
               dec,
               Decurio,
               :get,
               ["unknown id"]
             )
  end

  test "success", %{legs: legs, dec: dec} do
    assert {:ok, MapSet.new(legs)} ==
             :erpc.call(
               dec,
               Decurio,
               :add_nodes,
               [legs]
             )

    {:ok, %{id: id}} =
      :erpc.call(
        dec,
        Decurio,
        :run,
        [&Decurio.Test.Utils.simple_fn1/0]
      )

    Process.sleep(100)

    expected =
      Enum.map(legs, fn n ->
        {n, {:done, %Result{output: n}}}
      end)

    assert {:ok,
            %{
              bad_nodes: [],
              id: id,
              responses: expected
            }} == :erpc.call(dec, Decurio, :get, [id])
  end

  test "failure", %{legs: legs, dec: dec} do
    assert {:ok, MapSet.new(legs)} ==
             :erpc.call(
               dec,
               Decurio,
               :add_nodes,
               [legs]
             )

    {:ok, %{id: id}} =
      :erpc.call(
        dec,
        Decurio,
        :run,
        [&Decurio.Test.Utils.simple_fn2/0]
      )

    Process.sleep(100)

    expected =
      Enum.map(legs, fn n ->
        {n, {:bad_result, :unwrapped_result}}
      end)

    assert {:ok,
            %{
              bad_nodes: [],
              id: id,
              responses: expected
            }} == :erpc.call(dec, Decurio, :get, [id])
  end

  test "exception", %{legs: legs = [l1, l2], dec: dec} do
    assert {:ok, MapSet.new(legs)} ==
             :erpc.call(
               dec,
               Decurio,
               :add_nodes,
               [legs]
             )

    {:ok, %{id: id}} =
      :erpc.call(
        dec,
        Decurio,
        :run,
        [&Decurio.Test.Utils.simple_fn3/0]
      )

    Process.sleep(100)

    assert {:ok,
            %{
              bad_nodes: [],
              id: ^id,
              responses: [
                {^l1,
                 {
                   :bad_result,
                   {%RuntimeError{message: "some error"}, _stacktrace1}
                 }},
                {^l2,
                 {
                   :bad_result,
                   {%RuntimeError{message: "some error"}, _stacktrace2}
                 }}
              ]
            }} = :erpc.call(dec, Decurio, :get, [id])
  end
end
