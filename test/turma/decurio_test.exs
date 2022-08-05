defmodule Turma.DecurioTest do
  use ExUnit.Case

  alias Turma.Decurio

  describe "match_peers" do
    setup do
      inventory = %{
        "decurio" => [
          "decurio0:19876",
          "decurio1:19876"
        ],
        "legionarius" => [
          "legionarius0:19876",
          "legionarius1:19876",
          "legionarius1:19876",
          "other-legionarius:29876"
        ],
        "server" => [
          "my-server:19877",
          "decurio1:19876",
          "decurio2:19876"
        ]
      }

      %{inventory: inventory}
    end

    test "all", %{inventory: inventory} do
      assert set(Decurio.match_peers(inventory, :all)) ==
               set([
                 "decurio0:19876",
                 "decurio1:19876",
                 "legionarius0:19876",
                 "legionarius1:19876",
                 "other-legionarius:29876",
                 "my-server:19877",
                 "decurio2:19876"
               ])
    end

    test "tags", %{inventory: inventory} do
      assert set(Decurio.match_peers(inventory, {:tags, []})) == set([])

      assert set(Decurio.match_peers(inventory, {:tags, ["server"]})) ==
               set([
                 "decurio1:19876",
                 "my-server:19877",
                 "decurio2:19876"
               ])

      assert set(Decurio.match_peers(inventory, {:tags, ["server", "decurio"]})) ==
               set([
                 "decurio0:19876",
                 "decurio1:19876",
                 "my-server:19877",
                 "decurio2:19876"
               ])
    end

    test "regex", %{inventory: inventory} do
      assert set(Decurio.match_peers(inventory, {:regex, ~r/^$/})) == set([])

      assert set(Decurio.match_peers(inventory, {:regex, ~r/decurio/})) ==
               set([
                 "decurio0:19876",
                 "decurio1:19876",
                 "decurio2:19876"
               ])

      assert set(Decurio.match_peers(inventory, {:regex, ~r/(19877|29876)$/})) ==
               set([
                 "other-legionarius:29876",
                 "my-server:19877"
               ])
    end

    test "filter", %{inventory: inventory} do
      filter = fn inv ->
        inv
        |> Map.get("legionarius", [])
        |> Enum.take_every(2)
      end

      assert set(Decurio.match_peers(inventory, {:filter, filter})) ==
               set([
                 "legionarius0:19876",
                 "legionarius1:19876"
               ])

      filter = fn inv ->
        inv
        |> Map.get("server", [])
        |> Enum.take_every(2)
      end

      assert set(Decurio.match_peers(inventory, {:filter, filter})) ==
               set([
                 "my-server:19877",
                 "decurio2:19876"
               ])

      filter = fn _inv ->
        ["new-peer:19877"]
      end

      assert set(Decurio.match_peers(inventory, {:filter, filter})) == set([])
    end
  end

  defp set(xs) do
    MapSet.new(xs)
  end
end
