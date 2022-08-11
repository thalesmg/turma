defmodule Turma.DecurioTest do
  use ExUnit.Case
  use ExUnitProperties

  alias Turma.Decurio
  alias Turma.Test.Generators

  @expected :"$expected"
  @returned :"$returned"
  @caller :"$caller"

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

  describe "cancel_job" do
    setup do
      state = %{
        my_name: "decurio",
        router_sock: :router,
        dealer_sock: :dealer,
        responses: %{}
      }

      %{
        state: state
      }
    end

    test "entire job", %{state: state} do
      %{
        state: state,
        job_id: job_id,
        results: results
      } = pick(cancel_job_gen(state))

      res = Decurio.cancel_job(job_id, :all, results, state)

      assert Map.fetch(res.responses, job_id) == :error, inspect(res, pretty: true)
    end

    property "difference", %{state: state0} do
      check all(
              %{
                state: state,
                job_id: job_id,
                selector: selector,
                results: results
              } <- cancel_job_gen(state0),
              max_runs: 50
            ) do
        res = Decurio.cancel_job(job_id, selector, results, state)

        results_before =
          state.responses
          |> Map.fetch!(job_id)
          |> clean_results()

        results_after =
          res.responses
          |> Map.get(job_id, %{})
          |> clean_results()

        remaining = clean_results(res.remaining)
        canceled_results = res.canceled_results

        assert Map.merge(remaining, canceled_results) == results_before
        assert Map.merge(results_after, canceled_results) == results_before
      end
    end

    property "reduces returned responses if not pending", %{state: state0} do
      check all(
              %{
                state: state,
                job_id: job_id,
                selector: selector,
                results: results
              } <- cancel_job_gen(state0),
              max_runs: 50
            ) do
        res = Decurio.cancel_job(job_id, selector, results, state)

        returned_before = Map.fetch!(results, @returned)
        returned_after = Map.fetch!(res.remaining, @returned)

        canceled_not_pending =
          results
          |> Map.drop(Map.keys(res.remaining))
          |> Enum.filter(fn {_, r} -> r != :pending end)
          |> Enum.count()

        assert returned_after + canceled_not_pending == returned_before
      end
    end

    property "reduces expected responses", %{state: state0} do
      check all(
              %{
                state: state,
                job_id: job_id,
                selector: selector,
                results: results
              } <- cancel_job_gen(state0),
              max_runs: 50
            ) do
        res = Decurio.cancel_job(job_id, selector, results, state)

        expected_before = Map.fetch!(results, @expected)
        expected_after = Map.fetch!(res.remaining, @expected)
        assert expected_after <= expected_before
      end
    end

    property "@returned is never greater than @expected", %{state: state0} do
      check all(
              %{
                state: state,
                job_id: job_id,
                selector: selector,
                results: results
              } <- cancel_job_gen(state0),
              max_runs: 50
            ) do
        res = Decurio.cancel_job(job_id, selector, results, state)

        returned = Map.fetch!(res.remaining, @returned)
        expected = Map.fetch!(res.remaining, @expected)
        assert returned <= expected
      end
    end

    property "(@expected > 0 /\ @returned = @expected) <-> notify_caller", %{state: state0} do
      check all(
              %{
                state: state,
                job_id: job_id,
                selector: selector,
                results: results
              } <- cancel_job_gen(state0),
              max_runs: 50
            ) do
        res = Decurio.cancel_job(job_id, selector, results, state)

        returned = Map.fetch!(res.remaining, @returned)
        expected = Map.fetch!(res.remaining, @expected)

        assert (expected > 0 and returned == expected) == res.notify_caller?
      end
    end
  end

  defp cancel_job_gen(state0) do
    gen all(
          inventory <- Generators.inventory(),
          jobs <- Generators.jobs(inventory),
          selector_type <- member_of([:all, :tag, :regex, :filter]),
          selector <- Generators.selector_for(inventory, selector_type),
          {job_id, results} <-
            if(map_size(jobs) > 0,
              do:
                one_of([
                  member_of(jobs),
                  tuple({Generators.job_id(), Generators.results(inventory)})
                ]),
              else: tuple({Generators.job_id(), Generators.results(inventory)})
            )
        ) do
      %{
        state: Map.merge(state0, %{responses: %{job_id => results}, inventory: inventory}),
        job_id: job_id,
        selector: selector,
        results: results
      }
    end
  end

  defp set(xs) do
    MapSet.new(xs)
  end

  defp clean_results(results) do
    Map.drop(results, [@returned, @caller, @expected])
  end
end
