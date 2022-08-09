defmodule Turma.Test.Generators do
  import ExUnitProperties
  import StreamData

  def result() do
    one_of([
      constant(:pending),
      tuple({constant(:done), term()}),
      tuple({constant(:error), term()}),
      tuple({constant(:throw), term()})
    ])
  end

  def peer() do
    gen all(
          host <- string(Enum.concat([?a..?z, ?A..?Z, ?0..?9, [?-, ?_]]), min_length: 1),
          port <- integer(1..65535)
        ) do
      host <> ":" <> to_string(port)
    end
  end

  def results(inv \\ nil) do
    case inv do
      nil ->
        peer()

      %{} ->
        ps =
          inv
          |> Map.values()
          |> Enum.concat()

        if ps == [] do
          peer()
        else
          member_of(ps)
        end
    end
    |> list_of()
    |> bind(fn ps ->
      ps
      |> Map.new(&{&1, pick(result())})
      |> constant()
    end)
    |> bind(fn m ->
      returned = Enum.count(m, fn {_peer, res} -> res != :pending end)

      m
      |> Map.put(:"$returned", returned)
      |> Map.put(:"$expected", map_size(m))
      |> constant()
    end)
  end

  def job_id() do
    constant(:ok)
    |> map(fn _ -> :erlang.make_ref() end)
    |> unshrinkable()
  end

  def jobs(inv \\ nil) do
    map_of(job_id(), results(inv))
  end

  def tag() do
    string(:ascii)
  end

  def selector() do
    one_of([
      constant(:all),
      tuple({:tags, list_of(tag())})
      # TODO!
      # tuple({:regex, _}),
      # TODO!
      # tuple({:filter, _}),
    ])
  end

  def inventory(opts \\ []) do
    # list_of(peer())
    # |> bind(fn peers ->
    #   tag()
    #   |> list_of(min_length: 1)
    #   |> bind(fn tags ->
    #     case peers do
    #       [] ->
    #         constant(Map.new(tags, &{&1, []}))

    #       _ ->
    #         member_of(peers)
    #         |> list_of()
    #         |> bind(fn subset ->
    #           constant(Map.new(tags, &{&1, subset}))
    #         end)
    #     end
    #   end)
    # end)

    gen all(
          peers <- list_of(peer(), opts),
          tags <- list_of(tag(), [{:min_length, 1} | opts]),
          subset <-
            case peers do
              [] -> constant([])
              _ -> list_of(member_of(peers))
            end
        ) do
      Map.new(tags, &{&1, subset})
    end
  end

  def selector_for(_inv, :all) do
    :all
  end

  def selector_for(inv, :tag) do
    inv
    |> Map.keys()
    |> case do
      [] ->
        constant({:tags, []})

      ts ->
        ts
        |> member_of()
        |> list_of()
        |> map(&{:tags, &1})
    end
  end

  def selector_for(inv, :regex) do
    peers =
      inv
      |> Map.values()
      |> Enum.concat()

    case peers do
      [] ->
        constant({:regex, ~r/^$/})

      _ ->
        peers
        |> member_of()
        |> map(&Regex.compile!/1)
        |> map(&{:regex, &1})
    end
  end

  def selector_for(inv, :filter) do
    peers =
      inv
      |> Map.values()
      |> Enum.concat()

    case peers do
      [] ->
        constant({:filter, fn _ -> [] end})

      _ ->
        peers
        |> member_of()
        |> list_of()
        |> map(fn ps ->
          {:filter, fn _ -> ps end}
        end)
    end
  end
end
