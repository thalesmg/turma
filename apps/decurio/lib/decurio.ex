defmodule Decurio do
  use GenServer

  require Logger

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(_) do
    {:ok, %{known_nodes: MapSet.new(), requests: %{}}}
  end

  def add_nodes(nodes) when is_list(nodes) do
    GenServer.call(__MODULE__, {:add_nodes, nodes})
  end

  def run(fun) when is_function(fun, 0) do
    GenServer.call(__MODULE__, {:run, fun})
  end

  def get(id) do
    GenServer.call(__MODULE__, {:get, id})
  end

  def handle_call({:add_nodes, nodes}, _from, state) do
    connected_nodes =
      for n <- nodes,
          :net_kernel.hidden_connect_node(n),
          into: MapSet.new() do
        n
      end

    known_nodes = MapSet.union(state.known_nodes, connected_nodes)

    {:reply, {:ok, connected_nodes}, %{state | known_nodes: known_nodes}}
  end

  def handle_call({:run, fun}, _from, state) do
    id = gen_id()

    running_nodes =
      Map.new(state.known_nodes, fn node ->
        {
          node,
          :erlang.spawn_request(
            node,
            Legionarius,
            :wrap,
            [id, fun],
            [:monitor]
          )
        }
      end)

    requests = Map.put(state.requests, id, running_nodes)

    {
      :reply,
      {:ok, %{id: id, running_nodes: running_nodes}},
      %{state | requests: requests}
    }
  end

  def handle_call({:get, id}, _from, state) do
    case Map.fetch(state.requests, id) do
      {:ok, running_nodes} ->
        {res, bad_nodes} =
          running_nodes
          |> Map.keys()
          |> GenServer.multi_call(Legionarius, {:get, id})

        res =
          for {n, {:ok, res}} <- res, into: [] do
            {n, res}
          end

        {:reply, {:ok, %{id: id, responses: res, bad_nodes: bad_nodes}}, state}

      :error ->
        {:reply, {:error, :bad_request}, state}
    end
  end

  def handle_info(msg, state) do
    Logger.debug("#{__MODULE__} got " <> inspect(msg, pretty: true))
    {:noreply, state}
  end

  defp gen_id() do
    [?a..?z, ?A..?Z, ?0..?9]
    |> Enum.concat()
    |> Enum.take_random(16)
    |> to_string()
  end
end
