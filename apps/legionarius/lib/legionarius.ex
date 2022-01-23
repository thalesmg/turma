defmodule Legionarius do
  use GenServer

  alias Legionarius.Result

  require Logger

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(_) do
    {:ok, %{jobs: %{}, mons: %{}}}
  end

  def register(id) do
    GenServer.call(__MODULE__, {:register, id, self()})
  end

  def get(n) do
    GenServer.call(__MODULE__, {:get, n})
  end

  def put_result(id, result) do
    GenServer.cast(__MODULE__, {:put_result, id, result})
  end

  def wrap(id, fun) do
    register(id)

    case fun.() do
      result = %Result{} ->
        put_result(id, result)
        exit({:done, result})

      other_res ->
        exit({:bad_result, other_res})
    end
  end

  def handle_call({:register, id, pid}, _from, state) do
    ref = Process.monitor(pid)
    state = put_in(state, [:mons, ref], id)
    {:reply, :ok, state}
  end

  def handle_call({:get, id}, _from, state) do
    {:reply, Map.fetch(state.jobs, id), state}
  end

  def handle_call(req, _from, state) do
    {:reply, {:bad_call, req}, state}
  end

  def handle_cast({:put_result, id, result}, state) do
    state = put_in(state, [:jobs, id], result)
    {:noreply, state}
  end

  def handle_cast(_req, state) do
    {:noreply, state}
  end

  def handle_info({:DOWN, ref, :process, _pid, result}, state) do
    result = normalize(result)

    state =
      case pop_in(state, [:mons, ref]) do
        {nil, state} ->
          state

        {id, state} ->
          put_in(state, [:jobs, id], result)
      end

    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  defp normalize(result) do
    case result do
      {:done, %Result{}} ->
        result

      {:bad_result, _error} ->
        result

      other ->
        {:bad_result, other}
    end
  end
end
