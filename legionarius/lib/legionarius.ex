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
        exit({:done, id, result})

      other_res ->
        exit({:bad_result, id, other_res})
    end
  rescue
    err ->
      exit({:exception, id, {err, __STACKTRACE__}})
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
    state =
      with {id, result} <- normalize(result) do
        state
        |> Map.update!(:mons, &Map.delete(&1, ref))
        |> put_in([:jobs, id], result)
      else
        _ ->
          state
      end

    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  defp normalize(result) do
    case result do
      {:done, id, result = %Result{}} ->
        {id, {:done, result}}

      {:bad_result, id, error} ->
        {id, {:bad_result, error}}

      {:exception, id, error} ->
        {id, {:exception, error}}

      other ->
        {:bad_result, other}
    end
  end
end
