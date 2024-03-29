defmodule Turma.Legionarius do
  use GenServer

  require Logger

  @prefix "turma-command:"

  @type start_opts :: %{
          id: binary(),
          bind: {binary(), pos_integer()}
        }

  @spec start_link(start_opts) :: {:ok, pid()} | {:error, term()}
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl GenServer
  def init(opts) do
    Process.flag(:trap_exit, true)
    {:ok, dealer_sock} = :chumak.socket(:dealer, to_charlist(opts.id))
    {:ok, router_sock} = :chumak.socket(:router)
    {iface, base_port_num} = Map.get(opts, :bind, {"localhost", 9877})
    {:ok, _bind_pid0} = :chumak.bind(dealer_sock, :tcp, to_charlist(iface), base_port_num)
    {:ok, _bind_pid1} = :chumak.bind(router_sock, :tcp, to_charlist(iface), base_port_num + 1)

    receiver_pid = spawn_link(__MODULE__, :receiver_loop, [self(), dealer_sock])

    {:ok,
     %{
       jobs: %{},
       my_id: opts.id,
       dealer_sock: dealer_sock,
       router_sock: router_sock,
       receiver_pid: receiver_pid
     }}
  end

  @impl GenServer
  def handle_call(_call, _from, state) do
    {:reply, {:error, :bad_call}, state}
  end

  @impl GenServer
  def handle_cast(_cast, state) do
    {:noreply, state}
  end

  @impl GenServer
  def handle_info(
        {:data, receiver_pid, [@prefix, data]},
        state = %{receiver_pid: receiver_pid}
      ) do
    Logger.info("got data: #{inspect(data, pretty: true)}")
    # fixme: unsafe!
    case :erlang.binary_to_term(data) do
      {:run, id, identity, fun}
      when is_function(fun, 0) and
             is_binary(identity) and
             is_reference(id) ->
        Logger.info("running something...")
        job_pid = run(id, identity, fun)
        state = put_in(state, [:jobs, id], %{job_pid: job_pid})
        {:noreply, state}

      {:cancel, id, decurio_identity} ->
        Logger.info("cancelling #{inspect(id)} (#{inspect(decurio_identity)})...")
        job_info = state.jobs[id]

        try do
          Process.exit(job_info[:job_pid], :kill)
        rescue
          _ -> :ok
        end

        {_, state} = pop_in(state, [:jobs, id])
        {:noreply, state}

      x ->
        Logger.info("got something weird... #{inspect(x)}")
        {:noreply, state}
    end
  end

  def handle_info({:done, id, decurio_identity, res}, state) do
    :chumak.send_multipart(state.router_sock, [
      decurio_identity,
      :erlang.term_to_binary({:result, id, state.my_id, res})
    ])

    {_, state} = pop_in(state, [:jobs, id])
    {:noreply, state}
  end

  def handle_info(msg, state) do
    Logger.warning("got strange message: #{inspect(msg, pretty: true)}")
    {:noreply, state}
  end

  @impl GenServer
  def terminate(_reason, state) do
    :chumak.stop(state.dealer_sock)
    :chumak.stop(state.router_sock)
    state
  end

  def receiver_loop(parent, dealer_sock) do
    case :chumak.recv_multipart(dealer_sock) do
      {:ok, data} ->
        send(parent, {:data, self(), data})
        receiver_loop(parent, dealer_sock)

      error ->
        Logger.error("legionarius receiver error: #{inspect(error, pretty: true)}")
        receiver_loop(parent, dealer_sock)
    end
  end

  defp run(id, identity, fun) do
    parent = self()

    spawn_link(fn ->
      res =
        try do
          {:done, fun.()}
        catch
          kind, err ->
            {kind, err}
        end

      send(parent, {:done, id, identity, res})
    end)
  end
end
