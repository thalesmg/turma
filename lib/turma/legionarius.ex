defmodule Turma.Legionarius do
  use GenServer

  require Logger

  @prefix "turma-command:"

  @type start_opts :: %{
          bind: {binary(), pos_integer()},
          subscriptions: [binary()]
        }

  @spec start_link(start_opts) :: {:ok, pid()} | {:error, term()}
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def set_subscriptions(subs) do
    subs = MapSet.new(subs)
    GenServer.cast(__MODULE__, {:set_subscriptions, subs})
  end

  @impl GenServer
  def init(opts) do
    {:ok, sub_sock} = :chumak.socket(:sub)
    {:ok, router_sock} = :chumak.socket(:router)
    {iface, base_port_num} = Map.get(opts, :bind, {"localhost", 9877})
    {:ok, _bind_pid0} = :chumak.bind(sub_sock, :tcp, to_charlist(iface), base_port_num)
    {:ok, _bind_pid1} = :chumak.bind(router_sock, :tcp, to_charlist(iface), base_port_num + 1)
    subscriptions = Map.get(opts, :subscriptions, [])
    subscriptions = MapSet.new(subscriptions)

    Enum.each(
      subscriptions
      |> MapSet.put("all")
      |> MapSet.put(hostname()),
      &:chumak.subscribe(sub_sock, [@prefix, &1])
    )

    # :chumak.subscribe(sub_sock, @prefix)
    receiver_pid = spawn_link(__MODULE__, :receiver_loop, [self(), sub_sock])

    {:ok,
     %{
       sub_sock: sub_sock,
       router_sock: router_sock,
       subscriptions: subscriptions,
       receiver_pid: receiver_pid
     }}
  end

  @impl GenServer
  def handle_call(_call, _from, state) do
    {:reply, {:error, :bad_call}, state}
  end

  @impl GenServer
  def handle_cast({:set_subscriptions, subscriptions}, state) do
    to_unsubscribe = MapSet.difference(state.subscriptions, subscriptions)
    Enum.each(to_unsubscribe, &:chumak.unsubscribe(state.sub_sock, [@prefix, &1]))
    Enum.each(
      subscriptions
      |> MapSet.put("all")
      |> MapSet.put(hostname()),
      &:chumak.subscribe(state.sub_sock, [@prefix, &1])
    )
    state = %{state | subscriptions: subscriptions}
    {:noreply, state}
  end
  def handle_cast(_cast, state) do
    {:noreply, state}
  end

  @impl GenServer
  def handle_info(
        {:data, receiver_pid, [@prefix <> _tag, data]},
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
        run(id, identity, fun)
        :ok

      x ->
        Logger.info("got something weird... #{inspect(x)}")
        :ok
    end

    {:noreply, state}
  end

  def handle_info({:done, id, decurio_identity, res}, state) do
    :chumak.send_multipart(state.router_sock, [
      decurio_identity,
      :erlang.term_to_binary({:result, id, node(), res})
    ])

    {:noreply, state}
  end

  def handle_info(msg, state) do
    Logger.warning("got strange message: #{inspect(msg, pretty: true)}")
    {:noreply, state}
  end

  def receiver_loop(parent, sub_sock) do
    case :chumak.recv_multipart(sub_sock) do
      {:ok, data} ->
        send(parent, {:data, self(), data})
        receiver_loop(parent, sub_sock)

      error ->
        Logger.error("legionarius receiver error: #{inspect(error, pretty: true)}")
        receiver_loop(parent, sub_sock)
    end
  end

  defp run(id, identity, fun) do
    parent = self()

    spawn(fn ->
      res =
        try do
          {:ok, fun.()}
        catch
          kind, err ->
            {kind, err}
        end

      send(parent, {:done, id, identity, res})
    end)
  end

  defp hostname() do
    {:ok, hostname} = :inet.gethostname()
    to_string(hostname)
  end
end
