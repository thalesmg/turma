defmodule Turma.Decurio do
  use GenServer

  require Logger

  @prefix "turma-command:"

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def run(fun) when is_function(fun, 0) do
    run("all", fun)
  end

  def run("" <> tag, fun) when is_function(fun, 0) do
    GenServer.call(__MODULE__, {:run, tag, fun})
  end

  def get(id) do
    GenServer.call(__MODULE__, {:get, id})
  end

  def get_all() do
    GenServer.call(__MODULE__, :get_all)
  end

  def set_inventory(inventory = %{}) do
    GenServer.call(__MODULE__, {:set_inventory, inventory})
  end

  @impl GenServer
  def init(opts) do
    inventory = Map.get(opts, :inventory, %{})
    my_name = Map.get(opts, :name, "decurio")
    {:ok, pub_sock} = :chumak.socket(:pub)
    {:ok, dealer_sock} = :chumak.socket(:dealer, to_charlist(my_name))
    connect_all(inventory, pub_sock, dealer_sock)
    receiver_pid = spawn_link(__MODULE__, :receiver_loop, [self(), dealer_sock])

    {:ok,
     %{
       my_name: my_name,
       inventory: inventory,
       pub_sock: pub_sock,
       dealer_sock: dealer_sock,
       receiver_pid: receiver_pid,
       responses: %{}
     }}
  end

  @impl GenServer
  def handle_call({:run, tag, fun}, _from, state = %{pub_sock: pub_sock, my_name: my_name})
      when is_function(fun, 0) do
    if is_function(fun, 0) do
      # fixme: store this in state
      id = :erlang.make_ref()
      packet = [@prefix <> tag, :erlang.term_to_binary({:run, id, my_name, fun})]
      :chumak.send_multipart(pub_sock, packet)
      {:reply, {:ok, id}, state}
    else
      {:reply, {:error, {:bad_fun, fun}}, state}
    end
  end

  def handle_call({:set_inventory, inventory}, _from, state) do
    state = %{state | inventory: inventory}
    # FIXME: diff inventory and disconnect from old hosts?!?!
    connect_all(inventory, state.pub_sock, state.dealer_sock)
    {:reply, :ok, state}
  end

  def handle_call({:get, id}, _from, state = %{responses: responses}) do
    {:reply, Map.fetch(responses, id), state}
  end

  def handle_call(:get_all, _from, state = %{responses: responses}) do
    {:reply, responses, state}
  end

  def handle_call(_call, _from, state) do
    {:reply, {:error, :bad_call}, state}
  end

  @impl GenServer
  def handle_cast(_cast, state) do
    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:response, receiver_pid, data}, state = %{receiver_pid: receiver_pid}) do
    Logger.info("got a response! #{inspect(data, pretty: true)}")
    # fixme: unsafe!
    case :erlang.binary_to_term(data) do
      {:result, id, responder, res} when is_reference(id) ->
        Logger.info(
          "response for #{inspect(id)}, #{inspect(responder)}: #{inspect(res, pretty: true)}"
        )

        responses =
          Map.update(state.responses, id, %{responder => res}, fn old ->
            Map.put(old, responder, res)
          end)

        state = %{state | responses: responses}
        {:noreply, state}

      x ->
        Logger.info("weird result: #{inspect(x, pretty: true)}")
        {:noreply, state}
    end
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end

  defp connect_all(inventory, pub_sock, dealer_sock) do
    Enum.each(inventory, fn {host, _tags} ->
      {host, base_port} = parse_host(host)
      :chumak.connect(pub_sock, :tcp, to_charlist(host), base_port)
      :chumak.connect(dealer_sock, :tcp, to_charlist(host), base_port + 1)
    end)
  end

  defp parse_host(host) do
    [host, port] = String.split(host, ":", parts: 2)
    port = String.to_integer(port)
    {host, port}
  end

  def receiver_loop(parent, dealer_sock) do
    case :chumak.recv_multipart(dealer_sock) do
      {:ok, [data]} ->
        send(parent, {:response, self(), data})
        receiver_loop(parent, dealer_sock)

      error ->
        Logger.error("decurio receiver error: #{inspect(error, pretty: true)}")
        receiver_loop(parent, dealer_sock)
    end
  end
end
