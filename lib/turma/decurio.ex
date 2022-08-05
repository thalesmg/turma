defmodule Turma.Decurio do
  use GenServer

  require Logger

  @prefix "turma-command:"
  @expected :"$expected"
  @returned :"$returned"
  @caller :"$caller"

  @type tag() :: binary()
  @type peer() :: binary()
  @type job_id() :: reference()
  @type inventory() :: %{tag() => [peer()]}
  @type start_opts() :: %{
          inventory: inventory(),
          name: binary()
        }
  @type result() ::
          :pending
          | {:done, term()}
          | {:error, term()}
          | {:throw, term()}

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @spec run((() -> term())) :: {:ok, job_id()}
  def run(fun) when is_function(fun, 0) do
    run(:all, fun)
  end

  @spec run(binary(), (() -> term())) :: {:ok, job_id()}
  def run("" <> tag, fun) when is_function(fun, 0) do
    run({:tags, [tag]}, fun)
  end

  @spec run([binary()], (() -> term())) :: {:ok, job_id()}
  def run(tags, fun) when is_list(tags) and is_function(fun, 0) do
    run({:tags, tags}, fun)
  end

  @spec run(Regex.t(), (() -> term())) :: {:ok, job_id()}
  def run(regex = %Regex{}, fun) when is_function(fun, 0) do
    run({:regex, regex}, fun)
  end

  @spec run((inventory() -> [peer()]), (() -> term())) :: {:ok, job_id()}
  def run(filter, fun) when is_function(filter, 1) and is_function(fun, 0) do
    run({:filter, filter}, fun)
  end

  @spec run(
          :all
          | {:tags, [tag()]}
          | {:regex, Regex.t()}
          | {:filter, (inventory() -> [peer()])},
          (() -> term())
        ) :: {:ok, job_id()}
  def run(tags, fun) when (is_tuple(tags) or is_atom(tags)) and is_function(fun, 0) do
    GenServer.call(__MODULE__, {:run, tags, fun})
  end

  @spec get(job_id()) :: :error | {:ok, %{peer() => result()}}
  def get(id) do
    GenServer.call(__MODULE__, {:get, id})
  end

  @spec get_all() :: %{job_id() => %{peer() => result()}}
  def get_all() do
    GenServer.call(__MODULE__, :get_all)
  end

  @spec set_inventory(inventory()) :: :ok
  def set_inventory(inventory = %{}) do
    GenServer.call(__MODULE__, {:set_inventory, inventory})
  end

  def bootstrap(%{
        desired_inventory: desired_inventory = %{},
        command: "" <> cmd
      }) do
    unless Process.whereis(__MODULE__) do
      raise "no decurio running!"
    end

    {:ok, sup} = Task.Supervisor.start_link()

    inverted_inventory =
      desired_inventory
      |> Stream.flat_map(fn {tag, endpoints} ->
        Enum.map(endpoints, &{&1, tag})
      end)
      |> Enum.dedup()

    {ok, failed} =
      Task.Supervisor.async_stream_nolink(
        sup,
        inverted_inventory,
        fn {endpoint, tag} ->
          [host, _] = String.split(endpoint, ":", parts: 2)

          if host != "localhost" do
            {_, 0} =
              System.cmd(
                "ssh",
                [host, "-o", "StrictHostKeyChecking=no", "bash", "-c", "\"#{cmd}\""],
                stderr_to_stdout: true
              )
          end

          {endpoint, tag}
        end,
        max_concurrency: 10,
        timeout: 60_000,
        on_timeout: :kill_task
      )
      |> Enum.split_with(&match?({:ok, _}, &1))

    inventory =
      ok
      |> Stream.map(fn {:ok, {endpoint, tag}} -> {tag, endpoint} end)
      |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
      |> Map.update("decurio", [Application.get_env(Turma.Legionarius, :id)], fn vals ->
        [Application.get_env(Turma.Legionarius, :id) | vals]
      end)

    :ok = set_inventory(inventory)

    %{inventory: inventory, failed: failed}
  end

  @impl GenServer
  def init(opts) do
    inventory = Map.get(opts, :inventory, %{})
    my_name = Map.get(opts, :name, "decurio")
    {:ok, router_sock} = :chumak.socket(:router)

    dealer_sock =
      case :chumak.socket(:dealer, to_charlist(my_name)) do
        {:ok, dealer_sock} -> dealer_sock
        {:error, {:already_started, dealer_sock}} -> dealer_sock
      end

    connect_all(inventory, router_sock, dealer_sock)
    receiver_pid = spawn_link(__MODULE__, :receiver_loop, [self(), dealer_sock])

    {:ok,
     %{
       my_name: my_name,
       inventory: inventory,
       router_sock: router_sock,
       dealer_sock: dealer_sock,
       receiver_pid: receiver_pid,
       responses: %{}
     }}
  end

  @impl GenServer
  def handle_call(
        {:run, selector, fun},
        _from = {caller, _tag},
        state = %{router_sock: router_sock, my_name: my_name}
      ) do
    id = :erlang.make_ref()

    peers = match_peers(state.inventory, selector)

    Enum.each(peers, fn peer ->
      packet = [peer, @prefix, :erlang.term_to_binary({:run, id, my_name, fun})]
      :chumak.send_multipart(router_sock, packet)
    end)

    results =
      peers
      |> Map.new(&{&1, :pending})
      |> Map.put(@expected, length(peers))
      |> Map.put(@returned, 0)
      |> Map.put(@caller, caller)

    responses = Map.put(state.responses, id, results)
    state = %{state | responses: responses}

    {:reply, {:ok, id}, state}
  end

  def handle_call({:set_inventory, inventory}, _from, state) do
    state = %{state | inventory: inventory}
    # FIXME: diff inventory and disconnect from old hosts?!?!
    connect_all(inventory, state.router_sock, state.dealer_sock)
    {:reply, :ok, state}
  end

  def handle_call({:get, id}, _from, state = %{responses: responses}) do
    res =
      with {:ok, results} <- Map.fetch(responses, id) do
        {:ok, clean_results(results)}
      end

    {:reply, res, state}
  end

  def handle_call(:get_all, _from, state = %{responses: responses}) do
    res = Map.new(responses, fn {k, v} -> {k, clean_results(v)} end)
    {:reply, res, state}
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

        results =
          state.responses
          |> Map.fetch!(id)
          |> Map.put(responder, res)
          |> Map.update!(@returned, &(&1 + 1))

        responses = Map.put(state.responses, id, results)

        state = %{state | responses: responses}

        if results[@expected] == results[@returned] do
          send(results[@caller], {:job_finished, id})
        end

        {:noreply, state}

      x ->
        Logger.info("weird result: #{inspect(x, pretty: true)}")
        {:noreply, state}
    end
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end

  defp connect_all(inventory, router_sock, dealer_sock) do
    inventory
    |> Map.values()
    |> Enum.dedup()
    |> Stream.flat_map(& &1)
    |> Enum.each(fn host ->
      {host, base_port} = parse_host(host)
      :chumak.connect(router_sock, :tcp, to_charlist(host), base_port)
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

  def match_peers(inventory, :all) do
    inventory
    |> Map.values()
    |> Stream.flat_map(& &1)
    |> Enum.dedup()
  end

  def match_peers(inventory, {:tags, tags}) do
    inventory
    |> Map.take(tags)
    |> Map.values()
    |> Stream.flat_map(& &1)
    |> Enum.dedup()
  end

  def match_peers(inventory, {:regex, regex}) do
    inventory
    |> Map.values()
    |> Stream.flat_map(& &1)
    |> Stream.filter(&(&1 =~ regex))
    |> Enum.dedup()
  end

  def match_peers(inventory, {:filter, filter}) do
    all =
      inventory
      |> match_peers(:all)
      |> MapSet.new()

    inventory
    |> filter.()
    |> Stream.filter(&MapSet.member?(all, &1))
    |> Enum.dedup()
  end

  defp clean_results(results) do
    Map.drop(results, [@returned, @caller, @expected])
  end
end
