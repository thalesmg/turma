defmodule Turma.Decurio do
  use GenServer

  require Logger
  require Pingado

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
  @type selector() ::
          :all
          | [tag()]
          | Regex.t()
          | (inventory() -> [peer()])
  @type result() ::
          :pending
          | {:done, term()}
          | {:error, term()}
          | {:throw, term()}
  @type state() :: %{
          my_name: binary(),
          inventory: inventory(),
          router_sock: pid(),
          dealer_sock: pid(),
          receiver_pid: pid(),
          responses: %{job_id() => %{peer() => result()}}
        }

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

  @spec cancel(job_id()) :: :ok
  @spec cancel(job_id(), selector()) :: :ok
  def cancel(id, selector \\ :all) do
    GenServer.call(__MODULE__, {:cancel, id, to_selector(selector)})
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
    Process.flag(:trap_exit, true)
    inventory = Map.get(opts, :inventory, %{})
    my_name = Map.get(opts, :name, "decurio")
    {:ok, router_sock} = :chumak.socket(:router)
    {:ok, dealer_sock} = :chumak.socket(:dealer, to_charlist(my_name))

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

  def handle_call({:cancel, id, selector}, _from, state) do
    {reply, state} =
      case Map.fetch(state.responses, id) do
        {:ok, results} ->
          %{
            cancel_packets: cancel_packets,
            canceled_results: canceled_results,
            remaining: remaining,
            notify_caller?: notify_caller?,
            responses: responses
          } = cancel_job(id, selector, results, state)

          Enum.each(cancel_packets, fn packet ->
            :chumak.send_multipart(state.router_sock, packet)
          end)

          if notify_caller? do
            send(remaining[@caller], {:job_finished, id})
          end

          {canceled_results, %{state | responses: responses}}

        :error ->
          {nil, state}
      end

    {:reply, {:ok, reply}, state}
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

        # FIXME: race condition: we receive a response for a cancelled
        # job.
        results =
          state.responses
          |> Map.fetch!(id)
          |> Map.put(responder, res)
          |> Map.update!(@returned, &(&1 + 1))

        responses = Map.put(state.responses, id, results)

        state = %{state | responses: responses}

        if results[@expected] == results[@returned] do
          Pingado.tp(:job_finished, %{id: id, results: results})
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

  @impl GenServer
  def terminate(_reason, state) do
    :chumak.stop(state.dealer_sock)
    :chumak.stop(state.router_sock)
    state
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

  @spec match_peers(inventory(), selector()) :: [peer()]
  def match_peers(inventory, :all) do
    inventory
    |> Map.values()
    |> Stream.flat_map(& &1)
    |> Enum.uniq()
  end

  def match_peers(inventory, {:tags, tags}) do
    inventory
    |> Map.take(tags)
    |> Map.values()
    |> Stream.flat_map(& &1)
    |> Enum.uniq()
  end

  def match_peers(inventory, {:regex, regex}) do
    inventory
    |> Map.values()
    |> Stream.flat_map(& &1)
    |> Stream.filter(&(&1 =~ regex))
    |> Enum.uniq()
  end

  def match_peers(inventory, {:filter, filter}) do
    all =
      inventory
      |> match_peers(:all)
      |> MapSet.new()

    inventory
    |> filter.()
    |> Stream.filter(&MapSet.member?(all, &1))
    |> Enum.uniq()
  end

  @spec cancel_job(
          job_id(),
          selector(),
          %{peer() => result()},
          state()
        ) :: %{
          cancel_packets: [iodata()],
          remaining: %{peer() => result()},
          notify_caller?: boolean(),
          responses: %{job_id() => %{peer() => result()}}
        }
  def cancel_job(job_id, selector, results, state) do
    canceled_results =
      results
      |> clean_results()
      |> Map.take(match_peers(state.inventory, selector))

    matched = Map.keys(canceled_results)

    cancel_packets =
      Enum.map(canceled_results, fn {peer, _} ->
        [peer, @prefix, :erlang.term_to_binary({:cancel, job_id, state.my_name})]
      end)

    remaining =
      Enum.reduce(matched, results, fn peer, acc ->
        acc
        |> Map.update!(@expected, &(&1 - 1))
        |> Map.update!(@returned, fn x ->
          case acc[peer] do
            :pending -> x
            _ -> x - 1
          end
        end)
        |> Map.delete(peer)
      end)

    rem_exp = Map.fetch!(remaining, @expected)
    rem_ret = Map.fetch!(remaining, @returned)
    notify_caller? = rem_exp > 0 and rem_exp == rem_ret

    responses =
      if rem_exp == 0 do
        Map.delete(state.responses, job_id)
      else
        Map.put(state.responses, job_id, remaining)
      end

    %{
      cancel_packets: cancel_packets,
      canceled_results: canceled_results,
      remaining: remaining,
      notify_caller?: notify_caller?,
      responses: responses
    }
  end

  defp to_selector(:all) do
    :all
  end

  defp to_selector("" <> tag) do
    to_selector([tag])
  end

  defp to_selector(tags) when is_list(tags) do
    {:tags, tags}
  end

  defp to_selector(regex = %Regex{}) do
    {:regex, regex}
  end

  defp to_selector(filter) when is_function(filter, 1) do
    {:filter, filter}
  end

  defp clean_results(results) do
    Map.drop(results, [@returned, @caller, @expected])
  end
end
