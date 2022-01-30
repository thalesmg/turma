defmodule PraefectusWeb.Live.Control do
  use PraefectusWeb, :live_view

  def mount(_params, _session, socket) do
    IO.inspect(binding())

    socket =
      assign(
        socket,
        connected?: false,
        decurio_server: nil,
        nodes: "",
        fun: "",
        requests: %{}
      )

    {:ok, socket}
  end

  def handle_info(msg = {:done, node, id, result}, socket) do
    IO.inspect(binding())

    socket =
      socket
      |> put_flash(:info, "results came in! #{inspect(msg, pretty: true)}")
      |> assign(
        requests:
          put_in(
            socket.assigns.requests,
            [id, node],
            result
          )
      )

    {:noreply, socket}
  end

  def handle_info(msg, socket) do
    IO.inspect(binding())
    {:noreply, socket}
  end

  def handle_event("connect", params, socket) do
    IO.inspect(binding())

    node =
      params
      |> Map.fetch!("decurio_server")
      |> String.to_atom()

    connected? =
      node
      |> :net_kernel.hidden_connect_node()
      |> IO.inspect()

    socket =
      if connected? do
        :gen_event.add_sup_handler(
          {Decurio.Notifier, node},
          Decurio.Notifier,
          self()
        )
        |> IO.inspect()

        put_flash(socket, :info, "Connected to #{node}")
      else
        put_flash(socket, :error, "Could not connect to #{node}")
      end

    {:noreply, assign(socket, decurio_server: node, connected?: connected?)}
  end

  def handle_event("change_nodes", params, socket) do
    {:noreply, assign(socket, nodes: Map.get(params, "nodes", ""))}
  end

  def handle_event("add_nodes", params, socket) do
    nodes =
      socket.assigns.nodes
      |> String.split("\n")
      |> Enum.map(&String.to_atom/1)

    res =
      :erpc.call(
        socket.assigns.decurio_server,
        Decurio,
        :add_nodes,
        [nodes]
      )
      |> IO.inspect()

    {:noreply, put_flash(socket, :info, "#{inspect(res, pretty: true)}")}
  end

  def handle_event("change_fun", params, socket) do
    {:noreply, assign(socket, fun: Map.get(params, "fun", ""))}
  end

  def handle_event("run", params, socket) do
    socket =
      with {:ok, fun} when is_function(fun, 0) <- parse_fun(socket.assigns.fun) do
        res =
          :erpc.call(
            socket.assigns.decurio_server,
            Decurio,
            :run,
            [fun]
          )
          |> IO.inspect()

        socket
        |> put_flash(:info, "running #{inspect(res, pretty: true)}")
        |> assign(
          requests:
            Map.put(
              socket.assigns.requests,
              res.id,
              res.running_nodes
            )
        )
      else
        _ ->
          put_flash(socket, :error, "something bad happened")
      end

    {:noreply, socket}
  end

  def handle_event("update_request", params, socket) do
    IO.inspect(binding())
    id = Map.fetch!(params, "id")

    socket =
      with {:ok, %{responses: responses}} <-
             :erpc.call(
               socket.assigns.decurio_server,
               Decurio,
               :get,
               [id]
             )
             |> IO.inspect() do
        requests =
          Map.put(
            socket.assigns.requests,
            id,
            responses
          )

        assign(socket, requests: requests)
      else
        _ ->
          socket
      end

    {:noreply, socket}
  end

  defp parse_fun(fun_str) do
    try do
      {fun, []} = Code.eval_string("fn -> \n" <> fun_str <> "\n end")
      {:ok, fun}
    rescue
      e ->
        IO.inspect(e)
        :error
    end
  end
end
