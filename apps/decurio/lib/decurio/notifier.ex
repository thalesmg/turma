defmodule Decurio.Notifier do
  @behaviour :gen_event

  @impl :gen_event
  def init(watcher_pid) do
    {:ok, watcher_pid}
  end

  @impl :gen_event
  def handle_event({:done, node, id, result}, watcher_pid) do
    IO.inspect(binding(), label: ">>>>>>>>>>>")
    send(watcher_pid, {:done, node, id, result})
    {:ok, watcher_pid}
  end

  def handle_event(msg, watcher_pid) do
    IO.inspect(msg, label: "unknown >>>>>>>>>>>")
    {:ok, watcher_pid}
  end
end
