defmodule Turma.Test.Utils do
  def run_and_wait(args, timeout \\ 1_000) do
    {:ok, req_id} = apply(Turma.Decurio, :run, args)

    receive do
      {:job_finished, ^req_id} -> :ok
    after
      timeout -> raise "not finished!"
    end

    {:ok, req_id}
  end

  def success() do
    node()
  end

  def delayed_success() do
    Process.sleep(1_000)
    node()
  end

  def hang() do
    Process.register(self(), :hang_test)
    unreachable = :erlang.make_ref()

    receive do
      ^unreachable -> :impossible!
    end
  end

  def test_reply() do
    {:ok, id} = Turma.Decurio.run(&__MODULE__.success/0)

    receive do
      {:job_finished, ^id} -> :ok
    after
      1_000 -> :no_reply
    end
  end

  def filter1(inv) do
    inv
    |> Map.fetch!("legionarius")
    |> Enum.take_every(2)
  end

  def error_fn() do
    :erlang.error(:boom)
  end

  def raise_fn() do
    raise "boom"
  end

  def throw_fn() do
    throw(:boom)
  end
end
