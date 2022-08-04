defmodule Turma.Test.Utils do
  def success() do
    node()
  end

  def delayed_success() do
    Process.sleep(1_000)
    node()
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
