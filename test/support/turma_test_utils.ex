defmodule Turma.Test.Utils do
  def success() do
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
