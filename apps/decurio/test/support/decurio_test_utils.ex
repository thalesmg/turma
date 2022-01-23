defmodule Decurio.Test.Utils do
  alias Legionarius.Result

  def simple_fn1() do
    %Result{output: Node.self()}
  end

  def simple_fn2() do
    :unwrapped_result
  end

  def simple_fn3() do
    raise "some error"
  end
end
