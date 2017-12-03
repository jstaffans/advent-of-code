defmodule Util do

  def replicate(n, _) when n == 0, do: []

  def replicate(n, x), do: for _ <- 1..n, do: x

end
