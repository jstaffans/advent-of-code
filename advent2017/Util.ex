defmodule Util do

  def replicate(n, _) when n == 0, do: []

  def replicate(n, x), do: for _ <- 1..n, do: x

  def nth(list, idx) when is_number(idx) and idx < 0 do
    throw 'Index must be positive'
  end

  def nth(list, idx) when idx > length(list) - 1 do
    throw ' index bigger the list size'
  end

  def nth([ element | _ ] , 0), do: element

  def nth([ head | tail ], idx), do: nth(tail, idx - 1)

end
