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

defmodule Matrix do
  @moduledoc """
  Helpers for working with multidimensional lists, also called matrices.
  See http://blog.danielberkompas.com/2016/04/23/multidimensional-arrays-in-elixir.html
  """

  @doc """
  Converts a multidimensional list into a zero-indexed map.

  ## Example

      iex> list = [["x", "o", "x"]]
      ...> Matrix.from_list(list)
      %{0 => %{0 => "x", 1 => "o", 2 => "x"}}
  """
  def from_list(list) when is_list(list) do
    do_from_list(list)
  end

  defp do_from_list(list, map \\ %{}, index \\ 0)
  defp do_from_list([], map, _index), do: map
  defp do_from_list([h|t], map, index) do
    map = Map.put(map, index, do_from_list(h))
    do_from_list(t, map, index + 1)
  end
  defp do_from_list(other, _, _), do: other

  @doc """
  Converts a zero-indexed map into a multidimensional list.

  ## Example

      iex> matrix = %{0 => %{0 => "x", 1 => "o", 2 => "x"}}
      ...> Matrix.to_list(matrix)
      [["x", "o", "x"]]
  """
  def to_list(matrix) when is_map(matrix) do
    do_to_list(matrix)
  end

  defp do_to_list(matrix) when is_map(matrix) do
    for {_index, value} <- matrix,
        into: [],
        do: do_to_list(value)
  end
  defp do_to_list(other), do: other
end
