defmodule AdventOfCode.Day16 do

  def permutate(input, row, n) when n > 0 do
    next =
      input
      |> String.split(",")
      |> move(row)

    permutate(input, next, n-1)
  end

  def permutate(input, row, n) when n === 0, do: row

  defp move([move|rest], s) do
    s = apply_move(move, s)
    move(rest, s)
  end

  defp move([], s), do: s

  defp apply_move("s" <> n, s) do
    l = String.length(s)
    n = String.to_integer(n)
    String.slice(s, l-n, l) <> String.slice(s, 0, l-n)
  end

  defp apply_move("x" <> positions, s) do
    [p1, p2] =
      positions
      |> String.split("/")
      |> Enum.map(&String.to_integer/1)

    swap(s, p1, p2)
  end

  defp apply_move("p" <> names, s) do
    [p1, p2] =
      names
      |> String.split("/")
      |> Enum.map(&(find_name(s, 0, &1)))

    swap(s, p1, p2)
  end

  defp swap(s, p1, p2) do
    [p1, p2] = if p1 > p2, do: [p2, p1], else: [p1, p2]
    String.slice(s, 0, p1)
    <> String.at(s, p2)
    <> String.slice(s, p1+1, p2-p1-1)
    <> String.at(s, p1)
    <> String.slice(s, p2+1, String.length(s)-p2)
  end

  defp find_name(<<head :: binary-size(1)>> <> rest, i, name) when head === name, do: i

  defp find_name(<<head :: binary-size(1)>> <> rest, i, name), do: find_name(rest, i+1, name)

end

AdventOfCode.Day16.permutate "s1,x3/4,pe/b", "abcde", 1

"data/day16.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day16.permutate("abcdefghijklmnop", 1)
