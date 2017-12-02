defmodule AdventOfCode.Day2 do

  @doc """
  Get the diff_checksum (sum of the difference between largest and smallest element over all rows).
  """
  def diff_checksum(input) do
    m = to_matrix(input)
    Enum.reduce(
      m,
      0,
      fn (row, sum) ->
        row_diff =
          row
          |> Enum.sort
          |> (fn (sorted) -> List.last(sorted) - List.first(sorted) end).()
        sum + row_diff
      end
    )
  end

  @doc """
  Get the checksum formed by evenly disible pairs of numbers on each row.
  """
  def zero_remainder_checksum(input) do
    m = to_matrix(input)
    Enum.reduce(
      m,
      0,
      fn (row, sum) ->
          row
          |> Enum.sort
          |> row_fraction
          |> (fn (f) -> sum + f end).()
      end
    )
  end

  defp row_fraction([denominator|rest]) do
    n = nominator(denominator, rest)
    if n > 0, do: div(n, denominator), else: row_fraction(rest)
  end

  defp row_fraction([]), do: 0

  defp nominator(denominator, [head|rest]) do
    if rem(head, denominator) == 0, do: head, else: nominator(denominator, rest)
  end

  defp nominator(_, []), do: 0

  defp to_matrix(input) do
      input
      |> String.split("\n")
      |> Enum.map(fn (x) -> x |> String.split |> Enum.map(&String.to_integer/1) end)
  end
end

IO.puts "Part one"

IO.puts AdventOfCode.Day2.diff_checksum "5 1 9 5\n7 5 3\n2 4 6 8"

"data/day2.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day2.diff_checksum
|> IO.puts

IO.puts AdventOfCode.Day2.zero_remainder_checksum "5 9 2 8\n9 4 7 3\n3 8 6 5"

"data/day2.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day2.zero_remainder_checksum
|> IO.puts
