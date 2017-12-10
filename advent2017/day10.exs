defmodule AdventOfCode.Day10 do

  import Util

  def hash(list, input) do
    lengths =
      input
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)

    hash_one(list, 0, 0, lengths)
  end

  defp hash_one(list, current_position, skip_size, [length|rest]) do
    next = swap(list, current_position, length)
    hash_one(next, current_position + length + skip_size, skip_size + 1, rest)
  end

  defp hash_one(list, current_position, skip_size, []), do: list

  defp swap(list, left, length) when length > 1 do
    list_length = Enum.count(list)
    i = rem(left, list_length)
    right = i + length - 1
    j = rem(right, list_length)

    next =
      list
      |> List.replace_at(i, nth(list, j))
      |> List.replace_at(j, nth(list, i))

    swap(next, left + 1, length - 2)
  end

  defp swap(list, _, length), do: list

end

IO.inspect AdventOfCode.Day10.hash Enum.to_list(0..4), "3,4,1,5"
IO.inspect AdventOfCode.Day10.hash Enum.to_list(0..255), "165,1,255,31,87,52,24,113,0,91,148,254,158,2,73,153"
