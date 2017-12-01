defmodule AdventOfCode.Day1 do

  @doc """
  Get the sum of matching pairs.
  """
  def sum_1(input) do
    l = to_list(input)
    # fake a cycled list by appending first element
    cycled = l ++ Enum.slice(l, 0, 1)
    {_, sum} =
      Enum.reduce(
        cycled,
        {nil, 0},
        fn(this, acc) ->
          {prev, sum} = acc
          if this == prev, do: {this, sum + prev}, else: {this, sum}
        end
      )
    sum
  end

  @doc """
  Get the sum of elements matching halfway around the list.
  """
  def sum_2(input) do
    l = to_list(input)
    half = div(String.length(input), 2)
    pair_indexes = Enum.to_list(half..String.length(input)-1) ++ Enum.to_list(0..(half - 1))
    l_with_pair_indexes = Enum.zip(pair_indexes, l)

    Enum.reduce(
      l_with_pair_indexes,
      0,
      fn(x, sum) ->
        {other_index, this} = x
        other = Enum.at(l, other_index)
        if this == other, do: sum + this, else: sum
      end
    )
  end

  defp to_list(input) do
      input
      |> String.split("")
      |> Enum.drop(-1)
      |> Enum.map(&String.to_integer/1)
  end
end


IO.puts "Part one"

IO.puts AdventOfCode.Day1.sum_1 "1122"
IO.puts AdventOfCode.Day1.sum_1 "1111"
IO.puts AdventOfCode.Day1.sum_1 "1234"
IO.puts AdventOfCode.Day1.sum_1 "91212129"

"data/day1.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day1.sum_1
|> IO.puts

IO.puts "Part two"

IO.puts AdventOfCode.Day1.sum_2 "1212"
IO.puts AdventOfCode.Day1.sum_2 "1221"
IO.puts AdventOfCode.Day1.sum_2 "123123"
IO.puts AdventOfCode.Day1.sum_2 "12131415"

"data/day1.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day1.sum_2
|> IO.puts
