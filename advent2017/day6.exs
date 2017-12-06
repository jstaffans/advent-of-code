defmodule AdventOfCode.Day6 do

  def count_cycles(input) do
    input
    |> String.split
    |> Enum.map(&String.to_integer/1)
    |> cycle(MapSet.new, %{}, 1)
  end

  defp cycle(banks, lookup, cycle_tracking, num_cycles) do
    max_index = find_max_index(banks)
    next_banks = spread(banks, max_index)
    lookup_key = Enum.join(next_banks)
    if MapSet.member?(lookup, lookup_key) do
      {num_cycles, num_cycles - Map.get(cycle_tracking, lookup_key)}
    else
      cycle(
        next_banks,
        MapSet.put(lookup, lookup_key),
        Map.put(cycle_tracking, lookup_key, num_cycles),
        num_cycles + 1)
    end
  end

  defp find_max_index(banks) do
    {_, max_k} = banks
    |> Enum.with_index
    |> Enum.reduce({0, Enum.at(banks, 0)}, fn ({v, k}, {max_v, max_k}) ->
      if v > max_v, do: {v, k}, else: {max_v, max_k}
    end)
    max_k
  end

  defp spread(banks, source) do
    count = Enum.at(banks, source)
    updated_banks = List.replace_at(banks, source, 0)
    spread(updated_banks, source + 1, count)
  end

  defp spread(banks, index, remaining) when remaining == 0, do: banks

  defp spread(banks, index, remaining) do
    index_to_update = if (index >= Enum.count(banks)), do: 0, else: index
    next_banks = List.update_at(banks, index_to_update, &(&1 + 1))
    spread(next_banks, index_to_update + 1, remaining - 1)
  end

end

IO.puts "Part one"

IO.inspect AdventOfCode.Day6.count_cycles "0 2 7 0"

"data/day6.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day6.count_cycles
|> IO.inspect
