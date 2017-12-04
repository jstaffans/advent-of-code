defmodule AdventOfCode.Day4 do

  def count_valid(input) do
    phrases =
      input
      |> String.split("\n")
      |> Enum.map(&String.split/1)

    valid = Enum.map(phrases, fn (words) ->
      word_set = Enum.reduce(words, MapSet.new, fn (word, acc) -> MapSet.put(acc, sort_string(word)) end)
      Enum.count(words) == Enum.count(word_set)
    end)

    valid |> Enum.filter(&(&1)) |> Enum.count
  end

  defp sort_string(s) do
    s |> String.split("") |> Enum.sort |> Enum.join("")
  end

end

IO.puts "Part one"

IO.puts AdventOfCode.Day4.count_valid "aa bb cc dd ee\naa bb cc dd aa\naa bb cc dd aaa"

"data/day4.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day4.count_valid
|> IO.puts


Enum.sort(["b", "a"])
