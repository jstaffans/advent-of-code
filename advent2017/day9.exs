defmodule AdventOfCode.Day9 do

  def count_score(input) do
    parse(0, 0, 0, input)
  end

  defp parse(acc, current_score, garbage_count, ""), do: {acc, garbage_count}

  defp parse(acc, current_score, garbage_count, "{" <> rest) do
    parse(acc, current_score + 1, garbage_count, rest)
  end

  defp parse(acc, current_score, garbage_count, "}" <> rest) do
    parse(acc + current_score, current_score - 1, garbage_count, rest)
  end

  defp parse(acc, current_score, garbage_count, "," <> rest) do
    parse(acc, current_score, garbage_count, rest)
  end

  defp parse(acc, current_score, garbage_count, "<" <> rest) do
    parse_garbage(acc, current_score, garbage_count, rest)
  end

  defp parse_garbage(acc, current_score, garbage_count, "!" <> rest) do
    {_, rest} = String.split_at(rest, 1)
    parse_garbage(acc, current_score, garbage_count, rest)
  end

  defp parse_garbage(acc, current_score, garbage_count, ">" <> rest) do
    parse(acc, current_score, garbage_count, rest)
  end

  defp parse_garbage(acc, current_score, garbage_count, s) do
    {_, rest} = String.split_at(s, 1)
    parse_garbage(acc, current_score, garbage_count + 1, rest)
  end

end

IO.inspect AdventOfCode.Day9.count_score "{}"
IO.inspect AdventOfCode.Day9.count_score "{{<{}>}}"
IO.inspect AdventOfCode.Day9.count_score "{{{},{},{{}}}}"
IO.inspect AdventOfCode.Day9.count_score "{<a>,<a>,<a>,<a>}"
IO.inspect AdventOfCode.Day9.count_score "{{<ab>},{<ab>},{<ab>},{<ab>}}"
IO.inspect AdventOfCode.Day9.count_score "{{<a!>},{<a!>},{<a!>},{<ab>}}"

"data/day9.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day9.count_score
|> IO.inspect
