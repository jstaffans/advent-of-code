defmodule AdventOfCode.Day5 do

  def count_steps(input) do
    moves =
      input
      |> String.split
      |> Enum.map(&String.to_integer/1)

    step(0, 0, Enum.count(moves), moves)
  end

  defp step(position, steps, num_moves, moves) when position >= num_moves or position < 0, do: steps

  defp step(position, steps, num_moves, moves) do
    current_move = Enum.at(moves, position)
    next_moves = List.update_at(moves, position, fn (move) ->
      if move >= 3, do: move - 1, else: move + 1
    end)
    next_position = position + current_move
    step(next_position, steps + 1, num_moves, next_moves)
  end

end

IO.puts "Part one"

IO.puts AdventOfCode.Day5.count_steps "0 3 0 1 -3"

"data/day5.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day5.count_steps
|> IO.puts
