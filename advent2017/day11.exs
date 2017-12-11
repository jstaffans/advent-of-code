# Hex grid: https://www.redblobgames.com/grids/hexagons/

defmodule Cube do

  defstruct [:q, :r, :s]

end

defmodule Hex do

  def add(a, b), do: %Cube{q: a.q + b.q, r: a.r + b.r, s: a.s + b.s}

  def distance_origin(a), do: trunc((abs(a.q) + abs(a.r) + abs(a.s)) / 2)

  @directions %{
    n:  %Cube{q: 0, r: 1, s: -1},
    ne: %Cube{q: 1, r: 0, s: -1},
    se: %Cube{q: 1, r: -1, s: 0},
    s:  %Cube{q: 0, r: -1, s: 1},
    sw: %Cube{q: -1, r: 0, s: 1},
    nw: %Cube{q: -1, r: 1, s: 0}
  }

  def neighbor(direction, cube), do: add(cube, @directions[direction])

end

defmodule AdventOfCode.Day11 do

  def final_distance(input) do
    input
    |> String.split(",")
    |> Enum.map(&String.to_atom/1)
    |> Enum.reduce(%Cube{q: 0, r: 0, s: 0}, &Hex.neighbor/2)
    |> Hex.distance_origin
  end

  def furthest_distance(input) do
    moves =
      input
      |> String.split(",")
      |> Enum.map(&String.to_atom/1)

    move(%Cube{q: 0, r: 0, s: 0}, 0, moves)
  end

  defp move(cube, max_distance, [m|rest]) do
    next = Hex.neighbor(m, cube)
    current_distance = Hex.distance_origin(next)
    max_distance = if current_distance > max_distance, do: current_distance, else: max_distance
    move(next, max_distance, rest)
  end

  defp move(_, max_distance, []), do: max_distance

end

IO.inspect AdventOfCode.Day11.final_distance "ne,ne,ne"
IO.inspect AdventOfCode.Day11.final_distance "ne,ne,sw,sw"
IO.inspect AdventOfCode.Day11.final_distance "se,sw,se,sw,sw"

"data/day11.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day11.final_distance
|> IO.inspect

"data/day11.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day11.furthest_distance
