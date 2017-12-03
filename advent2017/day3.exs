defmodule AdventOfCode.Day3 do

  import Util
  alias ComplexNum.Cartesian, as: Complex

  def manhattan_distance(input) do
    origin = Complex.new(0, 0)
    spiral(origin, input - 1, 0)
  end

  defp spiral(position, steps, loop_index) do
    current_circle = circle(loop_index)

    {position, steps} = spiral_steps(position, steps, current_circle)

    if steps == 0, do: calculate_distance(position), else: spiral(position, steps, loop_index + 1)
  end

  # Given a position, a countdown of steps to take, and a flat list of moves,
  # spiral_steps will step outwards in the spiral by adding the next step to
  # the current position.

  defp spiral_steps(position, steps, _) when steps == 0 do
    {position, steps}
  end

  defp spiral_steps(position, steps, []), do: {position, steps}

  defp spiral_steps(position, steps, [next_step|rest]) do
    spiral_steps(Complex.add(position, next_step), steps - 1, rest)
  end

  # Get the "circle" of vector arrays for the current loop index (layer)
  defp circle(loop_index) do
    full_circle =
      [
        [Complex.new(1, 0), Complex.new(0, 1)],
        [Complex.new(-1, 0), Complex.new(-1, 0)],
        [Complex.new(0, -1), Complex.new(0, -1)],
        [Complex.new(1, 0), Complex.new(1, 0)]
      ]

    full_circle
    |> Enum.map(fn (vectors) ->
      vectors ++ replicate(loop_index * 2, List.last(vectors))
    end)
    |> List.flatten
  end

  # Calculate Manhattan distance given a position in the complex plane.
  defp calculate_distance(position) do
    abs(Complex.real(position)) + abs(Complex.imaginary(position))
  end

  # Part Two: first sum of all neighbors that is greater than input.
  def first_sum_gt(input) do
    origin = Complex.new(0, 0)
    lookup = Map.put(%{}, lookup_key(origin), 1)
    first_sum(origin, fn (x) -> x > input end, lookup, 0)
  end

  defp lookup_key(position) do
    {Complex.real(position), Complex.imaginary(position)}
  end

  # return the first sum for which pred is true
  defp first_sum(position, pred, lookup, loop_index) do
    current_circle = circle(loop_index)
    {position, lookup, last_sum} = spiral_sum(position, pred, lookup, current_circle)
    if pred.(last_sum), do: last_sum, else: first_sum(position, pred, lookup, loop_index + 1)
  end

  # given a lookup table, calculate the sum for a given position
  defp sum_at(position, lookup) do
    neighbor_vectors = [
      Complex.new(-1, 1), Complex.new(0, 1), Complex.new(1, 1),
      Complex.new(-1, 0), Complex.new(1, 0),
      Complex.new(-1, -1), Complex.new(0, -1), Complex.new(1, -1)
    ]

    Enum.reduce(
      neighbor_vectors,
      0,
      fn (neighbor_vector, acc) ->
        neighbor = Complex.add(position, neighbor_vector)
        neighbor_value = Map.get(lookup, lookup_key(neighbor), 0)
        acc + neighbor_value
      end)
  end

  defp spiral_sum(position, pred, lookup, []) do
    {position, lookup, Map.get(lookup, lookup_key(position))}
  end

  defp spiral_sum(position, pred, lookup, [next_step|rest]) do
    next_position = Complex.add(position, next_step)
    next_sum = sum_at(next_position, lookup)
    lookup = Map.put(lookup, lookup_key(next_position), next_sum)
    if pred.(next_sum), do: {nil, nil, next_sum}, else: spiral_sum(next_position, pred, lookup, rest)
  end

end

IO.puts "Part one"

IO.puts AdventOfCode.Day3.manhattan_distance 12
IO.puts AdventOfCode.Day3.manhattan_distance 23
IO.puts AdventOfCode.Day3.manhattan_distance 1024
IO.puts AdventOfCode.Day3.manhattan_distance 289326

IO.puts "Part two"

IO.puts AdventOfCode.Day3.first_sum_gt 100
IO.puts AdventOfCode.Day3.first_sum_gt 289326
