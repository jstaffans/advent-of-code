defmodule AdventOfCode.Day3 do

  import Util
  alias ComplexNum.Cartesian, as: Complex

  def manhattan_distance(input) do
    origin = Complex.new(0, 0)
    spiral(origin, input - 1, 0)
  end

  defp spiral(position, steps, loop_index) do
    full_circle =
      [
        [Complex.new(1, 0), Complex.new(0, 1)],
        [Complex.new(-1, 0), Complex.new(-1, 0)],
        [Complex.new(0, -1), Complex.new(0, -1)],
        [Complex.new(1, 0), Complex.new(1, 0)]
      ]

    current_circle =
      full_circle
      |> extend_circle(loop_index)
      |> List.flatten

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

  # Extend the individual vector arrays representing the "sides" of the spiral for the given loop index/layer.
  defp extend_circle(circle_vectors, loop_index) do
    Enum.map(circle_vectors, fn (vectors) ->
      vectors ++ replicate(loop_index * 2, List.last(vectors))
    end)
  end

  # Calculate Manhattan distance given a position in the complex plane.
  defp calculate_distance(position) do
    IO.inspect(position)
    abs(Complex.real(position)) + abs(Complex.imaginary(position))
  end

end

IO.puts "Part one"

IO.puts AdventOfCode.Day3.manhattan_distance 12
IO.puts AdventOfCode.Day3.manhattan_distance 23
IO.puts AdventOfCode.Day3.manhattan_distance 1024
IO.puts AdventOfCode.Day3.manhattan_distance 289326

IO.puts "Part two"
