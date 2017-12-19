defmodule AdventOfCode.Day19 do

  import Matrix
  alias ComplexNum.Cartesian, as: Complex

  def walk(input) do
    start = Complex.new(find_entrance(input, 0), 0)
    delta = Complex.new(0, 1)

    map = input
    |> String.split("\n")
    |> Enum.map(&parse_line/1)
    |> Matrix.from_list

    step(map, start, delta, "", 1)
  end

  defp parse_line(line) do
    String.split(line, "")
  end

  defp find_entrance("|" <> rest, column), do: column

  defp find_entrance(" " <> rest, column), do: find_entrance(rest, column + 1)

  defp step(map, position, delta, acc, steps) do
    c = char_at(map, position)

    cond do
      c == "|" or c == "-" ->
        step(map, Complex.add(position, delta), delta, acc, steps + 1)
      c == "+" ->
        rotate = Complex.new(Complex.imaginary(delta), Complex.real(delta))
        neighbor_1 = Complex.add(position, rotate)
        neighbor_2 = Complex.sub(position, rotate)
        delta = if char_at(map, neighbor_1) != " ", do: rotate, else: Complex.minus(rotate)
        step(map, Complex.add(position, delta), delta, acc, steps + 1)
      c == "T" -> {acc <> c, steps}
      true ->
        step(map, Complex.add(position, delta), delta, acc <> c, steps + 1)
    end
  end

  defp char_at(map, position) do
    map[Complex.imaginary(position)][Complex.real(position)] 
  end

end

"data/day19.txt"
|> File.read!
|> AdventOfCode.Day19.walk

