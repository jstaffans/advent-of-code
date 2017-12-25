defmodule AdventOfCode.Day24 do

  def find_strongest_bridge(input) do
    pieces =
      input
      |> String.split("\n")
      |> Enum.map(&to_piece/1)
      |> MapSet.new

    {pieces, rest} = pick_pieces(pieces, 0, [])

    pieces
    |> build(rest, 0, [])
    |> Enum.map(&strength/1)
    |> Enum.sort_by(&(elem(&1, 0)))   # sort by elem(1) to find strongest bridge of all
    |> Enum.reverse
    |> Enum.take(1)
  end

  # Convert "2/2" to internal piece representation [2, 2]
  defp to_piece(description) do
    description
    |> String.split("/")
    |> Enum.map(&String.to_integer/1)
  end

  # Pick a list of possible next pieces given the desired number of pins.
  # Returns a tuple, with the first element being all the pieces that
  # match the desired pins and the second element the remaining pieces.
  #
  # Returns :error if no pieces were found.
  defp pick_pieces(pieces, pins, acc) do
    piece =
      pieces
      |> Enum.filter(fn [a, b] -> a == pins or b == pins end)
      |> List.first

    case piece do
      nil -> if Enum.count(acc) == 0, do: :error, else: {acc, pieces}
      _ -> pick_pieces(MapSet.delete(pieces, piece), pins, [piece|acc])
    end
  end

  # depth-first accumulation of bridges.
  defp build([piece|rest], remaining, pins, acc) do
    free_to_attach = MapSet.union(MapSet.new(rest), remaining)
    attach(free_to_attach, next_pins(piece, pins), acc ++ [piece])
    ++ build(rest, MapSet.put(remaining, piece), pins, acc)
  end

  # terminating case for build.
  defp build([], remaining, pins, acc), do: []

  # picks a piece to attach to the accumulated bridge.
  # if no next piece can be found, the accumulated bridge is added
  # to the list of possible bridges.
  defp attach(remaining, pins, acc) do
    pieces = pick_pieces(remaining, pins, [])
    case pieces do
      :error -> [acc]
      {pieces, remaining} -> build(pieces, remaining, pins, acc)
    end
  end

  # given a piece and the amount of pins on the already-attached side,
  # returns the other amount of pins.
  defp next_pins(piece, pins) do
    [pins_1, pins_2] = piece
    if pins_1 == pins, do: pins_2, else: pins_1
  end

  defp strength(bridge) do
    bridge
    |> Enum.reduce({0, 0}, fn ([a, b], {length, sum}) -> {length + 1, sum + a + b} end)
  end
end

"data/day24.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day24.find_strongest_bridge

