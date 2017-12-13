defmodule AdventOfCode.Day13 do

  def severity(input) do
    layers =
      input
      |> to_layers

    scanners =
      layers
      |> init_scanners

    pass_layer(layers, scanners, 0, 0)
  end

  defp init_scanners(layers) do
    layers
    |> Map.keys
    |> Enum.reduce(%{}, fn (range, acc) ->
      Map.put(acc, range, 0)
    end)
  end

  defp pass_layer(layers, scanners, layer, damage) when map_size(layers) > 0 do
    damage_from_layer = if scanner_present?(layers, scanners, layer), do: layer * layers[layer], else: 0
    layers = remove_layer(layers, layer)
    scanners = move_scanners(layers, scanners)
    pass_layer(layers, scanners, layer + 1, damage + damage_from_layer)
  end

  defp pass_layer(layers, _, _, damage) when map_size(layers) == 0, do: damage

  defp scanner_present?(layers, scanners, layer) do
    scanner_position = Map.get(scanners, layer)
    is_integer(scanner_position) and rem(scanner_position, 2*(layers[layer]-1)) == 0
  end

  defp remove_layer(layers, layer) do
    Map.delete(layers, layer)
  end

  defp move_scanners(layers, scanners) do
    Enum.reduce(scanners, %{}, fn ({range, scanner_position}, acc) ->
      Map.put(acc, range, scanner_position + 1)
    end)
  end

  defp to_layers(input) do
    input
    |> String.split("\n")
    |> Enum.map(&(String.split(&1, ": ")))
    |> Enum.reduce(%{}, fn ([range, depth], acc) ->
      Map.put(acc, String.to_integer(range), String.to_integer(depth))
    end)
  end

end

AdventOfCode.Day13.severity "0: 3\n1: 2\n4: 4\n6: 4"
