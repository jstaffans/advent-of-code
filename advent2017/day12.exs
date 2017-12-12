defmodule Node do
  def from_string(s) do
    [id|children] =
      s
      |> String.split(~r{( <-> |, )})
      |> Enum.map(&String.to_integer/1)

    %{id => children}
  end
end

defmodule AdventOfCode.Day12 do

  def graph_size(input, root) do
    input
    |> to_graph
    |> traverse(root, MapSet.new())
    |> Enum.count
  end

  def num_groups(input) do
    input
    |> to_graph
    |> count_groups
  end

  defp to_graph(input) do
    input
    |> String.split("\n")
    |> Enum.map(&Node.from_string/1)
    |> Enum.reduce(%{}, &Map.merge/2)
  end

  defp traverse(graph, root, visited) do
    if MapSet.member?(visited, root) do
      visited
    else
      traverse_children(graph, Map.get(graph, root), MapSet.put(visited, root))
    end
  end

  defp traverse_children(graph, [child|rest], visited) do
    MapSet.union(traverse(graph, child, visited), traverse_children(graph, rest, visited))
  end

  defp traverse_children(_, [], _), do: MapSet.new

  defp count_groups(graph) when map_size(graph) > 0 do
    root = graph |> Map.keys |> List.first
    visited = traverse(graph, root, MapSet.new())
    graph = Enum.reduce(visited, graph, fn (node, acc) ->
      Map.delete(acc, node)
    end)

    1 + count_groups(graph)
  end

  defp count_groups(graph) when map_size(graph) == 0, do: 0

end

AdventOfCode.Day12.graph_size "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5", 0

"data/day12.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day12.graph_size(0)

"data/day12.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day12.num_groups
