defmodule AdventOfCode.Day7 do

  def parse_tree(input) do
    nodes =
      input
      |> String.split("\n")
      |> Enum.map(&(to_node(&1)))

    [root] = nodes |> create_tree(%{}) |> find_root |> Enum.to_list

    root
    # TODO: calculate weights for root's immediate children
  end

  defp create_tree([], tree), do: tree

  defp create_tree([node|rest], tree) do
    [name, weight, children] = node
    updated_tree = Map.put(tree, name, {weight, children})
    create_tree(rest, updated_tree)
  end

  def to_node(s) do
    [name, weight, children] = s |> String.split |> parse_node
  end

  defp parse_node([name, weight]) do
    [name, trim_weight(weight), []]
  end

  defp parse_node([name, weight, _ | children]) do
    [name, trim_weight(weight), Enum.map(children, &(String.replace_suffix(&1, ",", "")))]
  end

  defp trim_weight(weight), do: weight |> String.trim("(") |> String.trim(")") |> String.to_integer

  defp find_root(tree) do
    node_set = MapSet.new(Map.keys(tree))
    children_set = Enum.reduce(Map.values(tree), MapSet.new, fn ({_weight, children}, acc) ->
      MapSet.union(acc, MapSet.new(children))
    end)
    MapSet.difference(node_set, children_set)
  end

end

IO.puts "Part one"

IO.inspect AdventOfCode.Day7.parse_tree "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\n fwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"

# "data/day7.txt"
# |> File.read!
# |> String.trim
# |> AdventOfCode.Day7.parse_tree
# |> IO.inspect
