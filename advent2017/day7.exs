defmodule AdventOfCode.Day7 do

  def parse_tree(input) do
    tree =
      input
      |> String.split("\n")
      |> Enum.map(&(to_node(&1)))
      |> create_tree(%{})

    [root] = tree |> find_root |> Enum.to_list

    check_balance(tree, root)
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

  defp check_balance(tree, root) do
    {_weight, children} = tree[root]
    child_weights = Enum.map(children, &(calculate_weight(tree, &1)))

    all_same =
      child_weights
      |> Enum.map(&(&1 == List.first(child_weights)))
      |> Enum.all?

    if all_same do
      root <> " has wrong weight" # TODO, need to find amount manually currently
    else
      heavy_child =
        child_weights
        |> Enum.with_index
        |> Enum.reduce(-1, fn ({weight, i}, result) ->
        if weight == Enum.max(child_weights), do: i, else: result
      end)

      check_balance(tree, Enum.at(children, heavy_child))
    end
  end

  defp calculate_weight(tree, root_name) do
    {root_weight, children} = tree[root_name]
    root_weight + visit_children(tree, children)
  end

  defp visit_children(tree, [child|rest]) do
    calculate_weight(tree, child) + visit_children(tree, rest)
  end

  defp visit_children(tree, []), do: 0
end

IO.inspect AdventOfCode.Day7.parse_tree "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\n fwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"

"data/day7.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day7.parse_tree
|> IO.inspect
