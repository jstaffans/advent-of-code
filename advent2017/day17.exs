defmodule Buffer do

  def spin_and_insert(buffer, position, times, insertion) do
    new_position = rem(position + times, Enum.count(buffer)) + 1
    {List.insert_at(buffer, new_position, insertion), new_position}
  end

end

defmodule AdventOfCode.Day17 do

  def spin(times, num_insertions) do
    insert([0], 0, times, 1, num_insertions)
  end

  defp insert(buffer, position, times, insertion, num_insertions) when num_insertions > 0 do
    {buffer, position} =
      Buffer.spin_and_insert(buffer, position, times, insertion)
    insert(buffer, position, times, insertion + 1, num_insertions - 1)
  end

  defp insert(buffer, position, _, _, _) do
    Enum.at(buffer, position + 1)
  end

end

AdventOfCode.Day17.spin 312, 2017

