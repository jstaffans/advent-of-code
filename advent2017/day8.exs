defmodule AdventOfCode.Day8 do

  def run_instructions(input) do
    {registers, absolute_max_value} =
      input
      |> String.split("\n")
      |> Enum.map(&(to_instruction(&1)))
      |> run(%{}, 0)

    {Enum.max(Map.values(registers)), absolute_max_value}

  end

  defp to_instruction(s) do
    [register, op, value | pred] = String.split(s)

    update_fn =
      case op do
        "inc" -> fn (current_value) ->
            current_value = if current_value, do: current_value, else: 0
            {current_value, current_value + String.to_integer(value)}
          end
        "dec" -> fn (current_value) ->
            current_value = if current_value, do: current_value, else: 0
            {current_value, current_value - String.to_integer(value)}
          end
      end

    predicate =
      case pred do
        [_, lhs, ">", rhs] ->
          fn (regs) -> Map.get(regs, lhs, 0) > String.to_integer(rhs) end
        [_, lhs, "<", rhs] ->
          fn (regs) -> Map.get(regs, lhs, 0) < String.to_integer(rhs) end
        [_, lhs, ">=", rhs] ->
          fn (regs) -> Map.get(regs, lhs, 0) >= String.to_integer(rhs) end
        [_, lhs, "<=", rhs] ->
          fn (regs) -> Map.get(regs, lhs, 0) <= String.to_integer(rhs) end
        [_, lhs, "==", rhs] ->
          fn (regs) -> Map.get(regs, lhs, 0) == String.to_integer(rhs) end
        [_, lhs, "!=", rhs] ->
          fn (regs) -> Map.get(regs, lhs, 0) != String.to_integer(rhs) end
        _ -> "no match"
      end

    {register, update_fn, predicate}

  end

  defp run([instruction|rest], registers, max_value) do
    {register, update_fn, pred} = instruction
    if pred.(registers) do
      {_, new_registers} = Map.get_and_update(registers, register, update_fn)
      current_max_value = Enum.max(Map.values(new_registers))
      new_max_value = if current_max_value > max_value, do: current_max_value, else: max_value
      run(rest, new_registers, new_max_value)
    else
      run(rest, registers, max_value)
    end
  end

  defp run([], registers, max_value), do: {registers, max_value}
end

IO.inspect AdventOfCode.Day8.run_instructions "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"

"data/day8.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day8.run_instructions
|> IO.inspect
