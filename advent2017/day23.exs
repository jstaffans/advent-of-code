defmodule AdventOfCode.Day23 do

  def execute(input) do
    registers = %{
      "a" => 0,
      "b" => 0,
      "c" => 0,
      "d" => 0,
      "e" => 0,
      "f" => 0,
      "g" => 0,
      "h" => 0
    }

    instructions =
      input
      |> String.split("\n")
      |> Enum.with_index
      |> Enum.into(%{}, fn {a, b} -> {b, a} end)

    op(registers, instructions, 0, 0)
  end

  defp op(registers, instructions, index, mul_count) when index < map_size(instructions) do

    instruction =
      instructions[index]
      |> String.split
      |> Enum.map(fn v ->
      case Integer.parse(v) do
        {intVal, _} -> intVal
        :error -> v
      end
    end)

    case instruction do

      ["set", register, operand] ->
        value = get_value(registers, operand)
        registers = Map.put(registers, register, value)
        op(registers, instructions, index + 1, mul_count)

      ["sub", register, operand] ->
        value = get_value(registers, operand)
        registers = Map.update!(registers, register, &(&1 - value))
        op(registers, instructions, index + 1, mul_count)

      ["mul", register, operand] ->
        value = get_value(registers, operand)
        registers = Map.update!(registers, register, &(&1 * value))
        op(registers, instructions, index + 1, mul_count + 1)

      ["jnz", 1, offset] ->
        op(registers, instructions, index + offset, mul_count)

      ["jnz", register, offset] ->
        value = Map.get(registers, register)
        case value do
          0 -> op(registers, instructions, index + 1, mul_count)
          _ -> op(registers, instructions, index + offset, mul_count)
        end
    end
  end

  defp op(_, _, _, mul_count), do: mul_count

  defp get_value(registers, operand) when is_integer(operand), do: operand

  defp get_value(registers, operand), do: Map.get(registers, operand)


end

"data/day23.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day23.execute

