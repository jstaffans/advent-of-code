defmodule Generator do

  def next_value(prev_value, multiplier, denominator) do
    next = rem(prev_value * multiplier, 2147483647)
    if rem(next, denominator) === 0, do: next, else: next_value(next, multiplier, denominator)
  end

end

defmodule AdventOfCode.Day15 do

  use Bitwise

  def matches(a_start, b_start) do
    check_value(a_start, b_start, trunc(5.0e6), 0)
  end

  defp check_value(a_prev, b_prev, remaining, n_match) when remaining > 0 do
    a_task = Task.async(Generator, :next_value, [a_prev, 16807, 4])
    b_task = Task.async(Generator, :next_value, [b_prev, 48271, 8])

    {a_next, b_next} = {Task.await(a_task), Task.await(b_task)}

    low_a = a_next &&& 0xFFFF
    low_b = b_next &&& 0xFFFF
    n_match = if low_b === low_a, do: n_match + 1, else: n_match

    check_value(a_next, b_next, remaining - 1, n_match)
  end

  defp check_value(a_prev, b_prev, remaining, n_match), do: n_match

end

# AdventOfCode.Day15.matches 65, 8921
IO.puts AdventOfCode.Day15.matches 722, 354
