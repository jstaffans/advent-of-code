defmodule Particle do

  defstruct [:position, :velocity, :acceleration]

  def create(s) do
    s
    |> String.split(", ")
    |> Enum.map(&parse_component/1)
    |> Enum.reduce(fn (component, particle) ->
      Map.merge(component, particle, fn (_, a, b) -> a || b end)
    end)
  end

  defp parse_component("p=" <> coords) do
    %Particle{:position => parse_coords(coords)}
  end

  defp parse_component("v=" <> coords) do
    %Particle{:velocity => parse_coords(coords)}
  end

  defp parse_component("a=" <> coords) do
    %Particle{:acceleration => parse_coords(coords)}
  end

  defp parse_coords(coords) do
    coords
    |> String.slice(1..String.length(coords)-2)
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple
  end

end

defmodule AdventOfCode.Day20 do

  def simulate(input, ticks) do
    input
    |> String.split("\n")
    |> Enum.map(&Particle.create/1)
    |> tick(ticks)
    |> Enum.map(fn (%{position: {x, y, z}}) -> abs(x) + abs(y) + abs(z) end)
    |> Enum.with_index
    |> Enum.reduce(fn ({d1, i1} = p1, {d2, i2} = p2) -> if d2 < d1, do: p2, else: p1 end)
  end

  defp tick(particles, ticks_left) when ticks_left > 0 do
    particles =
      particles
      |> Enum.map(&accelerate/1)
      |> Enum.map(&move/1)

    tick(particles, ticks_left - 1)
  end

  defp tick(particles, _), do: particles

  defp accelerate(particle = %Particle{}) do
    %{acceleration: {x, y, z}} = particle
    Map.update(particle, :velocity, 0, fn ({vx, vy, vz}) -> {vx + x, vy + y, vz + z} end)
  end

  defp move(particle = %Particle{}) do
    %{velocity: {x, y, z}} = particle
    Map.update(particle, :position, 0, fn ({px, py, pz}) -> {px + x, py + y, pz + z} end)
  end

end

"data/day20.txt"
|> File.read!
|> String.trim
|> AdventOfCode.Day20.simulate(1000)

