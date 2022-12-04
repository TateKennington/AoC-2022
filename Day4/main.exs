defmodule Day do
  def part1(input) do
    input
    |> Stream.map(fn line ->
      [a, b, x, y] =
        String.split(line, [",", "-"])
        |> Enum.map(fn num -> Integer.parse(num) |> elem(0) end)

      if (a >= x and b <= y) or (x >= a and y <= b), do: 1, else: 0
    end)
    |> Enum.sum()
  end

  def part2(input) do
    input
    |> Stream.map(fn line ->
      [a, b, x, y] =
        String.split(line, [",", "-"])
        |> Enum.map(fn num -> Integer.parse(num) |> elem(0) end)

      if x <= b and a <= y, do: 1, else: 0
    end)
    |> Enum.sum()
  end
end

small_input = File.stream!("in.small")
input = File.stream!("in")

IO.puts(Day.part1(small_input))
IO.puts(Day.part1(input))

IO.puts(Day.part2(small_input))
IO.puts(Day.part2(input))
