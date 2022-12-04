defmodule Day do
  def part1(input) do
    input
    |> Stream.map(fn line ->
      line = String.trim(line)
      {first, second} = String.split_at(line, div(String.length(line), 2))
      first = String.to_charlist(first)
      second = String.to_charlist(second)
      MapSet.to_list(MapSet.intersection(MapSet.new(first), MapSet.new(second)))
    end)
    |> Stream.map(fn [c] -> if c >= ?a && c <= ?z, do: c - ?a + 1, else: c - ?A + 27 end)
    |> Enum.sum()
  end

  def part2(input) do
    input
    |> Stream.map(&String.trim/1)
    |> Stream.chunk_every(3)
    |> Stream.map(fn [a, b, c] ->
      first = String.to_charlist(a)
      second = String.to_charlist(b)
      third = String.to_charlist(c)
      overlap = MapSet.intersection(MapSet.new(first), MapSet.new(second))
      MapSet.to_list(MapSet.intersection(MapSet.new(overlap), MapSet.new(third)))
    end)
    |> Stream.map(fn [c] -> if c >= ?a && c <= ?z, do: c - ?a + 1, else: c - ?A + 27 end)
    |> Enum.sum()
  end
end

small_input = File.stream!("in.small")
input = File.stream!("in")

IO.puts(Day.part1(small_input))
IO.puts(Day.part1(input))

IO.puts(Day.part2(small_input))
IO.puts(Day.part2(input))
