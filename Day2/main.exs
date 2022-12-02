defmodule Day do
  def normalise(hand) do
    case hand do
      "X" -> 0
      "Y" -> 1
      "Z" -> 2
      "A" -> 0
      "B" -> 1
      "C" -> 2
    end
  end

  def beat(hand) do
    Integer.mod(hand + 1, 3)
  end

  def lose(hand) do
    case hand do
      0 -> 2
      x -> x - 1
    end
  end

  def part1(input) do
    input
    |> Stream.map(fn line ->
      a = normalise(String.at(line, 0))
      b = normalise(String.at(line, 2))

      b + 1 +
        cond do
          beat(a) === b -> 6
          a === b -> 3
          true -> 0
        end
    end)
    |> Enum.sum()
  end

  def part2(input) do
    input
    |> Stream.map(fn line ->
      a = normalise(String.at(line, 0))
      c = normalise(String.at(line, 2))

      b =
        case c do
          0 -> lose(a)
          1 -> a
          2 -> beat(a)
        end

      b + 1 +
        cond do
          beat(a) === b -> 6
          a === b -> 3
          true -> 0
        end
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
