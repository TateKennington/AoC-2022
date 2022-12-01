defmodule Day do
  def part1(input) do
    input
    |> Enum.reduce(
      {0, 0},
      fn line, {curr_max, curr} ->
        if(line !== "\n") do
          {curr_max, curr + elem(Integer.parse(line), 0)}
        else
          {max(curr_max, curr), 0}
        end
      end
    )
    |> elem(0)
  end

  def part2(input) do
    input
    |> Enum.reduce(
      [0],
      fn line, list ->
        if(line !== "\n") do
          [(line |> Integer.parse() |> elem(0)) + hd(list) | tl(list)]
        else
          [0 | list]
        end
      end
    )
    |> Enum.sort_by(&(-&1))
    |> Enum.take(3)
    |> Enum.sum()
  end
end

small_input = File.stream!("in.small")
input = File.stream!("in")

IO.puts(Day.part1(small_input))
IO.puts(Day.part1(input))

IO.puts(Day.part2(small_input))
IO.puts(Day.part2(input))
