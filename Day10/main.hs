import Data.Function
import System.IO

processInstruction ["noop"] = (0, 1)
processInstruction ["addx", count] = (read count, 2)

formatOutput [] = ""
formatOutput list = take 40 list ++ "\n" ++ formatOutput (drop 40 list)

part1 contents =
  lines contents
    & foldl
      ( \(reg, cycle, signal, cycles) line ->
          let parts = words line
              (delReg, delCycle) = processInstruction parts
              newCycle = cycle + delCycle
              (newSignal, newCycles) =
                if not (null cycles) && newCycle > head cycles
                  then (reg * head cycles + signal, tail cycles)
                  else (signal, cycles)
           in (reg + delReg, newCycle, newSignal, newCycles)
      )
      (1, 1, 0, [20 + 40 * x | x <- [0 .. 5]])
    & (\(_, _, signal, _) -> signal)
    & show

part2 contents =
  lines contents
    & foldl
      ( \(reg, cycle, output) line ->
          let parts = words line
              (delReg, delCycle) = processInstruction parts
              newCycle = cycle + delCycle
              newOutput =
                output
                  ++ ( [cycle .. newCycle -1]
                         & map (\col -> if abs ((col -1) `mod` 40 - reg) <= 1 then '#' else '.')
                     )
           in (reg + delReg, newCycle, newOutput)
      )
      (1, 1, [])
    & (\(_, _, output) -> output)
    & formatOutput

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents