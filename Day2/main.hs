import Data.Char
import Data.Function
import System.IO

normalise c =
  let i = ord c
   in if i >= ord 'X' then i - ord 'X' else i - ord 'A'

playRound (a, b)
  | beat a == b = 6 + b + 1
  | a == b = 3 + b + 1
  | otherwise = b + 1

beat a = (a + 1) `mod` 3

lose 0 = 2
lose a = a - 1

playRound' (a, c) =
  let b = case c of
        0 -> lose a
        1 -> a
        2 -> beat a
   in playRound (a, b)

part1 contents =
  lines contents
    & map (\line -> playRound (normalise $ head line, normalise $ line !! 2))
    & sum
    & show

part2 contents =
  lines contents
    & map (\line -> playRound' (normalise $ head line, normalise $ line !! 2))
    & sum
    & show

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents

  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
