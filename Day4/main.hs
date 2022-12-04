import Data.Bifunctor
import Data.Function
import System.IO

getRanges :: [Char] -> (Int, Int, Int, Int)
getRanges line =
  let (rangeA, rangeB) = break (== ',') line
      (a, b) = bimap read (read . drop 1) $ break (== '-') rangeA
      (x, y) = bimap read (read . drop 1) $ break (== '-') $ drop 1 rangeB
   in (a, b, x, y)

part1 contents =
  lines contents
    & map
      ( \line ->
          let (a, b, x, y) = getRanges line
           in if a >= x && b <= y || x >= a && y <= b then 1 else 0
      )
    & sum
    & show

part2 contents =
  lines contents
    & map
      ( \line ->
          let (a, b, x, y) = getRanges line
           in if b >= x && a <= y then 1 else 0
      )
    & sum
    & show

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents
  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
