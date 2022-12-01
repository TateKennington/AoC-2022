import Data.Function
import Data.List
import System.IO

group' [] = []
group' lines =
  let (block, rest) = break null lines in block : group' (drop 1 rest)

part1 contents =
  lines contents
    & group'
    & map (sum . map read)
    & maximum
    & show

part2 contents =
  lines contents
    & group'
    & map (sum . map read)
    & sortBy (flip compare)
    & take 3
    & sum
    & show

main = do
  small_contents <- readFile "in.small"
  contents <- readFile "in"

  putStrLn $ part1 small_contents
  putStrLn $ part1 contents

  putStrLn $ part2 small_contents
  putStrLn $ part2 contents
